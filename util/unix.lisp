;; (C) 2011 Pierre-Yves Baccou

(provide "unix")

;; todo : use package cffi
(sb-ext:unlock-package 'sb-ext) ; allow redefining get-time-of-day

(require 'cffi)
(require "util" "util.lisp") ; really required ?

(cffi:define-foreign-library libc
    (:unix (:or "libc.so.6" "libc.so"))
  (t (:default "libc")))
(cffi:use-foreign-library libc)

; for select
(defconstant +fd-setsize+ 1024)

; for open
(defconstant +open-modes+
  '((o-accmode . #o003)
    (o-rdonly . #o0)
    (o-wronly . #o1)
    (o-rdwr . #o2)
    (o-creat . #o100)
    (o-excl . #o200)
    (o-noctty . #o400)
    (o-trunc . #o1000)
    (o-append . #o2000)
    (o-nonblock . #o4000)
    (o-ndelay . #o4000)  ; o-nonblock
    (o-sync . #o10000)
    (o-fsync . #o10000)  ; o-sync
    (o-async . #o20000)))

(defconstant +intr-errno+ 4) ; interrupted system call

(cffi:defctype size_t :int)
(cffi:defctype ssize_t :uint)

(cffi:defcfun ("select" %posix-select) :int
  (nfds :int)
  (readfds :pointer)
  (writefds :pointer)
  (exceptfds :pointer)
  (timeout :pointer))

(cffi:defcfun ("open" %posix-open) :int
  (pathname :pointer)
  (flags :int))

(cffi:defcfun ("close" %posix-close) :int
  (fd :int))

(cffi:defcfun ("strerror" %posix-strerror) :pointer
  (errnum :int))

(cffi:defcfun ("read" %posix-read) ssize_t
  (fd :int)
  (buf :pointer)
  (count size_t))

(cffi:defcfun ("poll" %posix-poll) :int
  (fds :pointer)
  (nfds :uint)
  (timeout :pointer))

(cffi:defcfun ("gettimeofday" %get-time-of-day) :int
  (tv-sec :pointer)
  (tv-usec :pointer))
 
(defparameter nullp (cffi:null-pointer))

(cffi:defcstruct posix-timeval
  (sec :int)
  (usec :int))

(defun fill-fdset (fdset fds)
  (dotimes (i (/ +fd-setsize+ 32))
    (setf (cffi:mem-aref fdset :uint32 i)
	  0))
  (dolist (fd fds)
    (let ((fdelt (floor fd 8))
	  (fdmask (ash 1
		       (mod fd 8))))
      (setf (cffi:mem-aref fdset :uint8 fdelt)
	    (logior fdmask 
		    (cffi:mem-aref fdset :uint8 fdelt)))))
  fdset)

;  List all enabled fds in a fdset.
(defun find-fds (fdset &optional (max +fd-setsize+))
  (declare (fixnum max))
  (unless (cffi:null-pointer-p fdset)
    (let (fds)
      (dotimes (i (/ +fd-setsize+
		     8))
	(when (<= max 0) (return))
	(let ((byte (cffi:mem-aref fdset :uint8 i)))
	  (dotimes (j 8)
	    (when (logbitp j byte)
	      (push (+ j (* i 8))
		    fds)
	      (decf max)))))
      (reverse fds))))

(defun posix-errno ()
  (cffi:mem-aref (cffi:foreign-symbol-pointer "errno") :int))

(defun posix-strerror (n)
  (cffi:foreign-string-to-lisp
   (%posix-strerror n)))

(defun posix-fdset ()
  (cffi:foreign-alloc :uint8 :initial-element 0 :count (/ +fd-setsize+ 8)))

(defun posix-select (&key
		     (readfds nil) (writefds nil) (exceptfds nil)
		     (sec 0) (usec 0)
		     (timeout? t) interruptible?
		     (fdsets (list (posix-fdset) (posix-fdset) (posix-fdset))))
  (let ((nfds (if (or readfds writefds exceptfds)
		  (1+ (apply #'max
			     (append readfds writefds exceptfds)))
		  1))
	(r (if readfds (fill-fdset (first fdsets) readfds) nullp))
	(w (if writefds (fill-fdset (second fdsets) writefds) nullp))
	(e (if exceptfds (fill-fdset (third fdsets) exceptfds) nullp)))
    (cffi:with-foreign-object (timeout 'posix-timeval)
      (setf (cffi:foreign-slot-value timeout
				     'posix-timeval
				     'sec)
	    sec)
      (setf (cffi:foreign-slot-value timeout
				     'posix-timeval
				     'usec)
	    usec)
      (case (%posix-select nfds
			   r w e
			   (if timeout? timeout nullp))
	(-1 (when (/= +intr-errno+
		      (posix-errno))
	      (error (posix-strerror (posix-errno))))
	    (unless interruptible?
	      (posix-select :readfds readfds :writefds writefds :exceptfds exceptfds :sec sec :usec usec :timeout? timeout? :interruptible? interruptible? :fdsets fdsets))) ; watch out for TCO, otherwise a long-term memleak
	(0 nil)
	(t (mapcar #'find-fds
		   (list r w e)))))))

(defun posix-open (&key pathname flags)
  (let ((fla (apply #'logior
		    (mapcar (compose #'cdr (rcurry #'assoc +open-modes+))
			    flags)))
	(pth (cffi:foreign-string-alloc pathname)))
    (prog1
      (%posix-open pth fla)
      (cffi:foreign-string-free pth))))

(defun posix-close (&key fd)
  (%posix-close fd))

(defun posix-pipe ()
  (multiple-value-bind (fd1 fd2) (sb-posix:pipe)
    (values (list fd1 fd2)
	    (list (sb-sys:make-fd-stream fd1 :input  t :buffering :none :element-type 'unsigned-byte)
		  (sb-sys:make-fd-stream fd2 :output t :buffering :none :element-type 'unsigned-byte)))))

;; Work around bug
(let ((%buf (cffi:foreign-alloc :uint8 :initial-element 0 :count 1)))
;
(defun posix-read-char-no-hang (str)
  (let ((fd (sb-sys:fd-stream-fd str)))
    (when (member fd
		  (first (posix-select :readfds (list fd))))
      (let ((ret (%posix-read fd %buf 1)))
	(case ret
	  (1 t)
	  (t (error "Read returns /= 1 : ~A~%" ret)))))))
;
)

(defstruct timeval
  sec
  usec)

(defun get-time-of-day ()
  "Incomplete : ignoring timezone"
  (cffi:with-foreign-object (timeval 'posix-timeval)
    (case (%get-time-of-day timeval
			    nullp)
      (-1 (error (posix-strerror (posix-errno))))
      (0 (make-timeval :sec  (cffi:foreign-slot-value timeval
						      'posix-timeval
						      'sec)
		       :usec (cffi:foreign-slot-value timeval
						      'posix-timeval
						      'usec))))))

#|
/* The fd_set member is required to be an array of longs.  */
typedef long int __fd_mask;

typedef struct
  {
    __fd_mask __fds_bits[__FD_SETSIZE / __NFDBITS];
  } fd_set;

/* Number of descriptors that can fit in an `fd_set'.  */
#define	__FD_SETSIZE		1024


# define __FDS_BITS(set) ((set)->__fds_bits)


/* We don't use `memset' because this would require a prototype and
   the array isn't too big.  */
# define __FD_ZERO(set)  \
  do {									      \
    unsigned int __i;							      \
    fd_set *__arr = (set);						      \
    for (__i = 0; __i < sizeof (fd_set) / sizeof (__fd_mask); ++__i)	      \
      __FDS_BITS (__arr)[__i] = 0;					      \
  } while (0)
# define __FD_SET(d, set)    (__FDS_BITS (set)[__FDELT (d)] |= __FDMASK (d))
# define __FD_CLR(d, set)    (__FDS_BITS (set)[__FDELT (d)] &= ~__FDMASK (d))
# define __FD_ISSET(d, set)  (__FDS_BITS (set)[__FDELT (d)] & __FDMASK (d))

/* It's easier to assume 8-bit bytes than to get CHAR_BIT.  */
#define __NFDBITS	(8 * sizeof (__fd_mask))
#define	__FDELT(d)	((d) / __NFDBITS)
#define	__FDMASK(d)	((__fd_mask) 1 << ((d) % __NFDBITS))
|#