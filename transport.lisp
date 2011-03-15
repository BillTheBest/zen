;; (C) 2011 Pierre-Yves Baccou

; SUGGESTIONS : generate request-specs.lisp and reply-specs.lisp from the xml X11 protocol definition that apparently exists somewhere ?
;             : replace elt with faster svref throughout. 

;; TODO : interpret replies, etc build request/reply function table at compile time, if possible (compile a constant containing the table of closures ?)

; FIXME : reinstate length checks (with condition raised by subseq when out of bounds)
; TODO : (setf (gethash ',name *request-translators*) #'(lambda (seq endian) ...))

; TODO : replace decode-** fns with an assoc table
; ((enum . `(lambda (...) ..)
;  (or   . `(lambda (...) ..) etc

;; Vague Idea : simplify further by operating on streams instead of arrays ?

;; FIXME : make sure my changes in find-clients are sane (ie event-mask is a list of keywords, not a bitfield !)

;; idee : macro d'optimisation qui transforme automatiquement une liste en vecteur de bytes
;; (with-vectors (var1 var2) body)
;; qui fait toutes les mods necessaires et possibles.


(require "deps" "deps")
(load "data/request-opcodes.lisp")
(load "debug/debug")

;; Idea to document special variables : tell what functions they are used in.
(defvar *encoders* nil)
(defvar *decoders* nil)
(defvar *encoder-generators* nil)
(defvar *decoder-generators* nil)

(defvar *request-translators* nil)
(defvar *reply/event-translators* nil)

;; The decoder uses those 3 global variables
(defvar *endian*)
(defvar *array*)
(defvar *position*)

;; arg is a varname or value.
(defmacro ENCODE (action endian arg &optional default-length)
  (if (equal ''nil action)
      `(make-array ,default-length :initial-element ,arg)
      `(funcall (encoder ,action)
		,endian
		,arg)))

(defmacro DECODE (action endian array)
  `(funcall (decoder ,action)
	    ,endian
	    ,array))

(defun proto->encoder (proto)
  (let (commands args chunks)
    (dolist (item proto)
      (bind (length (action arg &key ((:as chunk-name))) &optional (default-value 18))
	  item
	(let ((chunk (or chunk-name
			 (gensym))))
	  (when (and arg
		     (atom arg))
	    (push arg args))
	  (push chunk chunks)
	  (push `(setf ,chunk
		       (ENCODE ',action
			       *endian*
			       ,(or arg default-value)
			       ,length)) ; default length
		commands))))
    (setf commands (nreverse commands)
	  chunks   (nreverse chunks))
    `(lambda (endian &key ,@args &allow-other-keys)  ; allow-other-keys to allow :sequence-number key even in :Connection reply
       (let (,@chunks)
	 (progn
	   ,@commands)
	 (concatenate '(vector (unsigned-byte 8) *)
		      ,@chunks)))))

;; TODO : if the request array is too short (not enough data), there will be a BOUNDING-INDICES-BAD-ERROR condition. Catch it somewhere and maybe close the connection ?
;; returns a list (:arg1 val1 :arg2 val2 ...)
(defun proto->decoder (proto)
  (let (commands vars temps)
    (dolist (item proto)
      (bind (length (action &optional var &key temp?) &optional __)
	  item
	(when var ; inelegant
	  (push var vars)
	  (when temp?
	    (push var temps)) 
	  (push `(setf ,var
		       (DECODE ',action
			       *endian*
			       (subseq *array*
				       *position*
				       ,(when length
					 `(+ *position*
					     ,length)))))
		commands))
	(when length
	  (push `(incf *position*
		       ,length)
		commands))))
    `(lambda (endian array)
       (let (,@vars)
	 (setf *position* 0
	       *endian* endian
	       *array* array)
	 (progn
	   ,@(nreverse commands))
	 (list ,@(mapcan #'(lambda (var)
			     `(,(make-keyword var) ,var))
			 (set-difference vars
					 temps)))))))

;; TODO : notice this is the same as defdecoder, but saved in *request-translators* instead of *decoders*. Unify !
(defmacro deftranslator/request (name &rest proto)
  `(push (cons ',name
	       ,(proto->decoder proto))
	 *request-translators*))

(defmacro deftranslator/event (name &rest proto)
  `(push (cons ',name
	       ,(proto->encoder proto))
	 *reply/event-translators*))
      
(defun fill-reply-length-field (name endian array)
  (bind (a b type base)
      (case name
	((:Connection :FailedConnection)
	 '(6 8 :CARD16 2))
	(t
	 '(4 8 :CARD32 8)))
    (setf (subseq array a b)
	  (funcall (encoder type)
		   endian
		   (- (/ (length array) 4)
		      base)))
    array))
;
(defmacro deftranslator/reply (name &rest proto)
  `(push (cons ',name
	       #'(lambda (endian &rest args) 
		   (fill-reply-length-field ,name
					    endian
					    (apply ,(proto->encoder proto)
						   endian
						   args))))
	 *reply/event-translators*))



;---------------------------------------------------------------------
;---------------------------------------------------------------------

;; Transport thread stuff
;; Using SBCL sockets here (convert to IOLib later, when I find a manual ?)

(define-constant +initial-inbuf-size+ #x4000) ; 16kB
(define-constant +server-base+ 6000)
(define-constant +server-port+ 3)
(define-constant +tcp-backlog+ 128)
(define-constant +min-request-length+ 4)
(define-constant +min-conn/init-length+ 12)

(defvar *srv-socket* nil)
(defvar *srv-socket-fd* nil)
(defvar *clients* nil)

(defvar *pipe-read-fd* nil)
(defvar *pipe-write-fd* nil)
(defvar *pipe-read-stream* nil)
(defvar *pipe-write-stream* nil)

(defvar *server-port* +server-port+)

(defstruct (client (:print-function print-client))  ; see also debug.lisp
  in-buffer 
  out-bufqueue
  outbuf-lock
  cur-out-buf
  in-fill-pointer
  out-fill-pointer
  address
  port
  socket
  fd
  endian
  initialized
  request-length
  (closedown-mode :destroy)
  processed-seqnum   ; latest partly or fully processed request from this client
  (seqnum 0)) ;; current sequence number on this connection. (one per connection !)

(defun print-client (c stream depth)
    (format stream "client ~A" (client-fd c))
)

; note on extensions : we assume, if request opcode > 128 (and therefore is an extension), that the second byte is the minor request opcode.
; I haven't met an extension that didn't work like this.
(defun retrieve-and-decode-request (c)
  (with-fields (in-buffer initialized request-length seqnum endian)
      (c client)
    (let* ((opcode (elt in-buffer
			0))
	   (ext-opcode (elt in-buffer
			    1))
	   (name (cond ((not initialized)
			:Connection)
		       ((< opcode 128) ; extension opcode ?
			(rest (assoc opcode
				     +request-opcodes+)))
		       (t
			(rest (assoc ext-opcode
				     (rest (assoc opcode
						  +ext-request-opcodes+)))))))
	   (args (funcall (rest (assoc name
				       *request-translators*))
			  endian
			  (subseq in-buffer
				  0
				  request-length))))
      (list* name    ; TODO : create a request struct
	     c
	     seqnum
	     nil ; suppress-replies
	     args))))     ; (:arg1 val1 :arg2 val2 ...)

;Quoting Stevens on send() : With a nonblocking TCP socket, if there is no room at all in the socket send buffer, we return immediately with an error of EWOULDBLOCK. If there is some room in the socket send buffer, the return value will be the number of bytes the kernel was able to copy into the buffer. (This is called a short count.)
; as this is a temporary sb-bsd-socket based implementation (final being either on a _solid_ iolib, or cffi sockets), I will ignore the EWOULBLOCK possibility and hope for the best.

(defun open-new-connection (srv-socket)
  (mvbind (socket address port)
      (socket-accept srv-socket)
    (when socket
      (setf (non-blocking-mode socket)
	    t)
      (format t "Opening new conn with fd ~A~%" (socket-file-descriptor socket))
      (make-client :address address
		   :out-bufqueue (make-queue)
		   :outbuf-lock (bt:make-lock)
		   :in-buffer (make-array +initial-inbuf-size+
					  :adjustable t
					  :element-type '(unsigned-byte 8))
		   :in-fill-pointer 0
		   :out-fill-pointer 0
		   :port port
		   :socket socket
		   :fd (socket-file-descriptor socket)))))

(define-constant +endian-codes+
  '((#o102 . :msb)
    (#o154 . :lsb)))

(defun get-endian! (c)
  "Update connection's endianness if available, and return it"
  (with-fields (endian in-fill-pointer in-buffer)
      (c client)
    (or endian
	(setf endian
	      (unless (= 0 in-fill-pointer)
		(rest (assoc (elt in-buffer
				  0)
			     +endian-codes+)))))))

(defun get-request-length! (c)
  "Update connection's current request length if available, and return it or nil"
  (macrolet ((val16_ (pos)
	       `(uint16 (subseq in-buffer
				,pos
				(+ ,pos 2))
			endian))
	     (val32_ (pos)
	       `(uint32 (subseq in-buffer
				,pos
				(+ ,pos 4))
			endian)))
    (with-fields (in-buffer in-fill-pointer initialized request-length)
	(c client)
      (or request-length
	  (let ((endian (get-endian! c)))
	    (when (>= in-fill-pointer (if initialized
					  +min-request-length+
					  +min-conn/init-length+))
	      (prog1
		  (setf request-length
			(if initialized 
			    (let ((length (* (val16_ 2)
					     4)))
			      (cond ((/= length 0) ; bigreq or not ?
				     length)
				    ((>= in-fill-pointer 8)
				     (val32_ 4)) 
				    (t
				     nil)))
			    (let ((n (val16_ 6))
				  (d (val16_ 8)))
			      (+ 12
				 n d
				 (pad n) (pad d)))))
		(when request-length
		  (adjust-array in-buffer (max +initial-inbuf-size+
					       request-length))))))))))

(defvar *resources* nil)

(defun kill-client (c)
  (format t "closing client connection ~A with fd ~A~%" c (client-fd c))
  (socket-close (client-socket c)) ; TODO : this could rather be part of gc finalization of (client-socket c)
  (setf *clients*
	(delete c
		*clients*))
  (dolist (resource *resources*)
    (when (is-class resource 'window)
      (setf (attr-event-masks (window-attr resource))
	    (delete c
		    (attr-event-masks (window-attr resource))
		    :key #'first))))
  (case (client-closedown-mode c)  ; TODO : other cases
    (:destroy (mapc #'free-resource
		    (remove c
			    *resources*
			    :key #'xresource-created-by
			    :test-not #'eql)))))

;; TODO : should this be a coroutine ?
(defun get-more-data! (c)
  "Receive+store more data in client connection c, and return whether c now has enough data for a request"
  (with-fields (in-buffer initialized request-length socket)
      (c client)
    (handler-case 
	(let ((reqsize (cond (request-length request-length)
			     (initialized    +min-request-length+)
			     (t              +min-conn/init-length+)))
	      (in-fill-pointer (client-in-fill-pointer c)))
	  (mvbind (data length)
	      (socket-receive socket
			      nil  ; buffer
			      (- reqsize
				 in-fill-pointer) 
			      :element-type '(unsigned-byte 8))
	    (cond ((null length)
		   nil) ;  interrupted system call socket-receive, retry later
		  ((= length 0)
		   (error 'socket-error))
		  (t
		   (replace in-buffer                          ; TODO : go native (cffi) instead of using socket-receive, so as to avoid those buffer copies.
			    (subseq data
				    0
				    length)
			    :start1 in-fill-pointer)
		   (eql (incf (client-in-fill-pointer c)  	    ; boolean return value for get-more-data!
			      length)
			(get-request-length! c))))))
      (socket-error (_)  ; either length 0 (orderly disconnection from client) or 'connection reset by peer' condition.
	(kill-client c)
	nil))))

;; this returns nil if there is data left to send. But this return value isn't used at the moment. 
(defun send-more-data! (c)
  (handler-case 
      (with-fields (socket out-fill-pointer out-bufqueue outbuf-lock cur-out-buf)
	  (c client)
	(when (and (null cur-out-buf)
		   (first out-bufqueue)) ; queue not empty
	  (bt:with-lock-held (outbuf-lock)
	    (setf cur-out-buf
		  (dequeue out-bufqueue))))
	(when cur-out-buf
	  (incf out-fill-pointer
		(socket-send socket
			     (subseq cur-out-buf
				     out-fill-pointer)
			     nil))
	  (when (= out-fill-pointer
		   (length cur-out-buf))
	    (setf out-fill-pointer 0
		  cur-out-buf nil))))
    (socket-error (_)  ; either length 0 (orderly disconnection from client) or 'connection reset by peer' condition.
      (kill-client c)
      nil)
    (bad-file-descriptor-error (_)
      (kill-client c)
      nil)))

;; TODO v2 : handle ENOTCONN or whatever condition is caused by a closed socket, by closing the connection and returning-from send.
;; todo v2 : to avoid retranslating same data for multiple clients, memoize this conversion. (with 2 memory slots only !)
(defun send (name c args)
  (with-fields (endian outbuf-lock out-bufqueue)
      (c client)
    (let ((data (apply (rest (assoc name
				    *reply/event-translators*))
		       endian
		       args)))
      (bt:with-lock-held (outbuf-lock)
	(enqueue data
		 out-bufqueue))
      (write-char #\a *pipe-write-stream*))))  ; ping transport thread with random data

(defun transport-init ()
  (setf *srv-socket*    (make-instance 'inet-socket :protocol :tcp :type :stream)
	*srv-socket-fd* (socket-file-descriptor *srv-socket*))
  (setf (non-blocking-mode *srv-socket*)
	t)
  (socket-bind *srv-socket*
	       #(127 0 0 1)
	       (+ +server-base+ *server-port*))
  (socket-listen *srv-socket*
		 +tcp-backlog+)
  (bind (fd-r fd-w)
      (posix-pipe)
    (setf *pipe-read-fd*      fd-r 
	  *pipe-write-fd*     fd-w
	  *pipe-read-stream*  (sb-sys:make-fd-stream fd-r :input  t :buffering :none)    ; :element-type 'unsigned-char useful ?
	  *pipe-write-stream* (sb-sys:make-fd-stream fd-w :output t :buffering :none)))) ; :element-type 'unsigned-char ?



