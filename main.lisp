;; (C) 2011 Pierre-Yves Baccou

;; not sure whether this file should stay or not (all main loops/threads together vs each in its domain file, those 2 exposition methods have advantages.
;; clear preference to keeping "main.lisp" right now
;; idea : no transport-init, everything generated in the defvars ?

;; See also sb-sys:add-fd-handler :
;;Arange to call FUNCTION whenever FD is usable. DIRECTION should be
;;  either :INPUT or :OUTPUT. The value returned should be passed to
;;  SYSTEM:REMOVE-FD-HANDLER when it is no longer needed.

(require "deps" "deps.lisp")

(defvar *clients* nil)

(defvar *srv-socket* nil)
(defvar *srv-socket-fd* nil)

(defvar *request-queue* (make-queue))
(defvar *reply/event-queue* (make-queue))

(defvar *request-lock* (bt:make-lock)) 
(defvar *reply/event-lock* (bt:make-lock))

(defvar *reply/event-condition* (bt:make-condition-variable))  
(defvar *request-condition* (bt:make-condition-variable))  

(defvar *main-thread* nil)
(defvar *transport-thread* nil)
(defvar *input-thread* nil)

(defvar *pipe-read-fd* nil)
(defvar *pipe-write-fd* nil)
(defvar *pipe-read-stream* nil)
(defvar *pipe-write-stream* nil)

(defvar *resources* nil)
(defvar *font-library* nil)

(defvar *init-complete* nil)

(defun main-thread ()
  (setf *resources* nil)
  (glx-init)
  (zen-init)
  (init-SW-cursor)
  (font-init)
  (setf *init-complete* t)
  (loop
     (apply #'request (bt:with-lock-held (*request-lock*)
			(while (null (car *request-queue*))
			  (bt:condition-wait *request-condition*
					     *request-lock*))
			(dequeue *request-queue*)))))

(defun transport-thread ()
  (let ((fdsets (list (posix-fdset) (posix-fdset) (posix-fdset))))  ; permanent storage for select 
    (loop
       (bind (readfds writefds exceptfds)
	   (posix-select :readfds (list* *srv-socket-fd*
					 *pipe-read-fd*
					 (mapcar #'client-fd
						 *clients*))
			 :writefds (mapcar #'client-fd
					   (remove-if-not #'(lambda (c)    ; remove empty queues
							      (or (client-cur-out-buf c)
								  (first (client-out-bufqueue c))))
							  *clients*))
			 :timeout? nil
			 :fdsets fdsets)
	 (cond ((member *pipe-read-fd*
			readfds)
		(while (read-char-no-hang *pipe-read-stream*)
		  ))
	       ((member *srv-socket-fd* readfds) 
		(awhen (open-new-connection *srv-socket*)
		  (push it *clients*)))
	       (t
		(dolist (c *clients*)
		  (with-fields (fd seqnum initialized in-fill-pointer request-length)
		      (c client)
		    (when (and (member fd
				       readfds)
			       (get-more-data! c))
		      (let ((request (retrieve-and-decode-request c)))
			(setf initialized     t
			      request-length  nil
			      in-fill-pointer 0
			      seqnum          (1+ seqnum))
			(bt:with-lock-held (*request-lock*)
			  (enqueue request
				   *request-queue*))
			(bt:condition-notify *request-condition*)))
		    (when (member fd
				  writefds)
		      (send-more-data! c))))))))))



#|
Architecture :

Main thread :
     consumes requests       (from *requests*)  ; obsolete name
     produces replies        (to **)  -> Transport
     produces errors and other events       (to **)-> Transport
     produces window hierarchy and grabs    (to **)-> Input (and more ?)
     -- NOT consuming input events

Input + cursor thread : 
     consumes messages from main (Windows mapped/unmapped/moved, keyboard focus window)
     [gets and processes input events, must know window positions, must know grabs]
     produces input events  -> Transport
     [moves HW cursor on screen]

Transport thread
     produces requests -> Main
     consumes replies/errors
     consumes input events
     consumes other events

-----

Various notes :

;; TODO : use GRAY STREAMS instead of (next-input-event) which would source bytes from the input devices.
;; A separate thread with the select() call would feed these Gray streams data.

;; put all socket stuff in one thread. But still need 2 sources of wakeup !
;; ideas :
;; 1) use one last fd to IPC from the main threads to this IO thread. (or have a pipe connecting the two threads)
;; 2) use a posix user signal to interrupt the select() call
;; in both cases, forget about condition variables.
;; consider usocket too
;; and do not forget that some this is obsolete, the right thing being STM.

;; todo : Looks like it can be simplified a lot (do not try to handle all active fds at once in the main loop ! wasting some posix-selects is no problem.)

; TODO review this architecture with role of send-more-data!, get-more-data! (or at least rename them). 

|#
