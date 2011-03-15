(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))
;(declaim (optimize (safety 0) (debug 0) (speed 3) (space 2) (compilation-speed 0)))
(declaim (sb-ext:muffle-conditions style-warning))

(require 'sb-sprof)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:restrict-compiler-policy 'debug 3)  ;  http://stackoverflow.com/questions/4638710/maximum-debug-info-from-steel-bank-common-lisp-slime/4638720#4638720
)

(load "transport")
(load "transport-types")

(load "data/request-opcodes.lisp")
(load "data/request-specs.lisp")
(load "data/reply-specs.lisp")
(load "data/event-specs.lisp")

(load "main")

(load "packages")
(load "zen")
(load "fonts")
(load "input")
(load "debug/info")

(defvar *zenmain-thread* nil)
(defvar *w* nil)  
(defvar *init-complete* nil)
(defvar *glx-window-exists* nil)

(defun step1 ()
  (setf *main-thread* (bt:make-thread #'main-thread :name 'zen-main-thread))
  (setf *transport-thread* (bt:make-thread #'transport-thread :name 'zen-transport-thread)))

(defun step2 ()
  (while (not *init-complete*)
    )
  (setf *zenmain-thread* (bt:make-thread #'main :name 'zenmain-thread))
  (while (not *glx-window-exists*)
    )
  (setf *input-thread* (bt:make-thread #'input-thread :name 'input-thread)))

(defun start ()
  (transport-init)
  (step1)
  (step2))

(defun rst ()
  (mapc (compose #'socket-close
		 #'client-socket)
	*clients*)
  (setf *clients* nil)
  (when (bt:thread-alive-p *transport-thread*)
    (bt:destroy-thread *transport-thread*))
  (when (bt:thread-alive-p *main-thread*)
    (bt:destroy-thread *main-thread*))
  (when (bt:thread-alive-p *input-thread*)
    (bt:destroy-thread *input-thread*))
  (when (bt:thread-alive-p *zenmain-thread*)
    (bt:destroy-thread *zenmain-thread*))
  (setf *init-complete* nil
	*glx-window-exists* nil)
  (sleep 1)
;;  (reset-server)
  (step1)
  (step2))
