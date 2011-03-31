;; (C) 2011 Pierre-Yves Baccou

(sb-ext:unlock-package 'sb-ext) ; to allow (defstruct gc), get-time-of-day

(require "deps" "deps.lisp")

(when (find-package 'cl-opengl)
  (rename-package 'cl-opengl 'cl-opengl)) ; remove nickname "GL"
(require 'clx)
(rename-package (find-package 'xlib) "XLIB" '("X"))
(when (find-package 'gl)
  (delete-package 'gl)) ; do not use clx/gl package
(when (find-package 'cl-opengl)
  (rename-package 'cl-opengl 'cl-opengl '(gl))) ; restore nickname "GL", now that conflict is solved

(when (find-package 'glx)
  (delete-package 'glx))

(eval-when (:compile-toplevel :load-toplevel :execute)
;
  (load "packages")
;
)

(load "zengl")
(load "glx/myglx")
(load "glx/constants")
(load "data/request-opcodes")  ; only for the CopyArea one
(load "data/error-spec")
(load "data/colors")
(load "region")

(rename-package 'myglx 'myglx '(glx))

(define-constant +root-id+ #x0000013c) ; to match X.Org server

(define-constant +root-width+ 1000)
(define-constant +root-height+ 800)
(define-constant +root-x+ 10)
(define-constant +root-y+ 10)

(define-constant +default-colormap+ 214)
(define-constant +visual-24+ #x21) 
;;(define-constant +visual-32+ #x23)

(defvar *screen* nil)
(defvar *saved-read-target* nil)
(defvar *saved-draw-target* nil)

;; --------------------------------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------------------------------
;; Conditions, boilerplate

;; X errors : signal conditions, send an error (and also whatever events were queued before the error occured, but :
;; note : see protocol book p 353. When a request terminates with an error, there is no partial execution, unless it was a request such as ChangeWindowAttributes.
;; In this case all the operations up to the one containing the error will be performed.

(define-condition internal-error (simple-error)
  ((str :initarg :str :reader internal-error-str)))

(define-condition xcondition (simple-condition)
  ((args :initarg :args :reader xcondition-args)))
(define-condition xreply (xcondition)
  ())
(define-condition xerror (xcondition simple-error)
  ((type   :initarg :type :reader xerror-type)))
(define-condition xevent (xcondition)
  ((name   :initarg :name)
   (client :initarg :client)))

;; --------------------------------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------------------------------
;; Requests => internal commands 

;; TODO: these are (almost) templates that will need to be expanded (esp. "request")
;; (NB : what did I mean here ?)

(defvar *requests* nil)
(defvar *client* nil)
(defvar *clients* nil)

(defvar *focus-window* :PointerRoot)
(defvar *focus-revert-to* :PointerRoot)
(defvar *last-focus-change-time* 0)

(defvar *button-state* '((:Button1 nil) (:Button2 nil) (:Button3 nil) (:Button4 nil) (:Button5 nil)))
(defvar *cursor-exact-coords* '(0.0 0.0))
(defvar *cursor-window* nil)

(defvar *request-queue* (make-queue))
(defvar *reply/event-queue* (make-queue))

(defmethod is-class (arg type)
  nil)

(defmacro defclass- (name parents &rest args)
  `(progn
     (defclass/struct ,name ,parents ,@args)
     (defmethod is-class ((arg ,name) (type (eql ',name)))
       t)))

(defun request (name client seqnum suppress-replies &rest args)
  (apply (rest (assoc name
		      *requests*))
	 client
	 seqnum
	 suppress-replies
	 args))

;; Careful when using this ! as it sets *client* to nil (bad)
(defun int-request (name suppress-replies &rest args)
  "internal request (execute from any thread)"
  (apply #'request
	 name nil nil suppress-replies args))

(defun enqueue-request (name suppress-replies &rest args)
  "enqueue request for execution in main thread"
  (bt:with-lock-held (*request-lock*)
    (enqueue (list* name nil nil suppress-replies
		    args)
	     *request-queue*))
  (bt:condition-notify *request-condition*))

(defmacro with-resources-and-atoms (xargs &body body)
  "Extract values from resource and atom ids, save result in same variable var."
  `(progn
     ,@(mapcan #'(lambda (xarg)
		   (bind (sym class &optional (alt-vals nil alt-vals?))
		       xarg
		     (if (eql class 'xatom)
			 `((setf ,sym (or (get-atom ,sym)
					  ,sym))
			   (unless (or (stringp ,sym)
				       ,(when alt-vals? `(member ,sym
								 ',alt-vals)))
			     (xerror :XAtom ,sym)))
			 `((setf ,sym (or (find-res ,sym)
					  ,sym))
			   (unless (or (is-class ,sym ',class)
				       ,(when alt-vals?  `(member ,sym
								 ',alt-vals)))
			     (xerror ,(make-keyword class)
				     ,sym))))))
	       xargs)
     ,@body))

(defun reconvert (arg)
  (when arg
    (if (atom arg)
	(xresource-id arg)
	(mapcar #'reconvert
		arg))))

;; TODO : can be shortened A LOT. Treat all xconditions (error, event, reply) the same way, as much as possible.
;; todo : create request struct. 3 members : function, arglist, hooks. Then *requests* will be a fixed-size hashtable of such structs

(defmacro defrequest (request xarglist &body body)
  (let ((arglist (mapcar #'carat
			 xarglist))
	(resources/atoms (intersection xarglist
				       '(gc window drawable pixmap picture cursor colormap font xatom)
				       :test #'(lambda (xarg item)
						 (eql (second (mklist xarg))
						      item)))))
    (with-gensyms (gevents greply gerror)
      `(let ((fun
	      #'(lambda (client seqnum suppress-replies &key ,@arglist)
		  (when client
		    (setf *client* client))
		  (if seqnum
		      (setf (client-processed-seqnum client)
			    seqnum)
		      (setf seqnum 
			    (when client
			      (client-processed-seqnum client))))
		  (let (,gevents ,greply ,gerror)
		    (block request-block
		      (handler-bind
			  ;; TODO : another condition whereby a connection closed whilst executing the request.
			  ((xerror #'(lambda (xerror)
				       (setf ,gerror
					     xerror)
				       (return-from request-block)))
			   (xreply #'(lambda (xreply)
				       (setf ,greply
					     xreply)))
			   (xevent #'(lambda (xevent)
				       (push xevent
					     ,gevents))))
			(with-resources-and-atoms (,@resources/atoms)
			  ,@body)))
		    (unless suppress-replies
		      (dolist (xevent (nreverse ,gevents)) ; fixme : at the moment, in a nested request, events will always be sent before those of the parent request.
			(with-slots (name client args)
			    xevent
			  (send name  ; this is repeated 3 times, think of factoring, within a (send) equivalent local to zen.lisp
				client
				(list* :sequence-number (client-processed-seqnum client)
				       (reconvert args)))))
		      (acond (,greply (send ',request
					    *client*
					    (list* :sequence-number seqnum
						   (reconvert (xcondition-args it)))))
			     (,gerror (send :Error
					    *client*
					    (list* :major-opcode (major-opcode ',request)
						   :minor-opcode (minor-opcode ',request)
						   :code (first (rassoc (xerror-type ,gerror) ; todo: evaluate from (first... ) at compile time ?
									+error-codes+))
						   :sequence-number seqnum
						   (reconvert (xcondition-args it)))))))))))   ; this step may not be needed for errors
	 (push (cons ',request
		     fun)
	       *requests*)))))

(defun reply (&rest args)
  (signal 'xreply :args args))

(defun xerror (type &optional (resource 0))
  (error 'xerror :type type :args (list :resource resource)))

;; ---

;; TODO : try to remove this (merge with find-input-window, for instance)
(defun first-window-selecting-any (events window &optional chain) ; downward chain builds up as we recurse
  (when window
    (let ((selected-events (intersection events
					 (remove-duplicates (mapcan #'rest
								    (attr-event-masks (window-attr window)))))))
      (if (some #'(lambda (event)
		    (notany #'(lambda (win)
				(member event
					(attr-do-not-propagate-mask (window-attr win))))
			    chain))
		selected-events)
	  window
	  (first-window-selecting-any events
				      (window-parent window)
				      (cons window
					    chain))))))

;; TODO : make it work with :msb clients ; byte-swapping for ClientMessage (complicated)
(defrequest :SendEvent ((destination window (:PointerWindow :InputFocus)) propagate event-mask event)
  (setf event
	(second event))    ; event is originally (:lsb data)
  (unless (= 32 (length event)) (error "Bad SendEvent event length !~%"))
  (setf (elt event 0)
	(logior #x80
		(elt event 0)))
  (let* ((real-destination (case destination
			     (:PointerWindow *cursor-window*)
			     (:InputFocus (let ((focus (focus-window)))
					    (if (member focus
							(all-parents *cursor-window*))
						*cursor-window*
						focus)))
			     (t destination)))
	 (clients (cond ((null event-mask)
			 (list (xresource-created-by real-destination)))
			((null propagate) (find-clients event-mask
							real-destination))
			(t (or (find-clients event-mask
					     real-destination)
			       (progn
				 (setf real-destination (first-window-selecting-any event-mask
										    real-destination))
				 (unless (and (eql destination
						   :InputFocus)
					      (member (focus-window)
						      (all-parents real-destination)))
				   (find-clients event-mask
						 real-destination))))))))
;; uninteresting hack : manually reproduce a send (see transport.lisp)
    (dolist (client clients)
      (let ((data (replace (copy-seq event)
			   (encode :CARD16 :lsb (client-processed-seqnum client))
			   :start1 2)))
	(bt:with-lock-held ((client-outbuf-lock client))
	  (enqueue data
		   (client-out-bufqueue client)))
	(write-char #\a *pipe-write-stream*)))))

;; The functions below returns the clients receiving the event, and perhaps some per-client arguments. (source window, event window...)
(defun find-clients (events window)
  "List clients that selected the event for this window. (Only used for some events)"
  (remove-if #'(lambda (client)
		 (not (intersection events
				    (assoc client
					   (attr-event-masks (window-attr window))))))
	     *clients*))

(defun find-input-window (window event masks)
  "Find first window, up the chain from 'window' , where event is selected by some client. Returns clients and window"
  (when window
    (with-slots (parent attr)
	window
      (let ((selected? (find-clients masks
				     window))
	    (propagate? (not (member event
				     (attr-do-not-propagate-mask attr)))))
	(let ((input-window (cond (selected? window)
				  (propagate? (find-input-window parent
								 event
								 masks))
				  (t nil)))
	      (focus (focus-window)))
	  (if (and input-window
		   (member event
			   '(:KeyPress :KeyRelease))
		   (not (member focus
				(all-parents input-window))))
	      focus
	      input-window))))))
	
(defvar *old-parent* nil)

;; return ((client1 eventwin1) (client2 eventwin2) ...)
(defun find-structure-clients-and-windows (window &key old-parent?)
  (nconc (mapcar (rcurry #'list
			 window)
		 (find-clients '(:StructureNotify)
			       window))
	 (mapcar (rcurry #'list
			 (window-parent window))
		 (find-clients '(:SubstructureNotify)
			       (window-parent window)))
	 (when old-parent?
	   (mapcar (rcurry #'list
			   *old-parent*)
		   (find-clients '(:SubstructureNotify)
				 *old-parent*)))))

(defun substructure-redirectee (window)
  (first (remove *client*
		 (find-clients '(:SubstructureRedirect) (window-parent window))))) ; should be a list of 0 or 1 elts

(defun resize-redirectee (window)
  (first (remove *client*
		 (find-clients '(:ResizeRedirect) (window-parent window))))) ; idem

;; all keywords, some with default values (eg window = (window window) keyword !).
;; allowed boolean options :structure-event (StructureNotify etc), :input-event (use "propagate")

(defvar *event-fns* nil)

(eval-when (:compile-toplevel :load-toplevel :execute) ; normally (eval-when (:compile-toplevel). also :execute for testing purposes
  (defvar *event-xarglists* nil))

(defun active-input-masks (button-state)
  (let ((active-input-masks '(:PointerMotion :KeyPress :KeyRelease :ButtonPress :ButtonRelease :EnterWindow :LeaveWindow)))
    (when (find t
		button-state
		:key #'second)
      (push :ButtonMotion active-input-masks))
    (dotimes (i 5)
      (when (second (elt button-state i))
	(push (elt '(:Button1Motion :Button2Motion :Button3Motion :Button4Motion :Button5Motion)
		   i)
	      active-input-masks)))
    active-input-masks))

;; explain in doc by expanding an example or 2
(defmacro defevent (name xarglist &key (mode :event-mask) event-mask custom-clients masks)
  (eval-when (:compile-toplevel :load-toplevel :execute)  ; normally (eval-when (:compile-toplevel). also execute for testing purposes
    (push (cons name
		(remove 'eventwin
			xarglist))
	  *event-xarglists*))
  (let ((final-args (mapcan #'(lambda (xarg)
				(bind (arg &optional default)
				    (mklist xarg)
				  `(,(make-keyword arg) (or ,arg ,default))))
			    xarglist)))
    `(push (cons ,name
		 #'(lambda (&key ,@(mapcar #'carat
					   xarglist))
		     (let ((clientinfos ,(case mode
					   (:structure '(find-structure-clients-and-windows window))
					   (:structure-reparent `(find-structure-clients-and-windows window
												     :old-parent? t))
					   (:input `(when eventwin
						      (find-clients (intersection ',masks
										  (active-input-masks *button-state*))
								    eventwin)))
					   (:redirect '(list (substructure-redirectee window)))
					   (:resize '(list (resize-redirectee window)))
					   (:reply '(list *client*))
					   (:custom custom-clients)
					   (:event-mask `(find-clients (list ,event-mask)
								       window)))))  ; 'normal' events
		       (dolist (clientinfo clientinfos)
			 (bind (client &optional eventwin)
			     (mklist clientinfo)
			   (signal 'xevent
				   :name ,name
				   :client client
				   :args (list ,@final-args)))))))
	   *event-fns*)))

;; This macro inserts all implicit args (= args absent from (event...) call), either with nil if there is a default in defevent, as in "(event :SomeEvent :parent nil)", or arg itself as in "(event :SomeEvent :x x)"
(defmacro event (name &rest args)
  (let* ((grouped-args (group args 2))
	 (xarglist (rest (assoc name
				*event-xarglists*)))
	 (bindings (mapcar #'(lambda (argname)
			       (acond ((find (make-keyword argname)  ; arg given in (event... ) command ?
					     grouped-args
					     :key #'first)
				       (list argname (second it)))
				      ((second (assoc argname  ; find the default
						    (mapcar #'mklist
							    xarglist)))
				       (list argname it))
				      (t nil)))
			   (mapcar #'carat
				   xarglist))))
    `(let* (,@(remove nil bindings))
       (funcall (rest (assoc ,name
			     *event-fns*))
		,@(mapcan #'(lambda (argname)
			      (list (make-keyword argname) argname))
			 (mapcar #'carat
				 xarglist))))))

			
;; --------------------------------------------------------------------------------------------------------------
;; 9) Events

;; FIXME : not well designed, I can't tell what event-coords is for ?
;; fix this by using the source argument, just used for construction of the others (root, eventwin, child)

;; eventwin, child, state, detail, root-x, root-y are given by the input code. The other arguments are computed here.
;; Implicit arg 'window' is set by the input code to be the source window of the event.
;; IMPLICIT args eventwin (as below) and child, also computed in event-func.

;; TODO : allow focus window, grabs
(defmacro definputevent (name (&rest other-args) &rest masks)
  `(defevent ,name (source detail 
		    (eventwin (find-input-window source ',name (intersection ',masks
									     (active-input-masks *button-state*))))
		    (root-x (floor (first *cursor-exact-coords*))) (root-y (floor (second *cursor-exact-coords*)))
		    (state (butmod-state))
		    (root (screen-root (drawable-screen source)))
		    (same-screen t)
		    (event-coords (if (null eventwin)   ; used just for construction
				      '(0 0)
				      (mapcar #'-  
					      (cursor-coords)
					      (screen-coordinates eventwin))))
		    (event-x (first  event-coords))
		    (event-y (second event-coords))
		    (time (server-time))
		    ,@other-args)
     :mode :input :masks ,masks))

(definputevent :ButtonPress () :ButtonPress)
(definputevent :ButtonRelease () :ButtonRelease)
(definputevent :KeyPress () :KeyPress)
(definputevent :KeyRelease () :KeyRelease)
(definputevent :MotionNotify () :PointerMotion  :ButtonMotion :Button1Motion :Button2Motion :Button3Motion :Button4Motion :Button5Motion)

;; Note 'virtual window crossing' for Enter/LeaveNotify and FocusIn/Out is not implemented.
(definputevent :EnterNotify ((focus (member (focus-window)
					    (all-parents eventwin)))
			     (mode :normal)
			     (child (when eventwin
				      (find source
					    (window-subwindows eventwin))))
			     (same-screen/focus (cond ((and focus same-screen) '(:focus :same-screen))
						      (focus '(:focus))
						      (same-screen '(:same-screen))
						      (t nil))))
  :EnterWindow)
(definputevent :LeaveNotify ((focus (member (focus-window)
					    (all-parents eventwin)))
			     (mode :normal)
			     (child (when eventwin
				      (find source
					    (window-subwindows eventwin))))
			     (same-screen/focus (cond ((and focus same-screen) '(:focus :same-screen))
						      (focus '(:focus))
						      (same-screen '(:same-screen))
						      (t nil))))
  :LeaveWindow)

(defevent :CirculateNotify (eventwin window place)
  :mode :structure)

(defevent :ConfigureNotify
    (eventwin window x y width height border-width override-redirect above-sibling)
  :mode :structure)

(defevent :CreateNotify
    (window parent x y width height border-width override-redirect)
  :mode :custom :custom-clients (find-clients '(:SubstructureRedirect) (window-parent window)))
;;  :mode :structure) ; special case, hope this works  ; update : it wasn't working

(defevent :DestroyNotify (eventwin window)
  :mode :structure)

(defevent :MapNotify (eventwin window (override-redirect (attr-override-redirect (window-attr window))))
  :mode :structure)

(defevent :UnmapNotify (eventwin window from-configure)
  :mode :structure)

(defevent :MapRequest (window (parent (window-parent window)))
  :mode :redirect)

(defevent :ConfigureRequest (window (parent (window-parent window)) x y width height border-width sibling stack-mode value-mask)
  :mode :redirect)

(defevent :CirculateRequest (window (parent (window-parent window)) place)
  :mode :redirect)

(defevent :ResizeRequest (window width height)
  :mode :resize)

;; ClientMessage : Cannot be generated by the server ! But TODO : implement byte-swapping for it in SendEvent.

(defevent :ColormapNotify (window colormap new state)
  :event-mask :ColormapChange)

(defevent :Expose (window
		   (x 0) (y 0)
		   (width (drawable-width window)) (height (drawable-height window))
		   (count 0))
  :event-mask :Exposure)

(defevent :FocusOut (window (mode :Normal) detail)
  :event-mask :FocusChange)

(defevent :FocusIn (window (mode :Normal) detail)
  :event-mask :FocusChange)

(defevent :GraphicsExposure (drawable
			     (x 0) (y 0)
			     (width (drawable-width drawable))
			     (height (drawable-height drawable))
			     (count 0)
			     major-opcode (minor-opcode 0))
  :mode :reply)
(defevent :NoExposure (drawable major-opcode (minor-opcode 0))
  :mode :reply)

(defevent :GravityNotify (eventwin window x y)
  :mode :structure)

(defevent :Keymapnotify (window keys)  ; window arg not sent out
  :event-mask :KeymapState)

(defevent :MappingNotify (request (first-keycode 8) (count 248))
  :mode :custom :custom-clients *clients*)

(defevent :PropertyNotify (window atom (state :NewValue) (time (server-time)))
  :event-mask :PropertyChange)

(defevent :ReparentNotify (eventwin window (parent (window-parent window)) x y
			   (override-redirect (attr-override-redirect (window-attr window))))
  :mode :structure-reparent) ;; Warning : *old-parent* must exist

(defevent :VisibilityNotify (window state)
  :event-mask :VisibilityChange)


;; --------------------------------------------------------------------------------------------------------------
;; 2) Resources

;; TODO : use hashtable
;; Resource IDs are given by the clients. (but we give them a mask to help create resource IDs)

(defvar *resources* nil)

(defclass xresource ()
  ((id :initarg :id :accessor xresource-id)
   (created-by :initarg :created-by :accessor xresource-created-by)))

(defmethod xresource-id (obj)  ; for non-resources
  obj)

(defun find-res (id)
  (find id
	*resources*
	:key #'xresource-id))

;; TODO : which client was it created by ? (give created-by field to all xresources ?)
(defun create-res (id obj)
  (if (find id
	    *resources*
	    :key #'xresource-id)
      (xerror :IdChoice id)
      (progn (setf (xresource-id obj) id
		   (xresource-created-by obj) *client*)
	     (push obj
		   *resources*))))

(defun destroy-res (resource)
  (setf *resources*
	(delete resource
		*resources*)))
;;      (error "Destroying unknown resource"))) ; actually not always an error.

;; --------------------------------------------------------------------------------------------------------------
;; 3) Properties, atoms

;; may add reverse hash table for performance, later.
(define-constant +initial-atoms+ 1024)

(defvar *atoms* (make-array +initial-atoms+ :adjustable t))

(let ((%n 0))
;
(defun intern-atom (name &optional only-if-exists)
  (when (= %n
	   (length *atoms*))
    (setf *atoms*
	  (adjust-array *atoms*
			(* 2 %n))))
  (aif (position name
		 *atoms*
		 :test #'equal)
       (1+ it)
       (unless only-if-exists
	 (let ((atom (incf %n)))
	   (setf (aref *atoms*
		       (1- atom))
		 name)
	   atom))))
;
(defun is-atom (atom)
  (and (numberp atom)
       (/= atom 0)
       (<= atom %n)))
;
) ; n

(defun get-atom (atom)
  (when (is-atom atom)
    (aref *atoms*
	  (1- atom))))

(load "data/builtin-atoms.lisp")

(defrequest :InternAtom (name only-if-exists)
  (let ((interned (intern-atom name
			       only-if-exists)))
    (reply :atom (when interned
		   name))))

(defrequest :GetAtomName ((atom xatom))
  (reply :name atom))

;;--------------------------

(defstruct property
  name  ; atom
  format
  type
  (data #()))

;; consider this :
;;(defun val (property [window])
;;  (rest (assoc property window)))
;; as closure or normal flet function.

(defrequest :ChangeProperty ((window window) (name xatom) (type xatom) format mode data)
  (setf data
	(second data))  ; because data = (endian seq)
  (flet ((property-match (prop format type mode)
	   (or (and (eql format
			 (property-format prop))
		    (eql type
			 (property-type prop)))
	       (eql mode :Replace))))
    (event :PropertyNotify :atom name)
    (let ((prop (find name
		      (window-properties window)
		      :key #'property-name)))
      (cond ((and prop
		  (property-match prop format type mode))
	     (setf (property-format prop) format
		   (property-type   prop) type
		   (property-data   prop) (ecase mode
					    (:Replace data)
					    (:Prepend (concatenate 'vector
								   data
								   (property-data prop)))
					    (:Append (concatenate 'vector
								  (property-data prop)
								  data)))))
	    (prop
	     (xerror :Match))
	    (t
	     (push (make-property :name name :format format :type type :data data)
		   (window-properties window)))))))

(defrequest :DeleteProperty ((window window) (name xatom))
  (with-slots (properties)
      window
    (when-let ((property (find name
			       properties
			       :key #'property-name)))
      (setf properties
	    (delete property
		    properties))
      (event :PropertyNotify :atom name :state :Deleted))))

(defun encode-getproperty-value (value format endian)
  (if (eql :lsb endian)
      (coerce value 'vector)
      (ecase format
	(8 (coerce value
		   'vector))
	(16 (concatenate 'vector
			 (mapcar (compose (curry (encoder 'CARD16)
						 endian)
					  (curry (decoder 'CARD16)
						 :lsb)
					  (rcurry #'coerce 'vector))
				 (group value 2))))
	(32 (concatenate 'vector
			 (mapcar (compose (curry (encoder 'CARD32)
						 endian)
					  (curry (decoder 'CARD32)
						 :lsb)
					  (rcurry #'coerce 'vector))
				 (group value 4)))))))

;; value has to be encoded here rather than as usual, because encoding depends on format and my transport code doesn't support that
(defrequest :GetProperty ((window window) (name xatom) (type xatom (:AnyPropertyType))
			  long-offset long-length delete)
  (setf long-offset (* 4 long-offset)
	long-length (* 4 long-length))
  (if-let ((p (find name
		 (window-properties window)
		 :key #'property-name)))
    (with-fields (data (ptype type) (pformat format))
	(p property)
      (let* ((end (min (length data)
		       (+ long-offset
			  long-length)))
	     (bytes-after (- (length data)
			     end)))
	(if (not (member type
			 (list ptype :AnyPropertyType)))
	    (reply :type ptype
		   :format pformat
		   :bytes-after (length data))
	    (progn
	      (reply :type ptype
		     :format pformat
		     :bytes-after bytes-after
		     :value (encode-getproperty-value (subseq data
							      long-offset
							      end)
						      pformat
						      (client-endian *client*)))
	      (when (and delete
			 (= 0 bytes-after))
		(enqueue-request :DeleteProperty nil
				 :window window :name name))))))
    (reply :type nil
	   :format 0
	   :bytes-after 0)))

(defrequest :ListProperties ((window window))
  (reply :atoms (mapcar #'property-name
			(window-properties window))))

;; untested
(defrequest :RotateProperties ((window window) delta names)
  (let* ((names (mapcar #'get-atom
			names))
	 (properties (mapcar #'(lambda (name)
				 (find name
				       (window-properties window)
				       :key #'property-name))
			     names)))
    (when (member nil
		  names)
      (xerror :Xatom))
    (when (member nil
		  properties)
      (xerror :Match))
    (when (mod delta
	       (length properties))
      (mapc #'(lambda (name property)
		(setf (property-name property)
		      name)
		(event :PropertyNotify :atom name))
	    names
	    (rotate-list properties
			 delta)))))

;; --------------------------------------------------------------------------------------------------------------
;; 8) Pixmaps

(defclass- pixmap (drawable)
  framebuffer)

;; note this redundancy : there is a copy of width,height in the pixmap class and another in the framebuffer struct.
(defrequest :CreatePixmap (pid (drawable drawable) depth width height)
  (if (and (/= depth 1)
	   (/= depth 24))  ; TODO : more useful depths allowed for pixmaps
      (xerror :Value)
      (create-res pid
		  (make-instance 'pixmap
				 :screen (drawable-screen drawable)
				 :id pid
				 :depth depth :width width :height height
				 :framebuffer (zengl:create-framebuffer width height)))))
	
(defrequest :FreePixmap ((pixmap pixmap))
  ;; this stuff belongs in zengl....
  (switch-draw-target (screen-root *screen*))
  (switch-read-target (screen-root *screen*))
  (setf *saved-draw-target* nil)
  (setf *saved-read-target* nil)
  (gl:Delete-Textures (list (zengl::framebuffer-texture (pixmap-framebuffer pixmap))))
  (gl:Delete-Renderbuffers (list (zengl::framebuffer-depth-st-rb (pixmap-framebuffer pixmap))))
  (gl:Delete-Framebuffers (list (zengl::framebuffer-fbo (pixmap-framebuffer pixmap))))
  (destroy-res pixmap))

;; Put this back in CreateWindow body if it's the only place it's used ?
(defmethod copy-pixmap (obj)
  nil)
(defmethod copy-pixmap ((pixmap pixmap))
  (with-slots (width height depth screen framebuffer)
      pixmap
    (make-instance 'pixmap
		   :depth screen
		   :id nil  ;; no id,  it's a server internal pixmap
		   :framebuffer (zengl:create-framebuffer width height
							  :source framebuffer)
		   :width width :height height)))

;; --------------------------------------------------------------------------------------------------------------
;; 1) Windows

;; TODO : maybe put the defaults elsewhere ? (in window creation routine ?)
;; TODO : add references (ie 'cweb paragraph numbers') to where these are used in the code
;; TODO : only one client can select SubstructureRedirect

(defvar *installed-colormaps* nil)  ; this does almost nothing, remove it ?
(defvar *cursor-confine-window* nil)

(defstruct screen
  id
  root
  framebuffer
  glx-context) ; unused, only one screen

(defclass- attr ()
  (background-pixmap nil)  ; a pixmap, :ParentRelative or nil (aka :None)
  (background-pixel nil)
  (border-pixmap :CopyFromParent)
  (border-pixel nil)
  (bit-gravity :Forget)
  (win-gravity :NorthWest)
  (event-masks nil) ; one per client : list of the form ((client . (:event1 :event2 ...)) ...)
  (do-not-propagate-mask nil) ; one per window : (:event1 :event2 ...)
  (override-redirect nil)
  (colormap :CopyFromParent)
  (cursor nil)
  (backing-store :NotUseful) (backing-planes 0) (backing-pixel 0) save-under) ; unused

(defclass- drawable (xresource)
  screen
  depth
  width
  height)

(defclass- window (drawable)
  attr
  mapped
  viewable
  visualid
  x
  y
  border-width
  parent
  subwindows
  input-subwindows ; todo later : support InputOnly windows, keep them separate from normal ones like so
  properties
  created-by
  (class :InputOutput)) ; unused

;; Idea : (fsetf window-background-pixmap (compose #'attr-background-pixmap #'window-attr)), etc. Or get rid of struct attr and put everything in struct window.

;; footnote : no class InputOutput / InputOnly . InputOnly windows will not be considered as 'real' window and perhaps not even allowed.
;; update : they aren't implemented.

(defun siblings (window)
  (awhen (window-parent window)
    (window-subwindows it)))

(defun root-window (window)
  (screen-root (drawable-screen window)))

(defmethod prep-target ((window window))
  (apply #'hide-sw-cursor
	 (cursor-coords)))

(defmethod unprep-target ((window window))
  (apply #'draw-sw-cursor
	 (cursor-coords)))

(defmethod prep-target ((pixmap pixmap))
  )
(defmethod unprep-target ((pixmap pixmap))
  )

;; needed now but should logically be lower in the file
(defmacro with-read-target ((target) &body body)
  `(unwind-protect 
	(progn
	  (prep-target ,target)
	  (switch-read-target ,target)
	  ,@body)
     (unprep-target ,target)
     (zengl:flush)))

(defmacro with-draw-target ((target &key gc) &body body)
  `(unwind-protect 
	(progn
	  (prep-target ,target)
	  (when (switch-draw-target ,target :gc ,gc)
	    ,@body))
     (unprep-target ,target)
     (zengl:flush)))

;; excludes event-mask which is a special case
(define-constant +attributes+ '(background-pixel border-pixel bit-gravity win-gravity
				do-not-propagate-mask override-redirect colormap cursor backing-store save-under backing-planes backing-pixel))

(define-constant +attributes/get-attr+ (set-difference +attributes+
						       '(background-pixel border-pixel cursor)))

;; TODO : all the checks in CreateWindow p 138
;; window arg must be given when changing the attrs of an _existing_ window.
(defun event-selected-by-some-other-client (event attr)
  (some #'(lambda (client-mask)
	    (member event
		    (rest client-mask)))
	(remove *client*
		(attr-event-masks attr)
		:key #'first)))

;; This code closely follows the specification pp. 16-17
(defmacro change-attributes! (attr value-list &optional window)
  (let ((lambda-list (mapcar #'(lambda (attr)
				 (list attr nil (symb attr "?")))
			     +attributes+)))
    `(bind (&key ,@lambda-list (event-mask nil event-mask?) (background-pixmap nil background-pixmap?) (border-pixmap nil border-pixmap?))
	 ,value-list
       (when (or (and (member :SubstructureRedirect event-mask)
		      (event-selected-by-some-other-client :SubstructureRedirect ,attr))
		 (and (member :ResizeRedirect event-mask)
		      (event-selected-by-some-other-client :ResizeRedirect ,attr))
		 (and (member :ButtonPress event-mask)
		      (event-selected-by-some-other-client :ButtonPress ,attr)))
	 (xerror :Access))
       (when event-mask?
	 (setf (attr-event-masks ,attr)
	       (cons (cons *client*
			   event-mask)
		     (delete *client*
			     (attr-event-masks ,attr)
			     :key #'first))))
       (when (or border-pixmap? border-pixel?)
	 (setf (attr-border-pixmap ,attr) nil
	       (attr-border-pixel  ,attr) nil))
       (when (or background-pixmap? background-pixel?)
	 (setf (attr-background-pixmap ,attr) nil
	       (attr-background-pixel  ,attr) nil))
       (when-let ((pixmap (and background-pixmap?
			       (find-res background-pixmap))))
	 (if (eql 'pixmap
		  (type-of pixmap))
	     (setf (attr-background-pixmap ,attr)
		   pixmap)
	     (xerror :Pixmap)))
       (when-let ((pixmap (and border-pixmap?
			       (find-res border-pixmap))))
	 (if (eql 'pixmap
		  (type-of pixmap))
	     (setf (attr-border-pixmap ,attr)
		   pixmap)
	     (xerror :Pixmap)))
       ,@(mapcar #'(lambda (attribute)
		     `(when ,(symb attribute "?")
			(setf (slot-value ,attr ',attribute)
			      ,attribute)))
		 +attributes+)
       (when (and colormap? ,window)
	 (event :ColormapNotify
		:new t :window ,window :colormap colormap
		:state (if (member colormap *installed-colormaps*)
			   :Installed
			   :Uninstalled)))
       (when (and ,window
		  (or border-pixel? border-pixmap? background-pixel? background-pixmap?))
	 (enqueue-request :DrawBorder t :window ,window))
       ,attr)))

(defmacro get-attributes (attr)
  `(with-slots (,@+attributes/get-attr+ )
       ,attr
     (list ,@(mapcan #'(lambda (attr-name)
			 (list (make-keyword attr-name) attr-name))
		     +attributes/get-attr+))))

(defrequest :GetWindowAttributes ((window window))
  (let* ((attr (window-attr window))
	 (event-masks (attr-event-masks attr)))
    (apply #'reply
	   :map-state (cond ((window-viewable window) :Viewable)
			    ((window-mapped window) :Unviewable)
			    (t :Unmapped))
	   :class (window-class window)
	   :visual (window-visualid window)
	   :map-is-installed (find (attr-colormap attr)
				   *installed-colormaps*)
	   :your-event-mask (rest (assoc *client*
					 event-masks))
	   :all-event-masks (reduce #'union
				    (mapcar #'rest
					    event-masks)
				    :initial-value nil)
	   (get-attributes attr))))

(defrequest :ChangeWindowAttributes ((window window) value-list)
  (with-slots (attr)
      window
    (setf attr
	  (change-attributes! attr
			      value-list
			      window))))
	
;; attr is list of the form (:background-pixmap 123 :event-mask mask (etc))
(defrequest :CreateWindow (wid (parent window) class depth visual x y width height border-width value-list)
  (when (eql :CopyFromParent visual)
    (setf visual 
	  (window-visualid parent)))
  (unless (and (= +visual-24+ visual)
	       (member depth '(0 24) :test #'=))
    (xerror :Match))
  (let* ((%attr (make-instance 'attr))
	 (attr (change-attributes! %attr
				   value-list))
	 (window (make-instance 'window
				:id wid :attr attr :x x :y y :width width :height height :border-width border-width :parent parent
				:visualid visual
				:depth 24 
				:screen (drawable-screen parent)
				:created-by *client*)))
    (when (eql :CopyFromParent
	       (attr-colormap attr))
      (setf (attr-colormap (window-attr window))
	    (attr-colormap (window-attr parent))))
    (create-res wid window)
    (awhen (copy-pixmap (attr-background-pixmap (window-attr window)))
      (setf (attr-background-pixmap (window-attr window))
	    it))
    (push window      ; on top
	  (window-subwindows parent))
    (event :CreateNotify
	   :override-redirect (attr-override-redirect attr))))

(defrequest :DestroyWindow ((window window))
  (when-let ((parent (window-parent window)))
    (int-request :UnmapWindow nil :window window)
    (int-request :DestroySubwindows nil :window window)
    (destroy-res window)
    (setf (window-subwindows parent)
	  (delete window
		  (window-subwindows parent)))
    (event :DestroyNotify)))

(defrequest :DestroySubwindows ((window window))
  (dolist (child (window-subwindows window))
    (int-request :DestroyWindow nil :window child)))

;; This implementation is quite slow
(defun siblings-with-overlap (window)
  "All siblings whose border or interior is partly under or above window"
  (with-slots (x y width height parent (bw border-width))
      window
    (remove-if #'(lambda (sibling)
		   (with-slots ((x0 x) (y0 y) (width0 width) (height0 height) (bw0 border-width))
		       sibling
		     (or (eql sibling window)
			 (not (window-viewable sibling)) ; TODO : not need to use 'viewable?' here as we probably can assume that parent is mapped !
			 (> x0 (+ x  width   (* 2 bw)))
			 (> y0 (+ y  height  (* 2 bw)))
			 (> x  (+ x0 width0  (* 2 bw0)))
			 (> y  (+ y0 height0 (* 2 bw0))))))
	       (siblings window))))

;; could also use before, after (update : what did I mean here ?!)
(defun occluding-siblings (window)
  (when (window-parent window)
    (let* ((siblings (siblings window))
	   (superiors (subseq siblings
			      0
			      (position window
					siblings))))
      (intersection (siblings-with-overlap window)
		    superiors))))

(defun occluded-siblings (window)
  (when (window-parent window)
    (let* ((siblings (siblings window))
	   (inferiors (subseq siblings
			      (1+ (position window
					    siblings)))))
      (intersection (siblings-with-overlap window)
		    inferiors))))

(defun all-parents (window)
  "Includes the window itself"
  (when window
    (cons window
	  (all-parents (window-parent window)))))

;; Random note : Enter/leavenotify when mapped/unmapped/circulated/gravitated/configured is produced in update-cursor-window.
(defrequest :MapWindow ((window window))
  (labels ((expose-win-and-subwins (window)
	     (with-slots (viewable width height subwindows)
		 window
	       (setf viewable t)
	       (enqueue-request :DrawBorder t :window window)
	       (clear-area window 0 0 width height t) 
	       (event :VisibilityNotify :state :unobscured) ; v simplified visibility support !
	       (event :Expose)
	       (dolist (child (remove-if-not #'window-mapped
					     subwindows))
		   (expose-win-and-subwins child)))))
    (with-slots (mapped viewable attr)
	window
      (unless mapped
	(if (and (substructure-redirectee window)
		 (not (attr-override-redirect attr)))
	    (event :MapRequest)
	    (progn
	      (setf mapped t)
	      (event :MapNotify)
	      (expose-win-and-subwins window)
	      (when viewable
		(update-cursor-window))))))))

(defun coverage (window rects)
  "Zone (in window's coord system) in window covered by rects (in parent's coord system). Also return whether any of window is covered, including borders"
  (let (covered? coverage)
    (with-slots (x y width height (bw border-width))
	window
      (dolist (rect rects)
	(awhen (rect-intersection (list (- bw)
					(- bw)
					(+ width  bw)
					(+ height bw))
				  (bind (xr1 yr1 xr2 yr2)
				      rect
				    (list (- xr1 x bw)
					  (- yr1 y bw)
					  (- xr2 x bw)
					  (- yr2 y bw))))
	  (bind (x1 y1 x2 y2)
	      it
	    (setf covered? t)  ; at least one rect covered window. Useful to know whether to redraw border, even though coverage is nil
	    (setf x1 (max x1 0)
		  y1 (max y1 0)
		  x2 (min x2 width)
		  y2 (min y2 height))
	    (when (and (/= x1 x2)
		       (/= y1 y2))
	      (push (list x1 y1 x2 y2)
		    coverage)))))
      (list coverage
	    covered?))))

(defun update-window (window region &key exclude)
  "clear/expose areas in window, and its children (with exclusions), coinciding with region"
  (when (and region
	     (window-viewable window))
    (dolist (rect region)
      (bind (x1 y1 x2 y2)
	  rect
	(clear-area window
		    x1 y1
		    x2 y2
		    t)))
    (dolist (child (set-difference (window-subwindows window)
				   exclude))
      (bind (sub-region covered?)
	  (coverage child
		    region)
	(update-window child
		       sub-region)
	(when covered?
	  (enqueue-request :DrawBorder t :window child))))))

(defun make-unviewable (window)
  (with-slots (mapped viewable subwindows)
      window
    (when viewable
      (setf viewable nil)
      (mapc #'make-unviewable
	    subwindows))))

;; probably some code in common with restacking
(defrequest :UnmapWindow ((window window) from-configure (bw border-width))
  (with-slots (subwindows mapped viewable x y height width parent (bw border-width))
      window
    (when (and mapped
	       parent)
      (when viewable
	(let ((free-region (list (list x
				       y
				       (+ x width  (* 2 bw))
				       (+ y height (* 2 bw))))))
	  (update-window parent
			 free-region
			 :exclude (list window))
	  (update-cursor-window)
	  (make-unviewable window)))
      (setf mapped nil)
      (event :UnmapNotify :from-configure from-configure)
      (when (and (not (eql :PointerRoot
			   *focus-window*))
		 (member window
			 (all-parents *focus-window*)))
	(setf *focus-window*
	      (if (eql :Parent *focus-revert-to*)
		  parent
		  *focus-revert-to*))))))

(defrequest :MapSubwindows ((window window))
  (mapc #'(lambda (child)
	    (int-request :MapWindow nil :window child))
	(window-subwindows window)))

(defrequest :UnmapSubwindows ((window window))
  (mapc #'(lambda (child)
	    (int-request :MapWindow nil :window child))
	(reverse (window-subwindows window))))

(defrequest :GetGeometry ((drawable drawable))
  (get-geometry drawable))

(defmethod get-geometry ((window window))
  (with-slots (depth width height x y border-width screen)
      window
    (reply :root (xresource-id (screen-root screen))
	   :depth depth :width width :height height :x x :y y :border-width border-width)))

(defmethod get-geometry ((pixmap pixmap))
  (with-slots (depth width height screen)
      pixmap
    (reply :root (xresource-id (screen-root screen))
	   :depth depth :width width :height height
	   :x 0 :y 0 :border-width 0)))

;; TODO : store them in window instead of recomputing every time. Probably necessary to get decent speed in cursor routines
(defun screen-coordinates (window)
  "Coords of the window origin (inside the border) on the screen"
  (if (null window)
      '(0 0)
      (with-slots (x y border-width parent)
	  window
	(mapcar #'+
		(list (+ x border-width)
		      (+ y border-width))
		(screen-coordinates parent)))))

(defun screencoord-x (window)
  (first (screen-coordinates window)))

(defun screencoord-y (window)
  (second (screen-coordinates window)))

;; This is a bit approximative, but there it goes. (see the code)
(defmethod occluded? ((window window))
  "Is the window occluded by a sibling, or is any of its parents ?"
  (or (occluding-siblings window)
      (occluded? (window-parent window))))

(defmethod occluded? ((pixmap pixmap))
  nil)

(defmethod occluded? ((window (eql nil)))
  nil)

;; internal only
(defrequest :DrawBorder (window)
  (with-slots (x y width height (bw border-width) parent attr)
      window
    (unless (= 0 bw)
      (let ((2bw (* 2 bw)))
	(zengl:draw-framebuffer (screen-framebuffer (drawable-screen window)))
	(setf *saved-draw-target*
	      nil)
	(apply #'hide-sw-cursor
	       (cursor-coords))
	(clip-by-siblings window)
	(apply #'zengl:draw-origin (mapcar #'-
					   (screen-coordinates window)
					   (list bw bw)))
	(zengl:add-stencil :rect (list bw
				       bw
				       (+ bw width)
				       (+ bw height))) ; inside stencil
	(awhen (attr-border-pixel attr)
	  (zengl:rectangle 0
			   0
			   (+ 2bw width)
			   (+ 2bw height)
			   (pixel-color it)))
	;; v2 : implement tiled border ?
	(apply #'draw-sw-cursor
	       (cursor-coords))
	(zengl:flush)))))

;; useful for internal CopyArea requests. 
(defvar *internal-gc* nil)

(defun move-window! (window to-x to-y &optional to-bw)
  (with-slots (x y width height framebuffer parent (bw border-width))
      window
    (setf to-bw
	  (or to-bw
	      bw))
    (let* ((2bw (* 2 bw))
	   (2to-bw (* 2 to-bw))
	   (old-rect (list x
			   y
			   (+ x width 2bw)
			   (+ y height 2bw)))
	   (new-rect (list to-x
			   to-y
			   (+ to-x width 2to-bw)
			   (+ to-y height 2to-bw))))
      (enqueue-request :CopyArea nil
		       :src parent :dst parent
		       :gc *internal-gc*
		       :src-x (+ x bw) :src-y (+ y bw)
		       :width width :height height
		       :dst-x (+ to-x to-bw) :dst-y (+ to-y to-bw))
      (setf bw to-bw
	    x to-x
	    y to-y)
      (update-window parent
		     (region-difference (list old-rect)
					(list new-rect))
		     :exclude (list window))
      (enqueue-request :DrawBorder t :window window)
      (when (occluded? window)
	(event :Expose)))))  ; a shortcut, I should only Expose the areas that need it.

;; TODO : notice the code is pretty much the same as move-window!, and merge
(defun resize-window! (window to-width to-height)
  (if (resize-redirectee window)
      (event :ResizeRequest :width to-width :height to-height)
      (with-slots (x y width height (bw border-width) subwindows parent attr)
	  window
	(let* ((dw (- to-width  width))
	       (dh (- to-height height))
	       (2bw (* 2 bw))
	       (old-rect (list x
			       y
			       (+ x width  2bw)
			       (+ y height 2bw)))
	       (new-rect (list x
			       y
			       (+ x to-width  2bw)
			       (+ y to-height 2bw))))
	  (update-window parent
			 (region-difference (list old-rect)
					    (list new-rect))
			 :exclude (list window))
	  (setf width  to-width
		height to-height)
	  (enqueue-request :DrawBorder t :window window)
	  ;; mostly ignore bit-gravity
	  (if (eql :Forget
		   (attr-bit-gravity attr))
	      (progn
		(clear-area window 0 0 to-width to-height t)
		(event :Expose))
	      (progn
		(update-window window
			       (region-difference `((0 0 ,width ,height))
						  `((0 0 ,to-width ,to-height))))))
	  ;; move subwindows
	  (dolist (child subwindows)
	    (with-slots ((xc x) (yc y))
		child
	      (apply #'move-window!
		     child
		     (mapcar #'+
			     (list xc yc)
			     (case (attr-win-gravity (window-attr child))
			       (:NorthWest (list 0         0))
			       (:North     (list (/ dw 2)  0))
			       (:NorthEast (list dw        0))
			       (:West      (list 0         (/ dh 2)))
			       (:Center    (list (/ dw 2)  (/ dh 2)))
			       (:East      (list dw        (/ dh 2)))
			       (:SouthWest (list 0         dh))
			       (:South     (list (/ dw 2)  dh))
			       (:SouthEast (list dw        dh)))))))))))
  
;; used in circulate and restack
(defun stack/covered-areas (window)
  "List stack windows together with all covered areas in that window"
  (maplist #'(lambda (rev-stack)
	       (let* ((child (first rev-stack))
		      (region (coverage child
					(mapcar #'(lambda (sibling)
						    (with-slots (x y width height (bw border-width))
							sibling
						      (list x y (+ x width (* 2 bw)) (+ y height (* 2 bw)))))
						(rest rev-stack)))))
		 (cons child
		       region)))
	   (reverse (window-subwindows window))))

(defun process-coverage-diff (parent old-coverage-data new-coverage-data)
  (dolist (child (window-subwindows parent))
    (bind (old-region old-covered)
	(rest (assoc child
		     old-coverage-data))
    (bind (new-region new-covered)
	(rest (assoc child
		     new-coverage-data))
      (acond ((region-difference old-region
				 new-region)
	      (update-window child
			     it)
	      (enqueue-request :DrawBorder t :window child))
	     ((not (eql old-covered new-covered))  ; happens when childs border is now covered, but not the inside.
	      (enqueue-request :DrawBorder t :window child)))))))

(defun restack-window! (win sibling stack-mode)
  (let* ((parent (window-parent win))
	 (siblings (remove win
			   (siblings win))))
    (let ((ps (position sibling
			siblings))
	  (bot (1- (length siblings)))
	  (occluding-sibling? (member sibling
				      (occluding-siblings win)))
	  (sibling-occluded? (member sibling
				     (occluded-siblings win)))
	  (occluded? (occluding-siblings win))
	  (occluding? (occluding-siblings win)))
      (let ((q (if sibling
		   (case stack-mode
		     (:Above ps)
		     (:Below (1+ ps))
		     (:TopIf (if occluding-sibling? 0 nil))
		     (:BottomIf (if sibling-occluded? bot nil))
		     (:Opposite (cond (occluding-sibling? 0)
				      (sibling-occluded? bot)
				      (t nil))))
		   (case stack-mode
		     (:Above 0)
		     (:Below bot)
		     (:TopIf (if occluded? 0 nil))
		     (:BottomIf (if occluding? bot nil))
		     (:Opposite (cond (occluded? 0)
				      (occluding? bot)
				      (t nil)))))))
	(when q
	  (let (saved-coverage-data new-coverage-data)
	    (setf saved-coverage-data
		  (stack/covered-areas parent))
	    (setf (window-subwindows parent)
		  (ninsert win
			   (delete win
				   (window-subwindows parent))
			   q))
	    (setf new-coverage-data
		  (stack/covered-areas parent))
	    (process-coverage-diff parent
				   saved-coverage-data
				   new-coverage-data)
	    t))))))

(defrequest :CirculateWindow ((win window) direction)
  (with-slots (subwindows)
      win
    (let ((lowest-occluded-child (last (remove-if-not #'occluding-siblings
						      subwindows)))
	  (highest-occluding-child (first (remove-if-not #'occluded-siblings
							 subwindows)))
	  (bot (1- (length subwindows))))
      (bind (restacked-window pos place)
	  (ecase direction
	    (:RaiseLowest  `(,lowest-occluded-child   0   :Top))
	    (:LowerHighest `(,highest-occluding-child ,bot :Bottom)))
	(when restacked-window
	  (if (substructure-redirectee win)
	      (event :CirculateRequest
		     :window restacked-window
		     :parent win)
	      (let (saved-coverage-data new-coverage-data)
		(setf saved-coverage-data
		      (stack/covered-areas win))
		(setf subwindows
		      (ninsert restacked-window
			       (delete restacked-window
				       subwindows)
			       pos))
		(setf new-coverage-data
		      (stack/covered-areas win))
		(process-coverage-diff win
				       saved-coverage-data
				       new-coverage-data)
		(event :CirculateNotify 
		       :window restacked-window)
		(update-cursor-window))))))))

(defrequest :ConfigureWindow ((window window) value-list)
  (bind (&key x y width height border-width sibling stack-mode)
      value-list
    (setf sibling
	  (find-res sibling))
    (with-slots ((orig-x x) (orig-y y) (orig-width width) (orig-height height) (orig-border-width border-width) attr)
	window
      (if (and (substructure-redirectee window)
	       (not (attr-override-redirect attr)))
	  (event :ConfigureRequest
		 :x (or x orig-x) :y (or y orig-y)
		 :width (or width orig-width) :height (or height orig-height)
		 :border-width (or border-width orig-border-width)
		 :sibling sibling  ; can be nil
		 :stack-mode (or stack-mode :Above)
		 :value-mask (mapcar #'first
				     (delete nil
					     `((:x ,x) (:y ,y) (:width ,width) (:height ,height) (:border-width ,border-width) (:sibling ,sibling) (:stack-mode ,stack-mode))
					     :key #'second)))  ; list of args actually supplied to the 'request ConfigureWindow' call.
	  (progn
	    (let ((resize? (or width height))
		  (move? (or x y border-width))
		  (restack? stack-mode))
	      (setf x (or x orig-x)
		    y (or y orig-y)
		    width (or width orig-width)
		    height (or height orig-height))
	      (when resize?
		(resize-window! window width height))
	      (when move?
		(move-window! window x y border-width))
	      (when restack?
		(restack-window! window sibling stack-mode)))
	    (update-cursor-window)
	    (with-slots (x y width height border-width parent attr)
		window
	      (let ((siblings (siblings window)))
		(event :ConfigureNotify
		       :override-redirect (attr-override-redirect attr)
		       :above-sibling (let ((p (position window
							 siblings)))
					(unless (null (nthcdr (1+ p)
							      siblings)) ; bottom ?
					  (elt siblings
					       (1+ p))))))))))))

(defrequest :ReparentWindow ((window window) (new-parent window) new-x new-y)
  ;; todo : more error conditions possible
  (if (member window
	      (all-parents new-parent))
      (xerror :Match)
      (with-slots (mapped parent x y)
	  window
	(let ((was-mapped? mapped))
	  (when was-mapped?
	    (int-request :UnmapWindow nil :window window))
	  (setf (window-subwindows parent)
		(delete window
			(window-subwindows parent)))
	  (push window
		(window-subwindows new-parent))
	  (setf *old-parent*
		parent)
	  (setf parent new-parent
		x new-x
		y new-y)
	  (event :ReparentNotify)
	  (when was-mapped?
	    (int-request :MapWindow nil :window window))))))

(defrequest :QueryTree ((window window))
  (with-slots (screen parent subwindows)
      window
    (reply :root (screen-root screen)
	   :parent parent
	   :children (reverse subwindows))))

(defun focus-window ()
  (if (eql :PointerRoot
	   *focus-window*)
      (screen-root *screen*)
      *focus-window*))


;; --------------------------------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------------------------------
;; Input

;; we ignore all the subtleties of FocusIn/Out and just put detail=None.
(defrequest :SetInputFocus ((focus window (:PointerRoot nil)) revert-to time)
  (let ((current-time (server-time)))
    (unless (and (numberp time)
		 (or (< time *last-focus-change-time*)
		     (> time current-time)))
      (if (and (is-class focus
			 'window)
	       (not (window-viewable focus)))
	  (xerror :Match)
	  (progn
	    (setf *last-focus-change-time*
		  (if (eql :CurrentTime time)
		      current-time
		      time))
	    (event :FocusOut :window (focus-window) :detail nil)
	    (setf *focus-window*
		  focus)
	    (event :FocusIn :window (focus-window) :detail nil)
	    (setf *focus-revert-to*
		  revert-to))))))

(defrequest :GetInputFocus ()
  (reply :focus *focus-window*
	 :revert-to *focus-revert-to*))


;; --------------------------------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------------------------------
;; 6a) GC and drawing

(defclass- gc (xresource)
  root
  depth
  (foreground 0)
  (background 1)
  (subwindow-mode :ClipByChildren)
  (line-width 1.0)
  (graphics-exposures t)
  clip-mask ; unused
  (clip-x-origin 0)
  (clip-y-origin 0)
  clip-rectangles ; what I actually use for gc clipping
  (function :Copy)
  font ; FIXME : no default font ?
  line-style plane-mask cap-style join-style fill-style fill-rule tile stipple tile-stipple-x-origin tile-stipple-y-origin dash-offset dashes arc-mode) ; unused

(defrequest :SetClipRectangles ((gc gc) clip-x-origin clip-y-origin rectangles ordering)
  (setf (gc-clip-rectangles gc)
	(if (null rectangles)
	    :clip
	    (mapcar #'(lambda (rect)
			(bind (x y width height)
			    rect
			  (list (+ clip-x-origin x)
				(+ clip-y-origin y)
				(+ clip-x-origin x width)
				(+ clip-y-origin y height))))
		    rectangles))))

;; -----------------
;; Section : 
;; Drawing (don't forget stencil and blend (todo) options !)
;; and Tiling. (unimplemented)

(defun stencil-around-window (window)
  (apply #'zengl:draw-origin (screen-coordinates window))
  (zengl:add-stencil :rect (list 0 0
				 (drawable-width window) (drawable-height window))
		     :reverse t))

(defun clip-by-children (window)
  (apply #'zengl:draw-origin (screen-coordinates window))
  (dolist (child (remove-if-not #'window-mapped 
				(window-subwindows window)))
    (with-slots (x y width height border-width)
	child
      (let ((2bw (* 2 border-width)))
	(zengl:add-stencil :rect (list x y
				       (+ x 2bw width) (+ y 2bw height)))))))

(defun clip-by-siblings (window)
  (when-let ((parent (window-parent window)))
    (apply #'zengl:draw-origin
	   (screen-coordinates parent))
    (dolist (sibling (occluding-siblings window))
      (with-slots (x y width height border-width)
	  sibling
	(let ((2bw (* 2 border-width)))
	  (zengl:add-stencil :rect (list x y
					 (+ x 2bw width) (+ y 2bw height))))))
    (clip-by-siblings parent)))

;; TODO : if they're not used elsewhere, reintegrate the 3 fns above in prepare-target

;; returns nil if target not drawable to
(defmethod switch-draw-target ((window window) &key gc)
  (when (window-viewable window)
    (setf *saved-draw-target*
	  window)
    (zengl:draw-framebuffer (screen-framebuffer (drawable-screen window)))
    (stencil-around-window window)
    (clip-by-siblings window)
    (if (null gc)
	(progn
	  (clip-by-children window)
	  (apply #'zengl:draw-origin
		 (screen-coordinates window)))
	(with-slots (subwindow-mode clip-rectangles)
	    gc
	  (when (eql :ClipByChildren
		     subwindow-mode)
	    (clip-by-children window))
	  (apply #'zengl:draw-origin
		 (screen-coordinates window))
	  (case clip-rectangles
	    ((nil) ) ; nothing 
	    (:clip (zengl:add-stencil :rect (list 0 0
						  (drawable-width window) (drawable-height window))))   ; effectively disable output  
	    (t (zengl:add-stencil :region clip-rectangles
				  :reverse t)))))
    t))

(defmethod switch-draw-target ((pixmap pixmap) &key gc)
  (when (pixmap-framebuffer pixmap)
    (setf *saved-draw-target*
	  pixmap)
    (zengl:draw-framebuffer (pixmap-framebuffer pixmap))
    (zengl:draw-origin 0 0)
    (when gc
      (let ((clip-rectangles (gc-clip-rectangles gc)))
	(case clip-rectangles
	  ((nil) ) ; nothing 
	  (:clip (zengl:add-stencil :rect (list 0 0
						(drawable-width pixmap) (drawable-height pixmap))))   ; effectively disable output  
	  (t (dolist (rect clip-rectangles)
	       (zengl:add-stencil :rect rect 
				  :reverse t))))))
    t))

(defmethod switch-read-target ((window window))
  (setf *saved-read-target*
	window)
  (zengl:read-framebuffer (screen-framebuffer (drawable-screen window)))
  (apply #'zengl:read-origin
	 (screen-coordinates window)))

(defmethod switch-read-target ((pixmap pixmap))
  (when (pixmap-framebuffer pixmap)
    (setf *saved-read-target*
	  pixmap)
    (zengl:read-framebuffer (pixmap-framebuffer pixmap))
    (apply #'zengl:read-origin
	   '(0 0))))

;; Random note : 2 reasons GC might not be present : 1) a RENDER request with GC fields in the picture object. 2) ClearArea
(defmacro defrequest/draw (name xargs &body body)
  (let ((target (first (intersection '(drawable window pixmap dst)
				     xargs
				     :test #'(lambda (elt xarg)
					       (eql (first (mklist xarg))
						    elt))))))
    `(defrequest ,name ,xargs
       (with-draw-target (,target :gc ,(if (member 'gc ; TODO (v2) : extract ClipByChildren value for a Render request (it's a field of Picture)
						   xargs
						   :key (compose #'first #'mklist)
						   :test #'equal)
					   'gc
					   nil))
	 ,@body))))

(define-constant +copyarea-opcode+
    (first (rassoc :CopyArea
		  +request-opcodes+)))

;; TODO : more correctly, stencil the dst with all the corresponding occluded areas in src.
;; and tile missing areas if required.
(defrequest/draw :CopyArea ((src drawable) (dst drawable) (gc gc) src-x src-y width height dst-x dst-y)  ; internal argument
  (unless (and (eql (drawable-depth src)  (drawable-depth dst))
	       (eql (drawable-screen src) (drawable-screen dst)))
    (xerror :Match))
  (switch-read-target src)
  (zengl:shift-read-origin src-x
			   src-y)
  (zengl:shift-draw-origin dst-x
			   dst-y)
  ;; TODO : make sure function = Copy and plane-mask is all 1's.
  (zengl:copy-area width
		   height)
  (when (and (not (eql *internal-gc*
		       gc))
	     (gc-graphics-exposures gc))
    (if (or (occluded? src)
	    (> (+ src-x width)
	       (drawable-width src))
	    (> (+ src-y height)
	       (drawable-height src)))
	(event :GraphicsExposure :drawable src :major-opcode +copyarea-opcode+)  ; approximation : graphics-expose the whole source
	(event :NoExposure :drawable src :major-opcode +copyarea-opcode+))))

(defmacro convert-resource (place class)
  `(when (numberp ,place)
     (setf ,place
	   (find-res ,place))
     (unless (is-class ,place ',class)
       (xerror ,(make-keyword class)
	       ,place))))

;; TODO : check the args !
(defrequest :CreateGC (cid (drawable drawable) value-list)
  (let ((gc (apply #'make-instance
		   'gc
		   :root  (screen-root (drawable-screen drawable))
		   :depth (drawable-depth drawable)
		   value-list)))
      (create-res cid
		  gc)
      (convert-resource (gc-font gc) font)
      (convert-resource (gc-root gc) window)))

(defrequest :CopyGC ((src-gc gc) (dst-gc gc) value-mask)
  (if (not (and (eql (gc-root src-gc)
		     (gc-root dst-gc))
		(eql (gc-depth src-gc)
		     (gc-depth dst-gc))))
      (xerror :Match)
      (progn
	(setf (gc-clip-rectangles dst-gc)
	      (gc-clip-rectangles src-gc))
	(dolist (keyword value-mask)
	  (let ((slot (intern (symbol-name keyword))))
	    (setf (slot-value dst-gc
			      slot)
		  (slot-value src-gc
			      slot)))))))

;; TODO (same as CreateGC) : check values
(defrequest :ChangeGC ((gc gc) value-list)
  (mapc #'(lambda (item)
	    (bind (slot-keyword value)
		item
	      (when (eql :clip-mask
			 slot-keyword)
		(setf (gc-clip-rectangles gc) 
		      nil))
	      (setf (slot-value gc
				(intern (symbol-name slot-keyword)))
		    value)))
	(group value-list
	       2))
  (convert-resource (gc-font gc) font)
  (convert-resource (gc-root gc) window))

(defrequest :FreeGC ((gc gc))
  (destroy-res gc))
	  
(defrequest :TranslateCoordinates ((src-window window) (dst-window window) src-x src-y)
  (bind (dst-x dst-y)
      (mapcar #'+
	      (list src-x src-y)
	      (mapcar #'-
		      (screen-coordinates dst-window)
		      (screen-coordinates src-window)))
    (reply :same-screen t
	   :dst-x dst-x :dst-y dst-y
	   :child (find-if #'(lambda (child)
			       (with-slots (x y width height)
				   child
				 (and (<= x dst-x (+ x width))
				      (<= y dst-y (+ y height)))))
			   (window-subwindows dst-window)))))
    
;; for v2
(defun tile-with-pixmap (&rest args)
  nil)

;; pixmap stuff unfinished
(defun draw-background (window x1 y1 x2 y2)
  (aif (attr-background-pixel (window-attr window))
       (zengl:rectangle x1 y1 x2 y2
			(pixel-color it))
       (mvbind (background-pixmap xt yt)
	   (funcall (alambda (win)
		      (let ((background-pixmap (attr-background-pixmap (window-attr win)))
			    (xt (screencoord-x win))
			    (yt (screencoord-y win)))
			(if (eql :ParentRelative
				 background-pixmap)
			    (self (window-parent win))
			    (values background-pixmap xt yt))))
		    window)
	 (when background-pixmap
	   (tile-with-pixmap background-pixmap
			     xt yt   ; tiling origin
			     (screencoord-x window) (screencoord-y window) ; start of extents
			     (drawable-width window) (drawable-height window)))))) ; size

(defrequest/draw :ClearArea ((window window) x y width height exposures)
  (when (= 0 width)
    (setf width  (drawable-width  window)))
  (when (= 0 height)
    (setf height (drawable-height window)))
  (when exposures
    (event :Expose))
  (zengl:add-stencil :rect (list x y
				 (+ x width) (+ y height))
		     :reverse t)
  (draw-background window
		   x y
		   (+ x width) (+ y height)))

;; caution, arg is a rectangle not (x y width height)
(defun clear-area (window x1 y1 x2 y2 &optional exposures)
  (enqueue-request :ClearArea nil :window window :x x1 :y y1 :width (- x2 x1) :height (- y2 y1) :exposures exposures))

;; lines all same color. Ignore width, stipple.
(defrequest/draw :PolySegment ((drawable drawable) (gc gc) segments)
  (zengl:lines (group (apply #'append
			     segments)
		      2)
	       (pixel-color (gc-foreground gc))
	       (gc-line-width gc)))

(defun convert-points-with-relative-coords (points)
  (do ((ps points (rest ps)))
      ((null (rest ps)))
    (let ((cur (first ps))
	  (next (second ps)))
      (incf (first next) (first cur))
      (incf (second next) (second cur))))
  points)

(defrequest/draw :PolyPoint ((drawable drawable) (gc gc) points coordinate-mode)
  (when (eql :previous
	     coordinate-mode)
    (setf points
	  (convert-points-with-relative-coords points)))
  (zengl:points points
		(pixel-color (gc-foreground gc))))
		    
;; we ignore line Joins anyway... so implement as segments.
(defrequest/draw :PolyLine ((drawable drawable) (gc gc) points coordinate-mode)
  (when (eql :previous
	     coordinate-mode)
    (setf points
	  (convert-points-with-relative-coords points)))
  (zengl:line-strip points
		    (pixel-color (gc-foreground gc))
		    (gc-line-width gc)))

(defrequest/draw :PolyRectangle ((drawable drawable) (gc gc) rectangles)
  (dolist (rect rectangles)
    (bind (x1 y1 w h)
	rect
      (let ((x2 (+ x1 w))
	    (y2 (+ y1 h)))  
	(zengl:line-strip `((,x1 ,y1) (,x1 ,y2) (,x2 ,y2) (,x2 ,y1) (,x1 ,y1))
			  (pixel-color (gc-foreground gc))
			  (gc-line-width gc))))))

(defrequest/draw :PolyFillRectangle ((drawable drawable) (gc gc) rectangles)
  (dolist (rect rectangles)
    (bind (x1 y1 w h)
	rect
      (let ((x2 (+ x1 w))
	    (y2 (+ y1 h)))  
	(zengl:rectangle x1 y1 x2 y2
			 (pixel-color (gc-foreground gc)))))))

(defrequest/draw :FillPoly ((drawable drawable) (gc gc) shape coordinate-mode points)
;;  (unless (eql :Convex shape) (format t "Warning : this app uses nonconvex polys (shape ~A), which are unsupported.~%" shape))
  (when (eql :previous
	     coordinate-mode)
    (setf points
	  (convert-points-with-relative-coords points)))
  (zengl:polygon points
		 (pixel-color (gc-foreground gc))))

(defrequest/draw :PutImage ((drawable drawable) (gc gc) depth width height dst-x dst-y left-pad format data)
  (bind (endian seq)
      data
    (let ((pixmap (cond ((= 1 depth)
			 (bitmap-to-pixmap seq
					   width
					   height
					   (+ width left-pad)
					   :left
					   (pixel-color (gc-foreground gc))
					   (pixel-color (gc-background gc))))
			((and (eql 24 depth)
			      (eql :zpixmap format)
			      (= 0 left-pad))
			 seq)
			(t
			 (xerror :Match))))) ; or nil ?
      (zengl:shift-draw-origin dst-x dst-y)
      (zengl:pixels width height
		    pixmap))))

(defmethod drawable-visualid ((pixmap pixmap))
  nil)
(defmethod drawable-visualid ((window window))
  (window-visualid window))

(defrequest :GetImage ((drawable drawable) x y width height plane-mask format)
  (with-read-target (drawable)
    (zengl:shift-read-origin x y)
    (bind (xr yr)
	(zengl::coord->GL (list 0 height) :read)
      (let ((data (zengl::image<-GL (gl:Read-Pixels xr yr
						    width height
						    :rgba :unsigned-byte)
				    width height)))
	(reply :depth (drawable-depth drawable)
	       :data data
	       :visual (drawable-visualid drawable))))))

;; --------------------------------------------------------------------------------------------------------------
;; 7) Pixel formats  (pictformats actually), visuals
;; note : as in WeirdX, it's perfectly acceptable to offer only one visual to start with.

;; A Visual specifies the translation of a pixel value into color. [Former comment : Visuals define nothing but the "type" parameter in glDrawPixels/glTexImage2D(). Simple !]
;; Offer 2 visuals : 24 and 32 bit. The visual used by the app makes NO difference to me !
;; As for PictFormats (this is for v2) :
;; "The server must support a Direct PictFormat with 8 bits each of red, green,
;; blue and alpha as well as a Direct PictFormat with 8 bits of red, green and
;; blue and 0 bits of alpha.  The server must also support Direct PictFormats
;; with 1, 4 and 8 bits of alpha and 0 bits of r, g and b."

;; use straight alpha in zenGL, premult alpha in zen

;; those 2 fns assume r g b a are premult alpha
(defun color-pixel (r g b &optional (a 0))
  (+ (ash a 24)
     (ash r 16)
     (ash g 8)
     (ash b 0)))

;; fixme : we are assuming a 24 bit visual and alpha should always be set to 1 as a result.
;; v2 : support 32 bit visuals from which we would actually copy alpha too
(defun pixel-color (color)
  (let ((b (ash (logand color       #xff) -0))
	(g (ash (logand color     #xff00) -8))
	(r (ash (logand color   #xff0000) -16))
	(a #xff))
;	(a (ash (logand color #xff000000) -24)))
    (list r g b a)))


;; -----------------------------------------------------------------------------------------
;; Colormap stuff. Minimal implementations

(defclass- colormap (xresource)
  screen
  visualid)

(defrequest :CreateColormap (mid visual (window window) alloc)
  (if alloc
      (xerror :Match)  ;; alloc should be always nil, for TrueColor
      ;; todo much later : check visual ID exists
      (create-res mid
		  (make-instance 'colormap
				 :id mid
				 :screen 0
				 :visualid visual))))

(defrequest :FreeColormap ((cmap colormap))
;; todo
  (destroy-res cmap))

(defrequest :CopyColormapAndFree (mid (src-cmap colormap))
  (with-slots (screen visualid)
      src-cmap
    (create-res mid
		(make-instance 'colormap
			       :screen screen
			       :visualid visualid))))

(defrequest :InstallColormap ((cmap colormap))
  (push cmap
	*installed-colormaps*))

(defrequest :UninstallColormap ((cmap colormap))
  (setf *installed-colormaps*
	(delete cmap
		*installed-colormaps*)))

(defrequest :ListInstalledColormaps ((window window))
  (reply :cmaps *installed-colormaps*))

(defrequest :FreeColors ((cmap colormap) pixels plane-mask)
  ;; nothing to do
  )

(defrequest :StoreColors ((cmap colormap) items)
  (when items
    (xerror :Access)))  ; Not possible for TrueColor

(defrequest :StoreNamedColor ((cmap colormap) pixel name do-red do-green do-blue)
  (xerror :Access))

(defrequest :QueryColors ((cmap colormap) pixels)
  (reply :colors (mapcar #'(lambda (pixel)
			     (mapcar (curry #'* 256)
				     (butlast (pixel-color pixel))))
			 pixels)))

(defrequest :AllocColor ((cmap colormap) red green blue)
  (reply :red red
	 :green green
	 :blue blue
	 :pixel (let ((r8 (floor red 256))
		      (g8 (floor green 256))
		      (b8 (floor blue 256)))
		  (color-pixel r8 g8 b8))))

;; Always throw an error as we only support the static TrueColor visual class.
(defrequest :AllocColorCells ((cmap colormap) colors planes contiguous)
  (xerror :Alloc))

(defrequest :AllocColorPlanes ((cmap colormap) colors reds greens blues contiguous)
  (xerror :Alloc))

(defun lookup-color (name)
  (or (rest (assoc name
		   +color-names+
		   :test #'equalp))
      (xerror :Name)))

(defrequest :AllocNamedColor ((cmap colormap) name)
  (bind (r g b)
      (lookup-color name)
    (bind (r16 g16 b16)
	(mapcar (curry #'* 256)
		(list r g b))
      (reply :pixel (color-pixel r g b)
	     :exact-red r16 :exact-green g16 :exact-blue b16
	     :visual-red r16 :visual-green g16 :visual-blue b16))))

(defrequest :LookupColor ((cmap colormap) name)
  (bind (r g b)
      (lookup-color name)
    (bind (r16 g16 b16)
	(mapcar (curry #'* 256)
		(list r g b))
      (reply :exact-red r16 :exact-green g16 :exact-blue b16
	     :visual-red r16 :visual-green g16 :visual-blue b16))))


;; --------------------------------------------------------------------------------------------------------------

;; used in fonts and also for depth 1 pixmaps (usually cursors)
(defun bitmap-to-pixmap (bitmap width height pitch pad-mode &optional (fg '(255 255 255)) (bg '(0 0 0)))
  "Translate bitmap into RGBA array"
  (let ((pixmap (make-array (* 4 width height)))
	(left-pad (ecase pad-mode 
		    (:right 0)
		    (:left (- pitch width)))))
    (dotimes (j height)
      (dotimes (i width)
	(let ((p (+ left-pad
		    i
		    (* pitch 
		       j)))
	      (pixmap-pos (* 4 
			     (+ i
				(* width
				   j)))))
	  (bind (color alpha)
	      (if (= 0 (logand 1 (ash (aref bitmap
					    (floor p 8))
				      (- (mod p 8) 7))))
		  (list bg 0)
		  (list fg #xff))
	    (setf (aref pixmap (+ pixmap-pos 0)) (elt color 0))
	    (setf (aref pixmap (+ pixmap-pos 1)) (elt color 1))
	    (setf (aref pixmap (+ pixmap-pos 2)) (elt color 2))
	    (setf (aref pixmap (+ pixmap-pos 3)) alpha)))))
    pixmap))

(load "fonts")


;; --------------------------------------------------------------------------------------------------------------

(defmethod free-resource ((window window))
  (enqueue-request :DestroyWindow t :window window))

(defmethod free-resource ((pixmap pixmap))
  (enqueue-request :FreePixmap t :pixmap pixmap))

(defmethod free-resource ((gc gc))
  (enqueue-request :FreeGC t :gc gc))

(defmethod free-resource ((font font))
  (enqueue-request :CloseFont t :font font))

(defmethod free-resource ((colormap colormap))
  (enqueue-request :FreeColormap t :cmap colormap))

(defmethod free-resource (resource)
  nil)



;; --------------------------------------------------------------------------------------------------------------
;; Initialisation (or better, put all this in previous sections !)

;; +extensions+ is defined in request-opcodes.lisp
(define-constant +max-request-length+ #x3fffff) ; picked same as Xorg
(define-constant +server-time-wrap+ #x100000000)

(let ((%client-id-mask 0))
;
(defrequest :Connection (protocol-major-version protocol-minor-version authorization-protocol-name authorization-protocol-data)
  (reply :protocol-major-version 11
	 :protocol-minor-version 0
	 :release-number 1742
	 :resource-id-base (incf %client-id-mask)  ; later : client number from 1 to 256 (max clients 255)
	 :resource-id-mask #xffff00 ; later #xffffff00 (last 16 bits = client number, so as to get a range for each client. 00 = server-created resources)
	 :motion-buffer-size 256
	 :maximum-request-length #xffff
	 :image-byte-order :msbfirst
	 :bitmap-format-bit-order :MostSignificant
	 :bitmap-format-scanline-unit 32
	 :bitmap-format-scanline-pad 32
	 :min-keycode #x08
	 :max-keycode #xff
	 :vendor "PYB"
	 :pixmap-formats '((:depth 1  :bits-per-pixel 1  :scanline-pad 32)
			   (:depth 24 :bits-per-pixel 32 :scanline-pad 32))
;			   (:depth 32 :bits-per-pixel 32 :scanline-pad 32))
	 :roots `((:root #x0000013c
		   :default-colormap ,+default-colormap+
		   :white-pixel #x00ffffff
		   :black-pixel #x00000000
		   :current-input-masks (:KeyPress :KeyRelease :EnterWindow :LeaveWindow :Exposure :StructureNotify :SubstructureNotify :SubstructureRedirect :FocusChange :PropertyChange :ColormapChange)
		   :width-in-pixels ,+root-width+
		   :height-in-pixels ,+root-height+
		   :width-in-millimeters ,(floor (* +root-width+ 0.26)) ; ratio correct for my screen...
		   :height-in-millimeters ,(floor (* +root-height+ 0.26))
		   :min-installed-maps 1
		   :max-installed-maps 1
		   :root-visual #x00000021
		   :backing-stores :Never
		   :save-unders nil
		   :root-depth 24
		   :allowed-depths ((:depth 24 :visuals
				     ((:visual-id ,+visual-24+ :class :TrueColor :bits-per-rgb-value 8 :colormap-entries 256 :red-mask #x00ff0000 :green-mask #x0000ff00 :blue-mask #x000000ff)))
				    (:depth 1 :visuals nil)
				    )))))
;;				    (:depth 32 :visuals
;;				      ((:visual-id ,+visual-32+ :class :TrueColor :bits-per-rgb-value 8 :colormap-entries 256 :red-mask #x00ff0000 :green-mask #x0000ff00 :blue-mask #x000000ff))))))))
;
) ; %client-id-mask

(defrequest :NoOperation ()
  )

(let ((%time-origin (get-time-of-day)))
;
(defun server-time (&key reset)
  "In milliseconds"
  (if reset
      (setf %time-origin
	    (get-time-of-day))
      (let ((current-time (get-time-of-day)))
	(mod (floor (+ (* 1000.0 (- (timeval-sec current-time)
				    (timeval-sec %time-origin)))
		       (/ (- (timeval-usec current-time)
			     (timeval-usec %time-origin))
			  1000.0)))
	     +server-time-wrap+))))
) ; %time-origin
	
(defrequest :QueryExtension (name)
  (aif (assoc name
	      +extensions+
	      :test #'string=)
       (bind (major-opcode first-event first-error)
	   (rest it)
	 (reply :present t :major-opcode major-opcode :first-event first-event :first-error first-error))
       (reply :present nil :major-opcode 0 :first-event 0 :first-error 0)))

(defrequest :ListExtensions ()
  (reply :names (mapcar #'first
			+extensions+)))

(defrequest :BigReqEnable ()
  (reply :maximum-request-length +max-request-length+))

(defrequest :SetCloseDownMode (mode)
  (setf (client-closedown-mode *client*)
	mode))

(defrequest :QueryBestSize (class (drawable drawable) width height)
  (reply :width 64 :height 64)) ; random value

;; Issue to doublecheck : if silent and client expected a reply, client is now stuck.
(defmacro unimplemented (name &optional silent?)
  `(push (cons ',name
	       #'(lambda (client seqnum &rest args)
		   (format t "Unimplemented request ~A called with args ~A~%" ',name args)
		   (setf (client-processed-seqnum client)
			 seqnum)
		   (unless ,silent?
		     (send :Error
			   client
			   (list :major-opcode (major-opcode ',name)
				 :minor-opcode (minor-opcode ',name)
				 :code (first (rassoc :Implementation
						      +error-codes+))
				 :sequence-number seqnum
				 :resource 0)))))
	 *requests*))


(defrequest :GrabPointer ((grab-window window) owner-events event-mask pointer-mode keyboard-mode (confine-to window (nil)) cursor time)
  (reply :status :success))

(defrequest :GrabKeyboard ((grab-window window) owner-events pointer-mode keyboard-mode time)
  (reply :status :success))

(unimplemented :UngrabKeyboard t)
(unimplemented :UngrabPointer t)
(unimplemented :GrabKey t)
(unimplemented :UngrabKey t)
(unimplemented :GrabButton t)
(unimplemented :UngrabButton t)
(unimplemented :ChangeActivePointerGrab t)
(unimplemented :AllowEvents t)

(unimplemented :KillClient)
(unimplemented :ChangeSaveSet t)

(unimplemented :GrabServer t)
(unimplemented :UngrabServer t)

(unimplemented :ChangePointerControl)
(unimplemented :GetPointerControl)
(unimplemented :ChangeKeyboardMapping)
(unimplemented :ChangeKeyboardControl)
(unimplemented :GetKeyboardControl)
(unimplemented :SetPointerMapping)

(unimplemented :GetMotionEvents)
(unimplemented :QueryKeymap)

(unimplemented :PolyArc t)
(unimplemented :PolyFillArc t)
(unimplemented :CopyPlane)
(unimplemented :SetDashes t)

(unimplemented :CreateCursor t)
(unimplemented :CreateGlyphCursor t)
(unimplemented :FreeCursor t)
(unimplemented :RecolorCursor t)

(unimplemented :Bell t)

(unimplemented :SetScreenSaver t)
(unimplemented :GetScreenSaver)
(unimplemented :ForceScreenSaver t)

(unimplemented :ChangeHosts)
(unimplemented :ListHosts)
(unimplemented :SetAccessControl)

(load "selection")

(defvar *display* nil)
(defvar *glx-display* nil)
(defvar *w* nil)  ; the X window for zen to display in.
(defvar *glx-w* nil)  ; and its matching GlxWindow.

(defconstant +root-background-color+ #x000044)

(defun zen-init ()
  (let ((rootwin (make-instance 'window
				:id +root-id+
				:parent nil
				:class :InputOutput
				:attr (make-instance 'attr :background-pixel +root-background-color+ :border-pixmap nil :border-pixel 0)
				:visualid +visual-24+
				:depth 24
				:x 0 :y 0
				:border-width 0
				:mapped t
				:viewable t
				:width +root-width+ :height +root-height+)))
    (create-res +root-id+
		rootwin)
    (setf *cursor-confine-window* rootwin)
    (setf *saved-read-target* rootwin)
    (setf *saved-draw-target* rootwin)
    (int-request :CreateColormap t
		 :mid +default-colormap+
		 :visual +visual-24+
		 :window rootwin
		 :alloc nil)
    (int-request :InstallColormap t
		 :cmap +default-colormap+)
    (setf (attr-colormap (window-attr rootwin))
	  (find-res +default-colormap+))
    
    (setf *internal-gc* 
	  (make-instance 'gc))
    (let ((screen (make-screen :id 0
			       :root rootwin
			       :framebuffer (zengl:make-framebuffer :fbo 0
								    :width +root-width+
								    :height +root-height+))))
      (setf (drawable-screen rootwin)
	    screen)
      (setf *screen*
	    screen)
      (zengl:init (screen-framebuffer screen)))))

(defun glx-init ()
  (setf *glx-display* (glx:XOpenDisplay nullp)
	*display* (x:open-default-display))
  (let* ((scr (car (x:display-roots *display*)))
	 (rootw (x:screen-root scr))
	 (double-buffer-attributes (cffi:foreign-alloc :int
						       :initial-contents (list +GLX_DRAWABLE_TYPE+ +GLX_WINDOW_BIT+
									       +GLX_RENDER_TYPE+ +GLX_RGBA_BIT+
									       +GLX_DOUBLEBUFFER+ 1 ; True
									       +GLX_RED_SIZE+   1
									       +GLX_GREEN_SIZE+  1
									       +GLX_BLUE_SIZE+   1
									       +GLX_ALPHA_SIZE+   1
									       +GLX_STENCIL_SIZE+   1
									       ;; +GLX_SAMPLE_BUFFERS+ 1
									       ;; +GLX_SAMPLES+ 4
									       +None+)))
	 (nels (cffi:foreign-alloc :int))
	 (pvid (cffi:foreign-alloc :int))
	 (fb-config (cffi:mem-aref (glx:%choose-fb-config *glx-display* 0 double-buffer-attributes nels) ; Todo : replace 0 by the actual value of (DefaultScreen *glx-display*) (get it via CLX)
				   :pointer 0))
	 vid colormap)
    ;; (setf *root-width* (screen-width scr))      ; will be useful later
    ;; (setf *root-height* (screen-height scr))
    (glx:%get-fb-config-attrib *glx-display*
			       fb-config
			       +GLX_VISUAL_ID+
			       pvid)
    (setf vid (cffi:mem-aref pvid :int))
    (cffi:foreign-free nels)
    (cffi:foreign-free pvid)

    (glx:XFlush *glx-display*)

    (setf colormap (x:create-colormap vid rootw nil))
    (x:install-colormap colormap)

    (setf *w* (x:create-window :parent rootw
			       :x +root-x+
			       :y +root-y+
			       :width +root-width+
			       :height +root-height+
			       :colormap colormap
			       :depth (x:screen-root-depth scr)
			       :class :input-output
			       :visual vid
			       :event-mask (x:make-event-mask :exposure :structure-notify)
			       :background (x:alloc-color colormap (x:lookup-color colormap "black"))
			       :border 0
			       :override-redirect :off))
    (x:display-finish-output *display*)
    (setf *glx-w*
	  (glx:%create-window *glx-display* fb-config (x:drawable-id *w*) nullp))
    (glx:XFlush *glx-display*)
    (glx:%make-context-current *glx-display*
			       *glx-w*
			       *glx-w*
			       (glx:%create-new-context *glx-display* fb-config +GLX_RGBA_TYPE+ nullp 1))
    (glx:XFlush *glx-display*)
    (setf zengl:*get-proc-address*     ; lispifies glXGetProcAddress() and use it in cl-opengl, via zenGL.
	  #'(lambda (name)
	      (glx:%get-proc-address (cffi:foreign-string-alloc name))))))

(defun first-clear ()
  (let ((rootwin (screen-root *screen*)))
    (enqueue-request :ClearArea t
		     :window rootwin
		     :x 0 :y 0
		     :width +root-width+ :height +root-height+
		     :exposures nil)))

(defvar *glx-window-exists* nil)

(defun main ()
  (x:map-window *w*)
  (x:display-force-output *display*)
  ;; The glxwindow never gets exposed more than once.
  ;; close-display will be invoked when zen dies.
  (handler-case
      (block l
	(loop
	   (x:event-case (*display* :discard-p t)
	     (:exposure (window count)
			t)
	     (:configure-notify (window)
				t)
	     (:map-notify (window)
			  (setf *glx-window-exists* t)
			  (first-clear)
			  t)
	     (:destroy-notify ()
			      (return-from l)
			      t))))
    (end-of-file (condition)
      (format t "EOF condition.~%")))
  (x:close-display *display*)
  (glx:XCloseDisplay *glx-display*))

;; remove after testing. part of (rst)
(defun reset-server ()
  (setf *request-queue* (make-queue))
  (setf *resources* nil)
  (glx-init)
  (zen-init))

;;(sb-ext:save-lisp-and-die "xd"  :executable t)


