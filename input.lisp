;; (C) 2011 Pierre-Yves Baccou

;; Idea (if slow) only fully process 1 motion event every n

(load "input-devices.lisp")

(defconstant +default-cursor+ "cursor-arrow.gif")
(defconstant +default-cursor-hotspot+ '(7 2))

(require "deps" "deps.lisp")
(require 'skippy)

(load "data/keys")

(require 'cl-opengl)
(require "zengl" "zengl")
;; (load "cursor/hwcursor.lisp")

;;(load "util/libxpm.lisp")

(define-constant +relative-axes+
  '((#x00 . :rel-x)
    (#x01 . :rel-y)
    (#x02 . :rel-z)
    (#x03 . :rel-rx)
    (#x04 . :rel-ry)
    (#x05 . :rel-rz)
    (#x06 . :rel-hwheel)
    (#x07 . :rel-dial)
    (#x08 . :rel-wheel)
    (#x09 . :rel-misc)
    (#x0f . :rel-max)))

(define-constant +input-event-types+
  '((#x00 . :ev-syn)
    (#x01 . :ev-key)
    (#x02 . :ev-rel)
    (#x03 . :ev-abs)
    (#x04 . :ev-msc)
    (#x05 . :ev-sw)
    (#x11 . :ev-led)
    (#x12 . :ev-snd)
    (#x14 . :ev-rep)
    (#x15 . :ev-ff)
    (#x16 . :ev-pwr)
    (#x17 . :ev-ff-status)
    (#x1f . :ev-max)))

(define-constant +input-key-values+
  '((0 . :released)
    (1 . :pressed)
    (2 . :repeat)))

(define-constant +timeval-struct+
  '(((0 4) #'int32 tv-sec)
    ((4 8) #'uint32 tv-usec)))

(define-constant +input-event-struct+
    `(((0 8)
       (translate-struct-fn ,+timeval-struct+)
       time)
      ((8 10)
       (compose #'rest (rcurry #'assoc +input-event-types+) #'uint16)
       type)
      ((10 12)
       (case type
;;	 (:ev-key (compose #'rest (rcurry #'assoc +input-event-keys+) #'uint16))
	 (:ev-rel (compose #'rest (rcurry #'assoc +relative-axes+) #'uint16))
	 (t #'uint16))
       code)
      ((12 16)
       #'int32
       value)))

;; why ? what's wrong with defdecoder ?
(defmacro translate-struct-fn (proto)
  (let* ((vars nil)
	 (bindings (mapcar #'(lambda (proto-element)
			       (bind ((k1 k2) fn name)
				   proto-element
			 	 (push name vars)
				 `(,name (funcall ,fn
						  (subseq seq
							  ,k1
							  ,k2)))))
			   proto)))
    `(lambda (seq)
       (let* (,@bindings)
	 (list ,@(reverse vars))))))

(fsetf translate-input-event
       (translate-struct-fn #.+input-event-struct+))

(let ((seq (make-array 16 :element-type '(unsigned-byte 8)))
      (buf (cffi:foreign-alloc :uint8 :initial-element 0 :count 16))
      (fdsets (list (posix-fdset) (posix-fdset) (posix-fdset)))) 
;
(defun next-input-event ()
  (let ((readfds (car (posix-select :readfds (mapcar #'second
						     *input-fds*)
				    :timeout? nil
				    :interruptible? t
				    :fdsets fdsets))))
		      
    (when readfds
      (let ((fd (first readfds)))
	(when (= -1
		 (%posix-read fd
			      buf
			      16))
	  (error "Couldn't read input event"))
	(dotimes (i 16)
	  (setf (elt seq i)
		(cffi:mem-aref buf :uint8 i)))
	(let ((source (first (find fd
				   *input-fds*
				   :key #'second))))
	  (cons source
		(translate-input-event seq))))))) ; => (:mouse/kbd time type code value)
;
) ; seq, buf, fdsets

;; todo : we should keep float coords if we use the factor.

(defvar *button-state* '((:Button1 nil) (:Button2 nil) (:Button3 nil) (:Button4 nil) (:Button5 nil)))
(defvar *modifier-state* '((:Shift nil) (:Lock nil) (:Control nil) (:Mod1 nil) (:Mod2 nil) (:Mod3 nil) (:Mod4 nil) (:Mod5 nil)))
(defvar *modifier-keycodes* +default-modifier-keycodes+)		

(defvar *scancode-to-keysyms* +scancode-to-keysyms+)

(defvar *cursor-factor* '(1.0 1.0))
(defvar *input-thread* nil)
(defvar *cursor-window* nil)
(defvar *cursor-exact-coords* '(0.0 0.0))
(defvar *cursor-width*)
(defvar *cursor-height*)
(defvar *cursor-hotspot* +default-cursor-hotspot+)

(defun butmod-state ()
  (mapcar #'first
	  (remove nil
		  (append *button-state*
			  *modifier-state*)
		  :key #'second)))
	
(defun cursor-coords ()
  (mapcar #'floor
	  *cursor-exact-coords*))

;; Using same technique as XOrg's XYToWindow(). Note as a window's subwindows are well ordered, the topmost window is correctly chosen.
;; Note xp, yp can be < 0 (if in the border)
(defun xy-to-window (coords window)
  "Find what window the point x,y (in window's coord system) is in. "
  (with-slots (width height subwindows)
      window
    (bind (xp yp)
	coords
      (let ((child (find-if #'(lambda (win)
				(with-slots ((xc x) (yc y) (wc width) (hc height) (bwc border-width))
				    win
				  (and (window-viewable win)
				       (<= xc xp (+ xc (* 2 bwc) wc))
				       (<= yc yp (+ yc (* 2 bwc) hc)))))
			    subwindows)))
	(if (or (null child)
		(>= xp width)
		(>= yp height))
	    (list xp yp
		  window)
	    (with-slots ((xc x) (yc y) (bwc border-width))
		child
	      (xy-to-window (list (- xp xc bwc)
				  (- yp yc bwc))
			    child)))))))

;; This threading would be much more useful with a HW cursor
(defun input-thread ()
  (setf *cursor-window*
	(screen-root *screen*))
;;  (init-HW-cursor)
  (loop
     (awhen (next-input-event)
       (bind (source time type code value)
	   it
  	 (cond ((and (eql :mouse source)
		     (eql :ev-rel type))
		(enqueue-request :MoveCursor nil
				 :code code :value value))
	       ((and (eql :mouse source)
		     (eql :ev-key type))
		(enqueue-request :Button nil :code code :value value))
	       ((and (eql :kbd source)
		     (eql :ev-key type))
		(enqueue-request :Keyboard nil
				 :code (+ 8 code)
				 :value value))
	       (t nil))))))

(defun open-cursor (file)
  (let* ((image (elt (skippy:images (skippy:load-data-stream file)) 0))
	 (width (skippy:width image))
	 (height (skippy:height image))
	 (data (skippy:image-data image)))
    (let ((arr (make-array (* 4 width height))))
      (dotimes (i (* width height))
	(bind (a r g b)
	    (ecase (aref data i)
	      (254 '(0 0 0 0))
	      (255 '(255 0 0 0))
	      (0 '(255 255 255 255)))
	  (setf (aref arr (+ 0 (* 4 i))) a
		(aref arr (+ 1 (* 4 i))) r
		(aref arr (+ 2 (* 4 i))) g
		(aref arr (+ 3 (* 4 i))) b)))
      (values arr width height))))

(let (%cursor-save-under %cursor-image %drawing-in-progress)
;
(defun init-SW-cursor ()
  (mvbind (data width height)
      (open-cursor +default-cursor+)
    (setf *cursor-width*  width
	  *cursor-height* height
	  %cursor-save-under (zengl:create-framebuffer width height)
	  %cursor-image      (zengl:create-framebuffer width height))
    (zengl:draw-framebuffer %cursor-image)
    (zengl:pixels width height
		  data)))
;
(defun hide-SW-cursor (x y)
  (zengl:read-framebuffer %cursor-save-under)
  (zengl:read-origin 0 0)
  (zengl:draw-framebuffer (screen-framebuffer *screen*))
  (apply #'zengl:draw-origin (mapcar #'-
				     (list x y)
				     *cursor-hotspot*))
  (zengl:copy-area *cursor-width* *cursor-height*) ; todo : make sure transparency can't be on
  (when *saved-read-target*
    (switch-read-target *saved-read-target*))
  (when *saved-draw-target*
    (switch-draw-target *saved-draw-target*))
  (zengl:flush))
;
(defun draw-SW-cursor (x y)
  (if %drawing-in-progress
      (error "Bug : drawing SW cursor twice in a row !~%")
      (setf %drawing-in-progress t))
  ;; save under
  (zengl:read-framebuffer (screen-framebuffer *screen*))
  (zengl:draw-framebuffer %cursor-save-under)
  (apply #'zengl:read-origin (mapcar #'-
				     (list x y)
				     *cursor-hotspot*))
  (zengl:draw-origin 0 0)
  (zengl:copy-area *cursor-width* *cursor-height*)
  ;; draw cursor
  (zengl:read-framebuffer %cursor-image)
  (zengl:read-origin 0 0)
  (zengl:draw-framebuffer (screen-framebuffer *screen*))
  (apply #'zengl:draw-origin (mapcar #'-
				     (list x y)
				     *cursor-hotspot*))
  (gl:enable :blend)
  (gl:Blend-Func :src-alpha :one-minus-src-alpha)
  (zengl:copy-area *cursor-width* *cursor-height*)
  (gl:disable :blend)
  (when *saved-read-target*
    (switch-read-target *saved-read-target*))
  (when *saved-draw-target*
    (switch-draw-target *saved-draw-target*))
  (zengl:flush)
  (setf %drawing-in-progress nil))
;
) ; %cursor-save-under %cursor-image)


;; TODO : invent a macro/fn utility that memoizes the last result
(defun update-cursor-window ()
  "Update cursor win and return t if it actually changed"
  (bind (xp yp new-cursor-win)
      (xy-to-window (cursor-coords)
		    (screen-root *screen*))
    (unless (eql *cursor-window*
		 new-cursor-win)
      (let ((details (cond ((member *cursor-window*
				    (all-parents new-cursor-win))
			    '(:Inferior :Ancestor))
			   ((member new-cursor-win
				      (all-parents *cursor-window*))
			    '(:Ancestor :Inferior))
			   (t
			    '(:Nonlinear :Nonlinear)))))
	(event :LeaveNotify :detail (first details)  :source *cursor-window*)
	(event :EnterNotify :detail (second details) :source new-cursor-win)
	(setf *cursor-window*
	      new-cursor-win))
      t)))

(defrequest :Button (code value)
  (let ((nbutton (second (assoc code 
			       +button-codes+))))
    (setf (second (nth nbutton
		       *button-state*))
	  (case value
	    (1 t)
	    (0 nil)))
    (case value 
      (1 (event :ButtonPress   :detail nbutton :source *cursor-window*)) ; see note about :MotionNotify
      (0 (event :ButtonRelease :detail nbutton :source *cursor-window*)))))

(defvar *keyboard-state*
  (make-array 248 :initial-element nil))

;; internal
(defrequest :Keyboard (code value)
  (setf (aref *keyboard-state* 
	      code)
	value)
  (when-let ((modifier (first (find code
				    *modifier-keycodes*
				    :test #'(lambda (code item)
					      (find code
						    (second item)))))))
    (setf (second (assoc modifier
			 *modifier-state*))
	  (case value
	    (1 t)
	    (0 nil))))
  (case value 
    (1 (event :KeyPress   :detail code :source *cursor-window*)) ; see note about :MotionNotify
    (0 (event :KeyRelease :detail code :source *cursor-window*))))

;; internal. Could be rewritten so as to do most of work within input thread
(defrequest :MoveCursor (code value)
  (let ((shift (ecase code
		 (:rel-x `(,value 0))
		 (:rel-y `(0 ,value))
		 (:rel-wheel `(0 0)))))
    (with-slots ((xc x) (yc y) (widthc width) (heightc height))
	*cursor-confine-window* ;; usually root window
      (setf shift
	    (mapcar #'max  ; unclear
		    shift
		    (mapcar #'-
			    (list xc yc)
			    *cursor-exact-coords*)))
      (setf shift
	    (mapcar #'min
		    shift
		    (mapcar #'-
			    (list widthc heightc)
			    *cursor-exact-coords*))))
    (unless (equal '(0 0)
		   shift)
      (move-cursor (mapcar #'+
			   *cursor-exact-coords*
			   shift)))))

(defun move-cursor (coords)
  (bind (x1 y1)
      (cursor-coords)
    (setf *cursor-exact-coords*
	  coords)
    (bind (x2 y2)
	(cursor-coords)
      (hide-SW-cursor x1 y1)
      (draw-SW-cursor x2 y2)
      (unless (update-cursor-window)
	(event :MotionNotify :detail :normal :source *cursor-window*))))) ; note source is not a final :MotionNotify event argument, just used for construction of the others

(defrequest :WarpPointer ((src-window window (nil)) (dst-window window (nil)) src-x src-y src-width src-height dst-x dst-y)
  (if (null dst-window)
      (move-cursor (mapcar #'+
			   *cursor-exact-coords*
			   (list dst-x dst-y)))
      (when (or (null src-window)
		(progn
		  (when (= 0 src-width)
		    (setf src-width 
			  (- (window-width src-window)
			     src-x)))
		  (when (= 0 src-height)
		    (setf src-height
			  (- (window-height src-window)
			     src-y)))
		  (bind (x y)
		      (cursor-coords)
		    (bind (x0 y0)
			(screen-coordinates src-window)
		      (and (<= (+ x0 src-x) x (+ x0 src-x src-width))
			   (<= (+ y0 src-y) y (+ y0 src-y src-height)))))))
	(move-cursor (mapcar #'+
			     (screen-coordinates dst-window)
			     (list dst-x dst-y))))))
      
(defrequest :GetKeyboardMapping (first-keycode count)
  (unless (<= 8 first-keycode (- 256 count))
    (xerror :Value))
  (reply :keysyms-per-keycode 2
	 :keysyms (apply #'append
			 (mapcar #'rest
				 (subseq *scancode-to-keysyms*
					 (- first-keycode
					    8)
					 (+ count
					    (- first-keycode
					       8)))))))
			
(defrequest :QueryPointer ((window window))
  (bind (root-x root-y)
      (cursor-coords)
    (bind (win-x win-y)
	(mapcar #'-
		(list root-x root-y)
		(screen-coordinates window))
      (let ((rootwin (screen-root *screen*)))
	(reply :root rootwin
	       :child (find *cursor-window*
			    (window-subwindows window))
	       :same-screen t
	       :root-x root-x :root-y root-y
	       :win-x win-x :win-y win-y
	       :mask (butmod-state))))))

(defrequest :GetModifierMapping ()
  (reply :keycodes-per-modifier 2
	 :keycodes (apply #'append
			  (mapcar #'(lambda (item)
				      (bind (modifier (&optional (code1 0) (code2 0)))
					  item
					(list code1 code2)))
				  *modifier-keycodes*))))

(defrequest :SetModifierMapping (keycodes-per-modifier keycodes)
  (unless (= (length keycodes)
	     (* 8 keycodes-per-modifier))
    (xerror :Length))
  (if (find t
	    *modifier-state*
	    :key #'second)
      (reply :status :busy)
      (let ((codes (group keycodes 
			  keycodes-per-modifier)))
	(dotimes (i 8)
	  (setf (second (elt *modifier-keycodes*
			     i))
		(elt keycodes
		     i))))))

(defrequest :GetPointerMapping ()
  (reply :map '(1 2 3 4 5)))
