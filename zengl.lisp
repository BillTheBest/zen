;; (C) 2011 Pierre-Yves Baccou

;; to add : clear

;;--------------------------------------------------------------------------

;; zenGL : an API layer between zen and OpenGL.

;; Render targets, Offscreen rendering
;; Drawing primitives
;; Pixel data, images
;; TODO : Blend
;; (implicit feature) use alpha.
;; Stencil option in 2,3
;; GLinit
;; coord->GL color-<>GL
;; TODO : GL OOM manager
;; TODO : images
;; TODO : whatever is useful for glyph stuff
;; TODO : stencil from framebuffer. For this, we require a framebuffer with a (1-bit used) stencil buffer and nothing else. 
;; zenGL:create-framebuffer should have an option :stencil? so that zen's PutImage could realise depth-1-pixmaps as pure stencil fbos. (do we require dummy color buffers too ?)

;; all GL functions that should (eg gl:color, gl:vertex) accept arguments packaged as a list also.
;; ex :  (gl:color (argb-to-color argb))
;; instead of    (apply #'gl:color (argb-to-color argb))

;; vertices (gl:vertex for a sequence of vertices). Create a vertex array above a certain size ?

;; consider, for large data, a new sequence type compatible with a cffi byte array, directly usable by C functions. (to avoid large array copies)
;; implement this as an extension to Common Lisp ?

;; (:use "cl-opengl" ?)

;; note : *screen* belongs to zen, not to zengl. The on-screen framebuffer, just as the glx context, is created outside zengl and passed to it as an argument.

(in-package "ZENGL")

(require "deps" "deps")

(require 'cl-opengl)
(load "util/cl-opengl-fix.lisp")

(load "region.lisp")

(defvar *get-proc-address* nil)

;; in X coordinates
(defvar *draw-origin-x* nil)
(defvar *draw-origin-y* nil)
(defvar *read-origin-x* nil)
(defvar *read-origin-y* nil)

(defvar *draw-framebuffer* nil)
(defvar *read-framebuffer* nil)

;;------------------------------------------------------------------------
;; ZenGL

(defstruct framebuffer
  fbo
  texture
;;  rb ; unused. In v2, texture OR fb will be non nil.
  depth-st-rb
  width
  height)

;; allow pbuffers in v2
;; Different depths ? (1, 24, 32)
(defun create-framebuffer (width height &key source)
  (let ((texture  (first (gl:Gen-Textures      1)))
	(depth-st-rb (first (gl:Gen-Renderbuffers-ext 1)))
	(fbo      (first (gl:Gen-Framebuffers-ext 1))))
    ;
    (gl:Bind-Texture :texture-2d texture)
    ;; necessary, otherwise mipmapping is turned on by default and texturing will disable itself for lack of mipmaps !
    (gl:Tex-Parameter :texture-2d :texture-mag-filter :linear)
    (gl:Tex-Parameter :texture-2d :texture-min-filter :linear)
    (gl:Tex-Env :texture-env :texture-env-mode :replace)
    (if (null source)
	(gl:Tex-Image-2D :texture-2d 0 :RGBA width height 0 :RGBA :unsigned-byte (cffi:null-pointer))  ; FIXME : not gonna work. data will be mirrored about y
	(progn
	  (read-framebuffer source)
	  (gl:Copy-Tex-Image-2D :texture-2d 0 :RGBA 0 0 width height 0)))
    ;
    (gl:Bind-Renderbuffer-ext :renderbuffer depth-st-rb)
    (gl:Renderbuffer-Storage-ext :renderbuffer :depth-stencil width height)
    ;
;    (gl:Bind-Framebuffer-ext :draw-framebuffer fbo)
;    (gl:Framebuffer-Texture-2D-ext :draw-framebuffer :color-attachment0 :texture-2d texture 0)
;   (gl:Framebuffer-Texture-2D-ext :read-framebuffer :color-attachment0 :texture-2d texture 0)
;    (gl:Framebuffer-Renderbuffer-ext :draw-framebuffer :depth-attachment :renderbuffer depth-rb)
    ;
    (setf *draw-framebuffer* nil  
	  *read-framebuffer* nil) ; because these 2 settings are lost.
    (let ((framebuffer (make-framebuffer :fbo fbo :texture texture :depth-st-rb depth-st-rb :width width :height height)))
      (gl:Bind-Renderbuffer-ext :renderbuffer 0)
;;      (sb-ext:finalize framebuffer ; see also the trivial-garbage library on CLiki
;;		       #'(lambda ()
			   ;; FIXME : switch to original creating context temporarily
;;			   (gl:Delete-Framebuffers-ext fbo)
;;			   (gl:Delete-Renderbuffers-ext depth-rb)
;;			   (gl:Delete-Textures texture)))
      framebuffer)))

(defun image->GL (data width height)
  (declare (type fixnum width height))
  (let ((new (make-array (* 4 width height)
			 :element-type 'unsigned-byte))
	(l (* 4 width height))
	(base-src 0) (base-dst 0) (a 0) (r 0) (g 0) (b 0))
    (declare (type (unsigned-byte 8) a r g b))
    (declare (type fixnum base-src base-dst l))
    (dotimes (i height)
      (setf base-src (- l (* 4 width (1+ i)))
	    base-dst (* 4 width i))
      (dotimes (j width)
	(setf a (aref data (the fixnum (+ base-src 0)))
	      r (aref data (the fixnum (+ base-src 1)))
	      g (aref data (the fixnum (+ base-src 2)))
	      b (aref data (the fixnum (+ base-src 3)))
	      (svref new (the fixnum (+ base-dst 0))) r
	      (svref new (the fixnum (+ base-dst 1))) g
	      (svref new (the fixnum (+ base-dst 2))) b
	      (svref new (the fixnum (+ base-dst 3))) a)
	(incf base-src 4)
	(incf base-dst 4)))
    new))

(defun image<-GL (data width height)
  (declare (type fixnum width height))
  (let ((new (make-array (* 4 width height)
			 :element-type 'unsigned-byte))
	(l (* 4 width height))
	(base-src 0) (base-dst 0) (a 0) (r 0) (g 0) (b 0))
    (declare (type (unsigned-byte 8) a r g b))
    (declare (type fixnum base-src base-dst l))
    (dotimes (i height)
      (setf base-src (- l (* 4 width (1+ i)))
	    base-dst (* 4 width i))
      (dotimes (j width)
	(setf r (aref data (the fixnum (+ base-src 0)))
	      g (aref data (the fixnum (+ base-src 1)))
	      b (aref data (the fixnum (+ base-src 2)))
	      a (aref data (the fixnum (+ base-src 3)))
	      (svref new (the fixnum (+ base-dst 0))) a
	      (svref new (the fixnum (+ base-dst 1))) r
	      (svref new (the fixnum (+ base-dst 2))) g
	      (svref new (the fixnum (+ base-dst 3))) b)
	(incf base-src 4)
	(incf base-dst 4)))
    new))

(defun color->GL (color)
  (bind (r g b &optional (a 255)) ; a ignored
      color 
    (bind (r1 g1 b1 a1)
	(mapcar (rcurry #'/ 255.0)
		(list r g b a))
      (list r1 g1 b1 a1))))

#|
;; use this in v2
;; convert 0-255 premult alpha to 0-1 straight alpha
(defun color->GL (color)
  (bind (r g b &optional (a 255.0))
      color
    (bind (r1 g1 b1 a1)
	(mapcar (rcurry #'/ 255.0)
		(list r g b a))
      (if (or (= a1 0)
	      (= a1 1))
	  (list r1 g1 b1 a1)
	  (list (/ r1 a1)
		(/ g1 a1)
		(/ b1 a1)
		a1)))))
|#

;; the opposite (unused and broken)
(defun color<-GL  (r g b &optional (a 1.0))
  (bind (r1 g1 b1 a1)
      (mapcar (curry #'* 255.0)
	      (list (* r a)
		    (* g a)
		    (* b a)
		    a))
    (list r1 g1 b1 a1)))

;; do not export these two
(defun coord->GL (point &optional (mode :draw))
  (bind (fb x0 y0)
      (ecase mode
	(:draw (list *draw-framebuffer* *draw-origin-x* *draw-origin-y*))
	(:read (list *read-framebuffer* *read-origin-x* *read-origin-y*)))
  (let ((h (framebuffer-height fb))
	(x (first  point))
	(y (second point)))
    (list (+ x x0)
	  (- h
	     (+ y y0))))))

(defun draw-origin (x y)
  (setf *draw-origin-x* x
	*draw-origin-y* y))

(defun shift-draw-origin (dx dy)
  (incf *draw-origin-x* dx)
  (incf *draw-origin-y* dy))

(defun read-origin (x y)
  (setf *read-origin-x* x
	*read-origin-y* y))

(defun shift-read-origin (dx dy)
  (incf *read-origin-x* dx)
  (incf *read-origin-y* dy))

;; operations after draw-framebuffer : composite, draw (with or without stencil), clear+tile, copy, draw traps

;; TODO : will also take GC like options like function, and maybe even plane-mask ?
(defun draw-framebuffer (framebuffer)
  (unless (eql *draw-framebuffer*
	       framebuffer)
    (when (and *draw-framebuffer*
	       (framebuffer-texture *draw-framebuffer*))
      ;;      (gl:Framebuffer-Renderbuffer :draw-framebuffer :color-attachment0 :renderbuffer 0)) ; detach texture from FBO
      (gl:Framebuffer-Texture-2D-ext :draw-framebuffer :color-attachment0 :texture-2d
				     0
				     0)) ; another way to do it
    (gl:Bind-Framebuffer-ext :draw-framebuffer (framebuffer-fbo framebuffer)))
  (aif (framebuffer-texture framebuffer)
       (progn
	 (gl:Draw-Buffer :color-attachment0)
	 ;; TODO : attach these only once at creation 
	 (gl:Framebuffer-Renderbuffer-ext :draw-framebuffer :depth-stencil-attachment :renderbuffer (framebuffer-depth-st-rb framebuffer))  
	 (gl:Framebuffer-Texture-2D-ext :draw-framebuffer :color-attachment0 :texture-2d  
					it
					0)
	 (gl:Clear :stencil-buffer))
       (progn
	 (gl:Draw-Buffer :front-and-back)
	 (gl:Clear :stencil-buffer)))
  (setf *draw-framebuffer*
	framebuffer)
  (setf *draw-origin-x* 0
	*draw-origin-y* 0))

(defun read-framebuffer (framebuffer)
  (unless (eql *read-framebuffer*
	       framebuffer)
    (when (and *read-framebuffer*
	       (framebuffer-texture *read-framebuffer*))
      (gl:Framebuffer-Texture-2D-ext :read-framebuffer :color-attachment0 :texture-2d
				     0
				     0))
    (gl:Bind-Framebuffer-ext :read-framebuffer (framebuffer-fbo framebuffer)))
  (aif (framebuffer-texture framebuffer)
       (progn
	 (gl:Bind-Texture :texture-2d 0)
	 (gl:Framebuffer-Renderbuffer-ext :read-framebuffer :depth-stencil-attachment :renderbuffer (framebuffer-depth-st-rb framebuffer))
	 (gl:Framebuffer-Texture-2D-ext :read-framebuffer :color-attachment0 :texture-2d
					it
					0) ; don't usually need to ?
	 (gl:Read-Buffer :color-attachment0))
       (gl:Read-Buffer :front))
  (setf *read-framebuffer*
	framebuffer)
  (setf *read-origin-x* 0
	*read-origin-y* 0))

(defun add-stencil (&key rect region framebuffer reverse)  ; rect, region OR framebuffer
  "Add stencil HIDING the given zone"
  (when rect
    (setf region
	  (list rect)))
  (when reverse
    (let ((w (framebuffer-width *draw-framebuffer*))
	  (h (framebuffer-height *draw-framebuffer*)))
      (setf region
	    (remove-rects (list 0 0 w h)
			  region))))
  (gl:Stencil-Func :always 1 1)
  (gl:Stencil-Op :replace :replace :replace)
  (gl:Color-Mask nil nil nil nil)
  (gl:Depth-Mask nil)
  (cond (region (dolist (rect region)
		  (apply #'rectangle
			 rect)))
	(framebuffer (with-fields (fbo width height)
			 (framebuffer framebuffer)
		       ;; TODO
		       )))
  (gl:Color-Mask t t t t)
  (gl:Depth-Mask t)
  (gl:Stencil-Func :equal 0 1) 
  (gl:Stencil-Op :keep :keep :keep))

;; zengl:copy-area does BlitFB or Texture quad depending on the nature of the source.
(defun copy-area (w h)
  (with-fields (texture (w0 width) (h0 height))
      (*read-framebuffer* framebuffer)
    (with-fields ((w1 width) (h1 height))
	(*draw-framebuffer* framebuffer)
      (setf w (min (+ w0 *read-origin-x*)
		   (+ w1 *draw-origin-x*)
		   w))
      (setf h (min (+ w0 *read-origin-y*)
		   (+ w1 *draw-origin-y*)
		   h))
      (bind (xr yr)
	  (coord->GL '(0 0) :read)
	(bind (xd yd)
	    (coord->GL '(0 0) :draw)   ; was : (list *draw-origin-x*	     *draw-origin-y*)
	  (if (= 0 (framebuffer-fbo *read-framebuffer*))
	      (%gl:Blit-Framebuffer-ext xr yr
					(+ xr w) (- yr h)
					xd yd
					(+ xd w) (- yd h)
					#x4000 ; :color-buffer-bit
					#x2601)  ;; :linear. not used here anyway
	      (let ((ox (/ (float xr) w0))
		    (oy (/ (float yr) h0))
		    (dx (/ (float w)  w0))
		    (dy (/ (float h)  h0)))
		(gl:Enable :texture-2d)
		(gl:Bind-Texture :texture-2d texture)
;;		(gl:Tex-Env :texture-env :texture-env-mode :replace) ; sloow ! 
		(%gl:Tex-Env-i #x2300 #x2200 #x1E01)
		;; (gl:Tex-Parameter :texture-2d :texture-mag-filter :linear) ; I don't think this is required again
		;; (gl:Tex-Parameter :texture-2d :texture-min-filter :linear)
		(gl:with-primitive :polygon
		  (gl:Tex-Coord  ox         oy        )
		  (gl:Vertex     xd         yd        )
		  
		  (gl:Tex-Coord  (+ ox dx)  oy        )
		  (gl:Vertex     (+ xd w )  yd        )
		  
		  (gl:Tex-Coord  (+ ox dx)  (- oy dy) )
		  (gl:Vertex     (+ xd w )  (- yd h ) )
		  
		  (gl:Tex-Coord  ox         (- oy dy) )
		  (gl:Vertex     xd         (- yd h ) ))
		(gl:Bind-Texture :texture-2d 0)
		(gl:Disable :texture-2d))))))))

(defun points (points &optional color)
  (when color
    (apply #'gl:Color
	   (color->GL color)))
  (gl:with-primitive :points
    (mapc #'(lambda (point)
	      (apply #'gl:Vertex
		     (coord->GL point)))
	  points)))

;; zenGL. points interpreted as alternating segment start, segment end
(defun lines (points &optional color (width 1.0))
  (gl:Line-Width width)
  (when color
    (apply #'gl:Color
	   (color->GL color)))
  (gl:with-primitive :lines
    (mapc #'(lambda (point)
	      (apply #'gl:Vertex
		     (coord->GL point)))
	  points)))

(defun line-strip (points &optional color (width 1.0))
  (gl:Line-Width width)
  (when color
    (apply #'gl:Color
	   (color->GL color)))
  (gl:with-primitive :line-strip
    (mapc #'(lambda (point)
	      (apply #'gl:Vertex
		     (coord->GL point)))
	  points)))

(defun polygon (points &optional color)
  (when color
    (apply #'gl:Color
	   (color->GL color)))
  (gl:with-primitive :polygon
    (mapc #'(lambda (point)
	      (apply #'gl:Vertex
		     (coord->GL point)))
	  points)))

(defun rectangle (x1 y1 x2 y2 &optional color)
  (polygon `((,x1 ,y1)
	     (,x1 ,y2)
	     (,x2 ,y2)
	     (,x2 ,y1))
	   color))

(defun pixels (width height data)
  (with-fields (texture fbo)
      (*draw-framebuffer* framebuffer)
    (bind (xr yr)
	(coord->GL (list 0 height))
      (if (= 0 fbo)
	  (progn
	    (gl:raster-pos xr yr)
	    (gl:draw-pixels width height :rgba :unsigned-byte (image->GL data width height)))  ; FIXME : use quad texture for this too ?
	  (progn
	    (gl:Bind-Texture :texture-2d texture) ; do I need to unbind the texture from the framebuffer ?
	    ;;	  (gl:Tex-Image-2D :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte (image->GL data width height)) ; WRONG !
	    (gl:Tex-Sub-Image-2D :texture-2d 0 xr yr width height :rgba :unsigned-byte (image->GL data width height))
	    (gl:Bind-Texture :texture-2d 0))))))
    
(defun init (fb)  ;  fb is the screen framebuffer
  (let ((w (framebuffer-width fb))
	(h (framebuffer-height fb)))
    (setf %gl:*gl-get-proc-address*
	  *get-proc-address*)
    (gl:Draw-Buffer :front-and-back)
    (gl:Viewport 0 0 w h)
    (gl:Matrix-Mode :projection)
    (gl:Load-Identity)
    (gl:Matrix-Mode :modelview)
    (gl:Load-Identity)
    (gl:Ortho 0 w 0 h -1.0 1.0)  ; useful ?

    (gl:Disable :lighting)
    (gl:Shade-Model :flat)
    (gl:Polygon-Mode :front-and-back :fill)
    (gl:Point-size 1.0)
    (gl:Disable :depth-test)

    (gl:Blend-Func :src-alpha :one-minus-src-alpha) ; used for cursor
    (gl:Disable :blend)

    (gl:Clear-Color 0.0 0.0 0.0 0.0)
    (gl:Clear-Stencil 0)

    (gl:Disable :texture-2d)
    (gl:Enable :stencil-test) ; never disabled
    (gl:Stencil-Func :equal 0 1)   ; pass if stencil = 0
    (gl:Stencil-Op :keep :keep :keep)
    (gl:Clear :color-buffer :stencil-buffer)))

(defun flush ()
  (gl:Flush))
	     




;;----------------------------------------------------------------
;; Bin

;; from cl-glfw : with-begin

;; blend seen as a drawing call ...
;; use as eg (triangles dst color coords :blend (...))
#|
(defun blend (src dst op)
  (gl:with-cap (:blend t :blend-func (ecase op
				       (:clear    '(:zero                :zero))
				       (:in       '(:dst-alpha           :zero))
				       (:out      '(:one-minus-dst-alpha :zero))
				       (:over     '(:one                 :one-minus-src-alpha))
				       (:add      '(:one                 :one ))
				       (:saturate '(:alpha-saturate      :one ))))
    (gl:Raster-Pos dst-x dst-y)
    (gl:Copy-Pixels src-x src-y ; and glBlitFrameBuffer ?
		    w h
		    :color))))


(defmacro with-begin (mode &body body)
  `(progn
     (gl:Begin ,mode)
     (unwind-protect
	  (progn
	    ,@body)
       (gl:End))))

(defmacro with (capability &body body)
  `(progn
     (gl:Enable ,capability)
     (unwind-protect
	  (progn
	    ,@body)
       (gl:Disable ,capability))))

(defmacro withs ((capabilities) &body body)
  `(progn
     (mapcar #'gl:enable
	     ',capabilities)
     (unwind-protect
	  (progn
	    ,@body)
       (mapcar #'gl:disable
	       ',capabilities))))
|#
