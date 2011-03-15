;; (C) 2011 Pierre-Yves Baccou

;; Do not waste time reading _this_ code ... it was put together in a hurry.
;; A modern X server shouldn't be supporting core fonts anyway.

;; perhaps a separate font thread to do all this regexp matching without interrupting the main thread ?
(require "deps" "deps")
(use-package 'cffi)

(require 'cl-ppcre)

(load "util/freetype.lisp")
(load "data/font-aliases.lisp")

;; (defconstant +font-dir+ "/usr/share/fonts/X11") ; unused here (put it in find-all-fontdirs.sh)

(defvar *fonts* nil)  ; list of (pattern . file)
(defvar *font-library* nil)

(defstruct glyph
  width ; glyphslot->advance or metrics->horiAdvance ?
  lbearing ; glyphslot->bitmap_left or metrics->horiBearingX ? 
  rbearing ; lbearing +  metrics->width
  ascent  ; aka glyphslot->bitmap_top or metrics->horiBearingY ? 
  descent ; ascent +  metrics->height
  bmp-width
  bmp-rows ; should be = ascent + descent
  bmp-pitch
  bitmap)

(defclass- font (xresource)
  (glyphs (make-array 256)) ; array of 256, not all occupied
  min-char ; where to start in glyphs
  max-char ; where to end
  default-char ; as in index in glyphs
  ascent  ; face ascent, descent used to determine vertical spacing
  descent
  height  ; in Freetype height = ascent + descent + gap
  min-bounds ; a glyph without bitmap
  max-bounds)

(defun font-init ()
  (find-all-fonts)
  (setf *font-library*
	(foreign-alloc :pointer))
  (FT_Init_Freetype *font-library*))

(defun split-string-once (string char)
  (let ((p (position char string)))
    (list (subseq string 0 p)
	  (subseq string (1+ p)))))

(defun find-fonts (fontdir-file)
  (let ((dir (namestring (make-pathname :directory (pathname-directory (pathname fontdir-file)))))) ; all this just to remove the file name at the end.
    (with-open-file (str fontdir-file :direction :input)
      (read-line str nil nil) ; skip first line
      (do ((line (read-line str nil nil) (read-line str nil nil)))
	  ((null line) nil)
	(bind (file pat)
	    (split-string-once line #\Space)
	  (push (cons (first (split-string-once file #\.))
		      (concatenate 'string dir file))
		*fonts*)
	  (push (cons pat
		      (concatenate 'string dir file))
		*fonts*))))))

(defun find-fonts (fontdir-file)
  (let ((dir (namestring (make-pathname :directory (pathname-directory (pathname fontdir-file)))))) ; all this just to remove the file name at the end.
    (with-open-file (str fontdir-file :direction :input)
      (read-line str nil nil) ; skip first line
      (do ((line (read-line str nil nil) (read-line str nil nil)))
	  ((null line) nil)
	(bind (file pat)
	    (split-string-once line #\Space)
	  (push (cons pat
		      (concatenate 'string dir file))
		*fonts*))))))

(defun find-all-fonts ()
  (let ((str (sb-ext:process-output (sb-ext:run-program "./find-all-fontdirs.sh" nil :output :stream))))
    (loop
       (aif (read-line str nil nil)
	    (find-fonts it)
	    (return)))
    (dolist (alias +font-aliases+)
      (bind (name pat)
	  alias
	(push (cons name
		    (rest (assoc pat 
				 *fonts*
				 :test #'string=)))
	      *fonts*)))))
    
(defun regexify (pattern)
  "Convert * to .* and ? to . in pattern"
  (setf pattern
	(cl-ppcre:regex-replace-all "\\*" pattern "\.*"))
  (setf pattern
	(cl-ppcre:regex-replace-all "\\?" pattern ".")))

(defun list-fonts (pat &optional max)
  (let ((scanner (cl-ppcre:create-scanner (regexify pat) :case-insensitive-mode t))
	results)
    (do ((fonts *fonts* (cdr fonts)))
	((or (null fonts)
	     (and max (= 0 max)))
	 (nreverse results))
      (let ((font (first fonts)))
	(when (cl-ppcre:scan scanner (first font))
	  (when max
	    (decf max))
	  (push font
		results))))))


;-----

(defun pad8 (n)
  (* 8 
     (ceiling n 8)))

(defun lognot8 (n)
  (logand #xff
	  (lognot n)))


(defun slow-copy-foreign-array (pointer type length)
  (let ((arr (make-array length)))
    (dotimes (i length)
      (setf (aref arr i)
	    (mem-aref pointer type i)))
    arr))

;; missing in cffi
(defun copy-foreign-array (pointer type length)
  (if (eql :uint8 type)
      (let ((arr (make-array length :element-type 'unsigned-byte)))
	(dotimes (i length)
	  (setf (svref arr i)
		(mem-aref pointer :uint8 i)))
	arr)
      (let ((arr (make-array length)))
	(dotimes (i length)
	  (setf (aref arr i)
		(mem-aref pointer type i)))
	arr)))

(defun load-glyph (ft-glyph)
  (with-foreign-slots ((metrics bitmap) ft-glyph FT_GlyphSlotRec)
    (with-foreign-slots ((horiAdvance horiBearingX horiBearingY width height) metrics FT_Glyph_Metrics)
      (bind (horiAdvance horiBearingX horiBearingY width height)
	  (mapcar (rcurry #'/ 64)
		  (list horiAdvance horiBearingX horiBearingY width height))
	(with-foreign-slots ((pitch rows buffer) bitmap FT_Bitmap)
	  (make-glyph :width horiAdvance ; width in the sense of X (= advance)
		      :lbearing horiBearingX
		      :rbearing (+ horiBearingX width) ; width in the sense of freetype here
		      :ascent horiBearingY
		      :descent (- height horiBearingY)
		      :bmp-width (foreign-slot-value bitmap 'FT_Bitmap 'width)
		      :bmp-pitch pitch ; should be same as bmp-width/8 ?
		      :bmp-rows rows
		      :bitmap (copy-foreign-array buffer :uint8 (* pitch rows))))))))

;; font rendering : use one texture for a whole text item. Prepare it by drawing the glyphs with tex-image (type :bitmap)
(macrolet ((%max-in-glyphs (f)
	     `(reduce #'max
		      (map 'vector ,f glyphs)))
	   (%min-in-glyphs (f)
	     `(reduce #'min
		      (map 'vector ,f glyphs))))
;  
(defun freetype-load-face (ft-face)
  (let ((font (make-instance 'font)))
    (with-fields (glyphs max-bounds min-bounds ascent descent height default-char max-char min-char) 
	(font font)
      (dotimes (i 256)
	(FT_Load_Char ft-face i 0)
	(setf (aref glyphs i)
	      (load-glyph (foreign-slot-value ft-face 'FT_FaceRec 'glyph))))
      (setf max-bounds
	    (make-glyph :width (%max-in-glyphs #'glyph-width)
			:lbearing (%max-in-glyphs #'glyph-lbearing)
			:rbearing (%max-in-glyphs #'glyph-rbearing)
			:ascent (%max-in-glyphs #'glyph-ascent)
			:descent (%max-in-glyphs #'glyph-descent)))
      (setf min-bounds
	    (make-glyph :width (%min-in-glyphs #'glyph-width)
			:lbearing (%min-in-glyphs #'glyph-lbearing)
			:rbearing (%min-in-glyphs #'glyph-rbearing)
			:ascent (%min-in-glyphs #'glyph-ascent)
			:descent (%min-in-glyphs #'glyph-descent)))
;      (let* ((available-sizes (foreign-slot-value ft-face
;						  'FT_FaceRec 'available_sizes)))
;	nil)
      (setf height (foreign-slot-value (foreign-slot-value ft-face ; ft-face->available_sizes[0]->height
							   'FT_FaceRec 'available_sizes)
				       'FT_Bitmap_Size 'height)
	    min-char 0
	    max-char 255
	    default-char 0
	    descent (glyph-descent max-bounds)
	    ascent (glyph-ascent max-bounds)))
    font))
;
(defmethod text-extents (text (font font))
  (let ((glyphs (map 'vector
		     #'(lambda (char)
			 (elt (font-glyphs font)
			      (char-code char)))
		     text)))
    (let ((overall-ascent (%max-in-glyphs #'glyph-ascent))
	  (overall-descent (%max-in-glyphs #'glyph-descent))
	  (overall-width (reduce #'+
				 glyphs
				 :key #'glyph-width))
	  (overall-left (glyph-lbearing (elt (font-glyphs font)
					     (char-code (aref text
							      0)))))
	  (overall-right (glyph-rbearing (elt (font-glyphs font)
					      (char-code (aref text
							       (1- (length text))))))))
      (values (font-ascent font) (font-descent font) overall-width overall-ascent overall-descent overall-left overall-right))))
;
) ; macro max/%min-in-glyphs

(defmethod text-extents (string (gc gc))
  (text-extents string (gc-font gc)))

(defmethod text-extents (string obj)
  (xerror :font))

;; There is an y inversion to make a GL texture out of this
(defun insert-glyph (glyph data x y text-width text-height fg bg)
  "x,y is wrt the upper left corner of data"
  (with-fields (width lbearing rbearing ascent descent bmp-width bmp-rows bmp-pitch bitmap)
      (glyph glyph)
    (let ((start-x (+ x lbearing))
	  (start-y (- (1- text-height) (- y ascent)))
	  (pixmap (bitmap-to-pixmap bitmap bmp-width bmp-rows (* 8 bmp-pitch) :right (pixel-color fg) (pixel-color bg))))
      (dotimes (j bmp-rows)
	(dotimes (i bmp-width)
	  (let ((from (* 4
			 (+ i
			    (* bmp-width
			       j))))
		(to (* 4 
		       (+ start-x
			  i
			  (* text-width
			     (- start-y j))))))
	    (dotimes (k 4)
	      (setf (aref data (+ to k))
		    (aref pixmap (+ from k))))))))
    (+ x
       width)))

(let ((%pfont (foreign-alloc :pointer)))
;
(defun freetype-open-face (file)
  (let ((retval (FT_New_Face (mem-aref *font-library* :pointer) file 0 %pfont)))
    (mem-aref %pfont :pointer)))
;
)

;; fixme : adjust text-width for extra overall-left, overall-right
;; x, y = baseline starting position
(defun text-item (text x y gc mode)
  (with-slots (font foreground background)
      gc
    (mvbind (font-ascent font-descent text-width ascent)
	(text-extents text font)
      (let* ((height (+ font-ascent font-descent))
	     (data (make-array (* 4
				  text-width
				  height) 
			       :initial-element 0)) ; fixme : rgba is wasteful here
	     (glyphs (font-glyphs font)))
	(let ((xd 0))
	  (map nil #'(lambda (char)
		       (setf xd
			     (insert-glyph (elt glyphs (char-code char))
					   data
					   xd
					   font-ascent
					   text-width
					   height
					   foreground
					   background)))
	       text))
	(decf y
	      ascent) ; baseline coord -> corner coord
	(when (eql :ImageText
		   mode)
	  (zengl:rectangle x
			   y
			   (+ x text-width)
			   (+ y height)
			   (pixel-color background)))
	(gl:Enable :texture-2d)
	(gl:Bind-Texture :texture-2d 0)
	(gl:Tex-Parameter :texture-2d :texture-mag-filter :linear)
	(gl:Tex-Parameter :texture-2d :texture-min-filter :linear)
	(gl:Tex-Env :texture-env :texture-env-mode :add)
	(gl:Tex-Image-2D :texture-2d 0 :rgba text-width height 0 :rgba :unsigned-byte data) ; Wanted to use a pure alpha texture  but it's deprecated in GL 3.0 ! 
	(gl:Enable :blend)
	(gl:Blend-Func :src-alpha :one-minus-src-alpha)
	(apply #'gl:Color
	       (zengl::color->GL (pixel-color foreground)))
	(gl:with-primitive :polygon
	  (gl:Tex-Coord 0 1)
	  (apply #'gl:Vertex
		 (zengl::coord->GL (list x
					 y)))
	  
	  (gl:Tex-Coord 1 1) 
	  (apply #'gl:Vertex
		 (zengl::coord->GL (list (+ x text-width)
					 y)))
	  
	  (gl:Tex-Coord 1 0)
	  (apply #'gl:Vertex
		 (zengl::coord->GL (list (+ x text-width)
					 (+ y height))))
	  
	  (gl:Tex-Coord 0 0)
	  (apply #'gl:Vertex
		 (zengl::coord->GL (list x
					 (+ y height)))))
	(gl:Bind-Texture :texture-2d 0)
	(gl:Disable :blend)
	(gl:Disable :texture-2d)
	text-width))))
  
(defrequest/draw :ImageText8  ((drawable drawable) (gc gc) x y string)
  (text-item string x y gc :ImageText))

(defrequest/draw :ImageText16 ((drawable drawable) (gc gc) x y string)
  (text-item string x y gc :ImageText))

(defrequest/draw :PolyText8 ((drawable drawable) (gc gc) x y items)
  (dolist (item items)
    (cond ((and (numberp item)
		(eql 'font
		     (find-res (class-of item))))
	   (setf (gc-font gc)
		 item))
	  ((numberp item)
	   (xerror :font))
	  (t (bind (delta string)
		 item
	       (incf x
		     delta)
	       (incf x
		     (text-item string x y gc :PolyText))))))) ; text-item returns effective text width

(defrequest/draw :PolyText16 ((drawable drawable) (gc gc) x y items)
  (enqueue-request :PolyText8 :drawable drawable :gc gc :x x :y y :items items))

(defrequest :QueryTextExtents (font string) ; font can be a GC !
  (mvbind (font-ascent font-descent overall-width overall-ascent overall-descent overall-left overall-right)
      (text-extents string
		    (find-res font))
    (reply :draw-direction :LeftToRight
	   :overall-ascent overall-ascent :overall-descent overall-descent :overall-width overall-width :overall-left overall-left :overall-right overall-right :font-ascent font-ascent :font-descent font-descent)))

(defun char-info (glyph)
  (with-fields (width lbearing rbearing ascent descent bmp-width bmp-rows bmt-pitch bitmap)
      (glyph glyph)
    (list :left-side-bearing lbearing :right-side-bearing rbearing :character-width width :ascent ascent :descent descent :attributes 0)))

(defun font-info (font)
  (with-fields (glyphs max-bounds min-bounds ascent descent height min-char max-char default-char)
      (font font)
    (list :draw-direction :LeftToRight :min-byte1 0 :max-byte1 0 :min-char-or-byte2 min-char :max-char-or-byte2 max-char 
	  :all-chars-exist t :default-char default-char :min-bounds (char-info min-bounds) :max-bounds (char-info max-bounds) :font-ascent ascent :font-descent descent :properties nil)))

;; font is a font or a GC
(defrequest :QueryFont ((font font))
  (apply #'reply 
	 :char-infos (map 'list
			  #'char-info 
			  (font-glyphs font))
	 (font-info font)))

;; this one a special because it sends multiple replies !
(let* ((empty-charinfo (list :left-side-bearing 0 :right-side-bearing 0 :character-width 0 :ascent 0 :descent 0 :attributes 0))
       (empty-fontinfo (list :draw-direction :LeftToRight :min-byte1 0 :max-byte1 0 :min-char-or-byte2 0 :max-char-or-byte2 0 :all-chars-exist t :default-char 0 :min-bounds empty-charinfo :max-bounds empty-charinfo :font-ascent 0 :font-descent 0 :properties nil)))
;
(defrequest :ListFontsWithInfo (pattern max-names)
  (handler-bind
      ((xreply #'(lambda (xreply)
		   (send :ListFontsWithInfo
			 *client*
			 (list* :sequence-number seqnum
				(mapcar #'xresource-id
					(xcondition-args xreply)))))))
    (dolist (font-path (list-fonts pattern max-names))
      (let ((font (freetype-load-face (freetype-open-face (rest font-path)))))
	(apply #'reply 
	       :name (first font-path)
	       :replies-hint 0
	       (font-info font))))
    (apply #'reply
	   :name ""
	   :replies-hint 0
	   empty-fontinfo)))   
;
) ; empty charinfo, fontinfo
     
(defrequest :ListFonts (pattern max-names)
  (reply :names (mapcar #'first
			(list-fonts pattern max-names))))

(defrequest :GetFontPath ()
  (reply :path '("/usr/share/fonts/X11")))

(defrequest :SetFontPath (path)
  ;; do nothing
  )

(defrequest :OpenFont (fid name)
  (let ((fonts (list-fonts name 1)))
    (if (null fonts)
	(xerror :Name)
	(let* ((font (first fonts))
	       (font (freetype-load-face (freetype-open-face (rest font)))))
	  (create-res fid
		      font)))))

(defrequest :CloseFont ((font font))
  (destroy-res font) ; fixme : only destroy when no GC is using it
  )
