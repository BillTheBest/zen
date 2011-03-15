;; (C) 2011 Pierre-Yves Baccou

(require "deps" "deps")

(defvar *encoders* nil)
(defvar *decoders* nil)
(defvar *encoder-generators* nil)
(defvar *decoder-generators* nil)

(load "data/bitfields.lisp") ; for +event-bits+

;; todo : Doc : precise definition of _action_
;; An action here is either a keyword denoting a type (eg :CARD32) or a composite eg (:LIST action1 action2) or ... improve this definition !

;;; ----------------------------------------------------------------
;;; ----------------------------------------------------------------
;;; ----------------------------------------------------------------
;; generators 

(flet ((coder (action *coders* *coder-generators*)
	 (if (atom action)
	     (rest (assoc (make-keyword action)
			  *coders*))
	     (bind (action-generator &rest args)  
		 action
	       (apply (rest (assoc (make-keyword action-generator)
				   *coder-generators*))
		      args)))))
;
(defun decoder (action)
  (coder action
	 *decoders*
	 *decoder-generators*))
;
(defun encoder (action)
  (coder action
	 *encoders*
	 *encoder-generators*))
) ; flet coder

(defmacro defdecoder-generator (name args &body body)
  `(push (cons ',name
	       #'(lambda (,@args)
		   ,@body))
	 *decoder-generators*))

(defdecoder-generator :LIST (action elt-size)
  (let ((dec (decoder action)))   ; note: I like this style, even if a bit redundant, as it implies that the decoder will be applied to many things.
    (lambda (endian seq)
      (mapcar #'(lambda (elt)
		  (funcall dec
			   endian elt))
	      (group seq
		     elt-size)))))

;; The mvbind is because result from ENUM can rightfully be nil (ie :None)
(defdecoder-generator :OR (action1 action2)
  (assert (eql 'ENUM (first action1)))
  (lambda (endian seq)
    (mvbind (result found?)
	(funcall (decoder action1)
		 endian seq)  
      (if found?
	  result
	  (funcall (decoder action2)
		   endian seq)))))
	
;; 2nd value = whether the value was found in the alist.
(defdecoder-generator :ENUM (action alist)
  (let ((dec (decoder action)))
    (lambda (endian seq)
      (aif (assoc (funcall dec
			   endian seq)
		  alist)
	   (values (rest it)
		   t)
	   (values nil
		   nil)))))

;; Still don't like this much
(defdecoder-generator :BITMASK-VALUES (action bm-vals)
  (labels ((rec (endian bitmask bm-vals data)
	     (cond ((null data)
		    nil)
		   ((null bm-vals)
		    (format t "Bitmask-values bad request !~%")
		    nil)
		   (t (bind (bit . (action arg))
			  (first bm-vals)
			(if (= 0 (logand bit 
					 bitmask))
			    (rec endian bitmask
				 (rest bm-vals)
				 data)
			    (list* (make-keyword arg)
				   (funcall (decoder action)
					    endian (first data))
				   (rec endian bitmask
					(rest bm-vals)
					(rest data)))))))))
    (lambda (endian seq)
      (let ((bitmask (funcall (decoder action)
			      endian (subseq seq
					     0
					     4)))
	    (data (group (subseq seq 4)
			 4)))
	(rec endian bitmask bm-vals data)))))

(defdecoder-generator :BITMASK (action bitmasks)
  (let ((dec (decoder action)))
    (lambda (endian seq)
      (let ((bitmask (funcall dec
			      endian seq)))
	(mapcar #'rest
		(remove 0
			bitmasks 
			:key #'(lambda (item) ; item : (bit . name)
				 (logand (first item) 
					 bitmask))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defencoder-generator (name args &body body)
  `(push (cons ',name
	       #'(lambda (,@args)
		   ,@body))
	 *encoder-generators*))

;; return nil if enum not found (useful for (or (enum ...) type) which also exists in replies !
(defencoder-generator :ENUM (action alist)
  (lambda (endian sym)
    (when-let ((value (first (rassoc sym
				     alist))))
      (ENCODE action endian value)))) ; TODO : replace funcalls with EN/DECODEs throughout, like so

(defencoder-generator :LIST (action &optional num) 
  (assert (null num))  ;; The numerical argument should be present only in decoder (request) protos !
  (let ((enc (encoder action)))
    (lambda (endian list)
      (apply #'concatenate
	     'vector
	     (mapcar #'(lambda (val)
			 (funcall enc
				  endian
				  val))
		   list)))))
   
(defencoder-generator :OR (action1 action2)
  (lambda (endian val)
    (or (funcall (encoder action1)
		 endian
		 val)  
	(funcall (encoder action2)
		 endian
		 val))))

;; only used in events
(defencoder-generator :BITMASK (action bitmask-list)
  (let ((enc (encoder action)))
    (lambda (endian keywords)
      (let ((bitmask (apply #'logior
			    (mapcar (compose #'first
					     (rcurry #'rassoc
						     bitmask-list))
				    keywords))))
	(funcall enc
		 endian
		 bitmask)))))


;;; ----------------------------------------------------------------
;;; ----------------------------------------------------------------
;;; ----------------------------------------------------------------
;; coders/decoders


(defmacro defdecoder (name &body body)
  `(push (cons ',name
	       (lambda (endian seq)
		 ,@body))
	 *decoders*))

(defdecoder nil  ; fixme : is this one useful ?
    nil)
(defdecoder '(nil)
    nil)

(defdecoder :BOOL
  (not (zerop (elt seq 0))))

(defdecoder :CARD8
  (elt seq 0))

(defdecoder :CARD16
  (ecase endian
    (:lsb (+ (ash (elt seq 0) 0)
	     (ash (elt seq 1) 8)))
    (:msb (+ (ash (elt seq 1) 0)
	     (ash (elt seq 0) 8)))))

(defdecoder :CARD32
  (ecase endian
    (:lsb (+ (ash (elt seq 0) 0)
	     (ash (elt seq 1) 8)
	     (ash (elt seq 2) 16)
	     (ash (elt seq 3) 24)))
    (:msb (+ (ash (elt seq 3) 0)
	     (ash (elt seq 2) 8)
	     (ash (elt seq 1) 16)
	     (ash (elt seq 0) 24)))))

(defdecoder :ATOM  ; atoms are converted in with-resources-and-atoms
  (dCARD32 endian seq))

(fsetf dCARD8  (decoder :CARD8))
(fsetf dCARD16 (decoder :CARD16))
(fsetf dCARD32 (decoder :CARD32))

; check/test this... 
(defdecoder :INT8
  (let ((i (dCARD8 endian seq)))
    (if (= (ash i -7) 1)
	(- i #x100)
	i)))

(defdecoder :INT16
  (let ((i (dCARD16 endian seq)))
    (if (= (ash i -15) 1)
	(- i #x10000)
	i)))

(defdecoder :INT32
  (let ((i (dCARD32 endian seq)))
    (if (= (ash i -31) 1)
	(- i #x100000000)
	i)))

(defdecoder :CHAR2B
  (dCARD16 :msb seq)) ; always :msb endianness here

(fsetf dINT16  (decoder :INT16))
(fsetf dCHAR2B (decoder :CHAR2B))

(defdecoder :VECTOR
  (list endian seq)) ; in case reordering is needed

(defdecoder :POINT
  (let ((x (dINT16 endian (subseq seq 0 2)))
	(y (dINT16 endian (subseq seq 2 4))))
    (list x y)))

(defdecoder :SEGMENT
  (let ((x1 (dINT16 endian (subseq seq 0 2)))
	(y1 (dINT16 endian (subseq seq 2 4)))
	(x2 (dINT16 endian (subseq seq 4 6)))
	(y2 (dINT16 endian (subseq seq 6 8))))
    (list x1 y1 x2 y2)))

(defdecoder :RECTANGLE
  (let ((x (dINT16 endian (subseq seq 0 2)))
	(y (dINT16 endian (subseq seq 2 4)))
	(w (dCARD16 endian (subseq seq 4 6)))
	(h (dCARD16 endian (subseq seq 6 8))))
    (list x y w h)))

(defdecoder :ARC
  (let ((x (dINT16 endian (subseq seq 0 2)))
	(y (dINT16 endian (subseq seq 2 4)))
	(w (dCARD16 endian (subseq seq 4 6)))
	(h (dCARD16 endian (subseq seq 6 8)))
	(a1 (dINT16 endian (subseq seq 8 10)))
	(a2 (dINT16 endian (subseq seq 10 12))))
    (list x y w h a1 a2)))

(defdecoder :STR
  (let* ((n (elt seq 0))
	 (str (subseq seq
		      1
		      (1+ n))))
    (unless (stringp str)
      (throw 'abort-request 'badstr)) ; FIXME. Internal error or xerror ?
    str))

(defdecoder :STRING8
  (map 'string #'code-char seq))

(defdecoder :STRING16
  (map 'string 
       #'(lambda (v)
	   (code-char (dCHAR2B endian v)))
       (group seq
	      2)))

;(defdecoder :STRING16 (seq l)
;  (if (zero l)
;      nil
;      (cons (CHAR2B endian (subseq seq 0 2))
;	    (STRING16 endian (subseq seq 2) (1- l)))))

; FIXME, if this is useful at all (I don't think so), or REMOVE
;;(defdecoder :HOST
;;  (let* ((family (ENUM8 endian (elt seq 0)))
;;	 (n (dCARD16 endian (subseq seq 2 4)))
;;	 (address (subseq seq 4 (+ 4 n))))
;;    (list family address)))    

(defun codes-to-string (seq)
  (map 'string #'code-char seq))

(defun string-to-codes (str)
  (map 'vector #'char-code str))

;; Only used in request SetFontPath (see the request encoding for better understanding)
(defdecoder :list-STR/k
    (labels ((get-STRs (seq n)
	       (unless (zerop n)
		 (let ((len (dCARD8 endian
				   (elt seq 0))))
		   (cons
		    (codes-to-string (subseq seq
					     1
					     n))
		    (get-STRs (subseq seq (1+ len))
			      (1- n)))))))
      (get-STRs (subseq seq 4)
		(dCARD16 endian (subseq seq 0 2)))))

;; fixme : still used ?
(defdecoder :list-TEXTITEM8
    (labels ((do-list-TEXTITEM8 (endian seq)
	       (when (> (length seq) 2)
		 (let ((len (dCARD8 endian (subseq seq 0 1))))
		   (unless (zerop len)
		     (if (= len 255) ; font-shift indicator ?
			 (cons (dCARD32 ':msb (subseq seq 1 5)) ; always :msb first in font indicator
			       (do-list-TEXTITEM8 endian (subseq seq 5)))
			 (let ((delta (dCARD8 endian (subseq seq 1 2)))
			       (next-textitem8 (codes-to-string (subseq seq  ; todo : is codes-to-string useful ? as we reverse this in the request anyway
									2
									(+ 2 len)))))
			   (cons (list delta next-textitem8)
				 (do-list-TEXTITEM8 endian (subseq seq
								   (+ len 2)))))))))))
      (do-list-TEXTITEM8 endian seq)))

(defdecoder :list-TEXTITEM16
    (labels ((char2b-codes-to-string16 (seq endian)
	       (unless (zerop (length seq))
		 (cons (dCHAR2B endian (subseq seq 0 2))
		       (char2b-codes-to-string16 (subseq seq 2) endian))))
	     (do-list-TEXTITEM16 (endian seq)
	       (when (> (length seq) 3)
		 (let ((len (dCARD16 endian (subseq seq 0 1))))
		    (if (= len 255) ; font-shift indicator ?
			(cons (dCARD32 ':msb (subseq seq 1 5)) ; always :msb first
			      (do-list-TEXTITEM16 endian (subseq seq 5)))
			(let ((delta (dCARD16 endian (subseq seq 1 2)))
			      (next-textitem16 (char2b-codes-to-string16 (subseq seq 2 (+ (* len 2) 2)) endian)))
			  (cons (list delta next-textitem16)
				(do-list-TEXTITEM16 endian (subseq seq
								   (+ len 2))))))))))
      (do-list-TEXTITEM16 endian seq)))

;; Note : the X11 _official_ protocol section about PolyText is full of errors !!
;; pad can be 0, 1, 2 or 3. If pad = 3 the first pad byte must be 0, so that the padding is not interpreted as a TextItem.

;---------------------------------------------------

;; Representation of composite object MUST be (:var1 value1 :var2 value2 ...) because that's what comes out of proto->decoder ; we then don't have to touch it :
(defmacro defdecoder-from-proto (name proto)
  `(push (cons ',name
	       ,(proto->decoder proto))
	 *decoders*))

(defdecoder-from-proto :COLORITEM
 ((4 (CARD32 pixel))
  (2 (CARD16 red))
  (2 (CARD16 green))
  (2 (CARD16 blue))
  (1 ((BITMASK8 ((#x01 . :do-red)
		 (#x02 . :do-green)
		 (#x04 . :do-blue)))
      do-rgb))
  (1 (nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro defencoder (action (arg) &rest body)
  `(push (cons ',action
	       (lambda (endian ,arg)
		 ,@body))
	 *encoders*))

(defencoder :BOOL (val)
  (vector (if val 1 0)))

(defencoder :CARD8 (val)
  (vector val))

(defencoder :CARD16 (val)
  (let ((low (logand #xff
		     val))
	(high (ash (logand #xff00
			   val)
		   -8)))
    (ecase endian
      (:lsb (vector low high))
      (:msb (vector high low)))))

(defencoder :CARD32 (val)
  (let ((byte3 (ash (logand #xff000000 val) -24))
	(byte2 (ash (logand   #xff0000 val) -16))
	(byte1 (ash (logand     #xff00 val) -8))
	(byte0 (ash (logand       #xff val)  0)))
    (ecase endian
      (:lsb (vector byte0 byte1 byte2 byte3))
      (:msb (vector byte3 byte2 byte1 byte0)))))

(fsetf eCARD8  (encoder :CARD8))
(fsetf eCARD16 (encoder :CARD16))
(fsetf eCARD32 (encoder :CARD32))

; check/test this... 
(defencoder :INT8 (val)
  (eCARD8 endian
	  (case (signum val)
	    ((0 1) val)
	    (-1 (+ #x100
		   val)))))

(defencoder :INT16 (val)
  (eCARD16 endian
	   (case (signum val)
	     ((0 1) val)
	     (-1 (+ #x10000
		    val)))))

(defencoder :INT32 (val)
  (eCARD32 endian
	  (case (signum val)
	    ((0 1) val)
	    (-1 (+ #x100000000
		   val)))))

(defencoder :CHAR2B (char)
  (vector (first char)
	  (second char)))

(fsetf eINT16  (encoder :INT16))
(fsetf eCHAR2B (encoder :CHAR2B))

(defencoder :POINT (point)
  (concatenate 'vector
	       (eINT16 endian (first point))
	       (eINT16 endian (second point))))

(defencoder :SEGMENT (segment)
  (concatenate 'vector
	       (eINT16 endian (first segment))
	       (eINT16 endian (second segment))
	       (eINT16 endian (third segment))
	       (eINT16 endian (fourth segment))))

(defencoder :RECTANGLE (rect)
  (concatenate 'vector
	       (eINT16 endian (first rect))
	       (eINT16 endian (second rect))
	       (eCARD16 endian (third rect))
	       (eCARD16 endian (fourth rect))))

(defencoder :ARC (arc)
  (concatenate 'vector
	       (eINT16 endian (first arc))
	       (eINT16 endian (second arc))
	       (eCARD16 endian (third arc))
	       (eCARD16 endian (fourth arc))
	       (eINT16 endian (fifth arc))
	       (eINT16 endian (sixth arc))))

; imagining a macro rassoc/err that will produce the error handling code below when the variable name is "ev"...
; and the macro that defines rassoc/err automatically from rassoc (as well as other .../err functions) !

(defun get-mask (ev)
  (aif (first (rassoc ev
		    +event-bits+))
       it
       (error 'internal-error :str (format nil "Server error : unknown event ~A~%" ev))))

(defencoder :SETofEVENT (events)
  (let ((mask (apply #'+
		     (mapcar #'get-mask 
			     events))))
    (eCARD32 endian mask)))

(defencoder :SETofEVENT/16 (events)
  (let ((mask (apply #'+
		     (mapcar #'get-mask 
			     events))))
    (eCARD16 endian mask)))
  
; host is of the form : (Internet (127 0 0 1))
; FIXME : each host in ListHosts must be padded to 4 bytes
; This function probably doesn't work ? 
#|
(defencoder :HOST (host)
  (let ((family (first host))
	(address (second host)))
    (concatenate 'vector
		 (ENUM8 endian family '((0 . "Internet")
					(1 . "DECnet")
					(2 . "Chaos")))
		 host)))
|#

(defencoder :HOST (host)
  nil)

(defencoder :STRING8 (s)
  (map 'vector #'char-code s))

(defencoder :STR (s)
  (concatenate 'vector
	       (list (length s))
	       (map 'vector #'char-code s)))

(defencoder :VECTOR (seq)
  seq)	    

(defencoder :ATOM (string)
  (acond ((null string)
	  (eCARD32 endian 0))
	 ((intern-atom string t)
	  (eCARD32 endian it))
	 (t (error "Sending back unknown atom ~A~%" string))))


(defencoder :RGB (color)
  (bind (r g b)
      color
    (concatenate 'vector 
		 (eCARD16 endian r)
		 (eCARD16 endian g)
		 (eCARD16 endian b)
		 (eCARD16 endian 0)))) ; unused

;---------------------------------------------------

;; TODO : use this terminology in the docs.

;; encoding _simple objects_ : defencoder.

;; encoding _composite objects_ :  
(defmacro defencoder-from-proto (name proto)
  `(push (cons ',name
	       (lambda (endian composite-arg) ; composite-arg should be a list (:arg1 val1 :arg2 val2 ...)
		 (apply ,(proto->encoder proto)
			endian
			composite-arg)))
	 *encoders*))

(defencoder-from-proto :FORMAT
  ((1 (CARD8 depth))
   (1 (CARD8 bits-per-pixel))
   (1 (CARD8 scanline-pad))
   (5 (nil nil)))) ; unused

(defencoder-from-proto :SCREEN 
  ((4 (WINDOW root))
   (4 (COLORMAP default-colormap))
   (4 (CARD32 white-pixel))
   (4 (CARD32 black-pixel))
   (4 (SETofEVENT current-input-masks))
   (2 (CARD16 width-in-pixels))
   (2 (CARD16 height-in-pixels))
   (2 (CARD16 width-in-millimeters))
   (2 (CARD16 height-in-millimeters))
   (2 (CARD16 min-installed-maps))
   (2 (CARD16 max-installed-maps))
   (4 (VISUALID root-visual))
   (1 ((ENUM CARD8 ((0 . :Never)
		    (1 . :WhenMapped)
		    (2 . :Always)))
       backing-stores))
   (1 (BOOL save-unders))
   (1 (CARD8 root-depth))
   (1 (CARD8 (length allowed-depths)))
   (nil ((list DEPTH) allowed-depths))))  ; (n is always a multiple of 4))

(defencoder-from-proto :DEPTH
  ((1 (CARD8 depth))
   (1 (nil nil)) ; unused
   (2 (COUNT16 (length visuals)))
   (4 (nil nil)) ; unused
   (nil ((list VISUALTYPE) visuals))))

(defencoder-from-proto :VISUALTYPE
  ((4 (VISUALID visual-id))
   (1 ((ENUM CARD8 ((0 . :StaticGray)
		    (1 . :GrayScale)
		    (2 . :StaticColor)
		    (3 . :PseudoColor)
		    (4 . :TrueColor)
		    (5 . :DirectColor)))
       class))
   (1 (CARD8 bits-per-rgb-value))
   (2 (CARD16 colormap-entries))
   (4 (CARD32 red-mask))
   (4 (CARD32 green-mask))
   (4 (CARD32 blue-mask))
   (4 (nil nil)))) ; unused


(defencoder-from-proto :FONTPROP
  ((4 (ATOM name))
   (4 (LIST BYTE) value)))

(defencoder-from-proto :CHARINFO
  ((2 (INT16 left-side-bearing))
   (2 (INT16 right-side-bearing))
   (2 (INT16 character-width))
   (2 (INT16 ascent))
   (2 (INT16 descent))
   (2 (CARD16 attributes))))
   
;---------------------------------------------------------------------
;---------------------------------------------------------------------	
;---------------------------------------------------------------------
; Aliases are for both encoder and decoder.

(labels ((alias (new name)
	   (push (cons new
		       (rest (assoc name
				    *decoders*)))
		 *decoders*)
	   (push (cons new
		       (rest (assoc name
				    *encoders*)))
		 *encoders*))
	 (aliases (aliases name)
	   (dolist (al aliases)
	     (alias al
		    name))))

; note ENUM8/4 = ENUM8 but with a 4 byte argument (read first byte). Happens to be same fn as ENUM8
; this also applies to the rest of the /4's.

  (aliases '(:KEYCODE :KEYCODE/4 :BUTTON :UINT8 :COUNT8 :BYTE :ENUM8/4 :CARD8/4)
	   :CARD8)
  (aliases '(:COUNT16 :UINT16 :CARD16/4)
	   :CARD16)
  (aliases '(:WINDOW :PIXMAP :CURSOR :FONT :GCONTEXT :COLORMAP
	     :DRAWABLE :FONTABLE :VISUALID :VALUE
	     :TIMESTAMP :KEYSYM :PICTURE :PICTFORMAT :GLYPHSET 
	     :GLYPH :COUNT32 :UINT32)
	   :CARD32)
  (alias :INT8/4 :INT8)
  (alias :INT16/4 :INT16)
  (alias :BOOL/4 :BOOL))


; FIX ? doesn't look right
#|
(defdecoder :FIXED
"explain FIXED"
  (let ((val (CARD32 endian seq)))
    (+ (ash val 16)
       (/ (logand ash #xffff) #xffff))))
|#
