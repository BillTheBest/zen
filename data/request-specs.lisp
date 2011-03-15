;; (C) 2011 Pierre-Yves Baccou

;; TODO : read syntax. put markers between {}'s so that {expr} is expanded to {expr :temp? t}
(eval-when (:compile-toplevel :load-toplevel :execute)
;
(require "util" "util/util")
;
(enable-braces '(nil nil))
);

(load "data/bitfields.lisp")

(defmacro interpret-requests (&rest protos)
  (cons 'progn
	(mapcar #'(lambda (proto)
		    `(deftranslator/request ,(first proto) ,@(rest proto)))
		protos)))

(interpret-requests
;
(:CreateWindow
 (1 {opcode} 1)
 (1 (CARD8 depth))
 (2 {request-length})
 (4 (WINDOW wid))
 (4 (WINDOW parent))
 (2 (INT16 x))
 (2 (INT16 y))
 (2 (CARD16 width))
 (2 (CARD16 height))
 (2 (CARD16 border-width))
 (2 ((ENUM CARD16 ((0 . :CopyFromParent)
		   (1 . :InputOutput)
		   (2 . :InputOnly)))
     class))
 (4 ((or (ENUM CARD32 ((0 . :CopyFromParent)))
	 VISUALID)
     visual))
 (nil ((BITMASK-VALUES CARD32 #.+createwindow-mask-values+)
       value-list)))

(:ChangeWindowAttributes
 (1 {opcode} 2)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window))
 (nil ((BITMASK-VALUES CARD32 #.+createwindow-mask-values+)
       value-list)))

(:GetWindowAttributes
 (1 {opcode} 3)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:DestroyWindow
 (1 {opcode} 4)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:DestroySubwindows
 (1 {opcode} 5)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:ChangeSaveSet
 (1 {opcode} 6)
 (1 ((ENUM CARD8 ((0 . :Insert)
		  (1 . :Delete)))
     mode))
 (2 {request-length})
 (4 (WINDOW window)))

(:ReparentWindow
 (1 {opcode} 7)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window))
 (4 (WINDOW new-parent))
 (2 (INT16 new-x))
 (2 (INT16 new-y)))

(:MapWindow
 (1 {opcode} 8)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:MapSubwindows
 (1 {opcode} 9)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:UnmapWindow
 (1 {opcode} 10)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:UnmapSubwindows
 (1 {opcode} 11)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:ConfigureWindow
 (1 {opcode} 12)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window))
 (nil ((BITMASK-VALUES CARD16  ; like BITMASK-VAL32 only reads 1st 2 bytes of the 4 byte sequence.	(ie (CARD16) !)
	((#x0001 . (INT16/4 x))
	 (#x0002 . (INT16/4 y))
	 (#x0004 . (CARD16/4 width))
	 (#x0008 . (CARD16/4 height))
	 (#x0010 . (CARD16/4 border-width))
	 (#x0020 . (WINDOW sibling))
	 (#x0040 . ((ENUM CARD8 ((0 . :Above)
				 (1 . :Below)
				 (2 . :TopIf)
				 (3 . :BottomIf)
				 (4 . :Opposite))) stack-mode))))
       value-list)))

(:CirculateWindow
 (1 {opcode} 13)
 (1 ((ENUM CARD8 ((0 . :RaiseLowest)
		  (1 . :LowerHighest)))
     direction))
 (2 {request-length})
 (4 (WINDOW win)))

(:GetGeometry
 (1 {opcode} 14)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable)))

(:QueryTree
 (1 {opcode} 15)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:InternAtom
 (1 {opcode} 16)
 (1 (BOOL only-if-exists))
 (2 {request-length})
 (2 (COUNT16 n :temp? t)) ; (n length of name)
 (2 {unused})
 (n (STRING8 name))
 ((pad n) {unused}))

(:GetAtomName
 (1 {opcode} 17)
 (1 {unused})
 (2 {request-length})
 (4 (ATOM atom)))

;; would like to be able to write here :(flet (bla (l)) (ChangeProperty (1 {opcode} 18) (...
;; TODO : recheck this with xtrace eventually, it's a complex one.
(:ChangeProperty
 (1 {opcode} 18)
 (1 ((ENUM CARD8 ((0 . :Replace)
		  (1 . :Prepend)
		  (2 . :Append)))
     mode))
 (2 {request-length})
 (4 (WINDOW window))
 (4 (ATOM name))
 (4 (ATOM type))
 (1 (CARD8 format))
 (3 {unused})
 (4 (CARD32 l :temp? t)) ;  (CARD32 length of data in format units)) ;
 ;; (= n for format = 8)
 ;; (= n/2 for format = 16)
 ;; (= n/4 for format = 32)
 ((ecase format
    (8 l)
    (16 (* l 2))
    (32 (* l 4)))
;  ((list BYTE 1) data))
  (VECTOR data))
 ;; (n) is a multiple of 2 for format = 16)
 ;; (n is a multiple of 4 for format = 32)
 ((pad (ecase format  ; there must be a way not to repeat this twice (FIXME !)
	 (8 l)
	 (16 (* l 2))
	 (32 (* l 4))))
  {unused}))

(:DeleteProperty
 (1 {opcode} 19)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window))
 (4 (ATOM name)))

(:GetProperty
 (1 {opcode} 20)
 (1 (BOOL delete))
 (2 {request-length})
 (4 (WINDOW window))
 (4 (ATOM name))
 (4 ((or (ENUM CARD32 ((0 . :AnyPropertyType)))
	 ATOM)
     type))
 (4 (CARD32 long-offset))
 (4 (CARD32 long-length)))

(:ListProperties
 (1 {opcode} 21)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:SetSelectionOwner
 (1 {opcode} 22)
 (1 {unused})
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 WINDOW)
     owner))
 (4 (ATOM sel))
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time)))

(:GetSelectionOwner
 (1 {opcode} 23)
 (1 {unused})
 (2 {request-length})
 (4 (ATOM sel)))

(:ConvertSelection
 (1 {opcode} 24)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW requestor))
 (4 (ATOM sel))
 (4 (ATOM target))
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 ATOM)
     property))
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time)))

(:SendEvent
 (1 {opcode} 25)
 (1 (BOOL propagate))
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . :PointerWindow)
		       (1 . :InputFocus)))
	 WINDOW)
     destination))
 (4 (#.SETofEVENT event-mask))
 (32 (VECTOR event))) ; encoded event

(:GrabPointer
 (1 {opcode} 26)
 (1 (BOOL owner-events))
 (2 {request-length})
 (4 (WINDOW grab-window))
 (2 (#.SETofPOINTEREVENT event-mask))
 (1 ((ENUM CARD8 ((0 . :Synchronous)
		  (1 . :Asynchronous)))
     pointer-mode))
 (1 ((ENUM CARD8 ((0 . :Synchronous)
		  (1 . :Asynchronous)))
     keyboard-mode))
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 WINDOW)
     confine-to))
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 CURSOR)
     cursor))
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time)))

(:UngrabPointer
 (1 {opcode} 27)
 (1 {unused})
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time)))

(:GrabButton
 (1 {opcode} 28)
 (1 (BOOL owner-events))
 (2 {request-length})
 (4 (WINDOW grab-window))
 (2 (#.SETofPOINTEREVENT event-mask))
 (1 ((ENUM CARD8 ((0 . :Synchronous)
		  (1 . :Asynchronous)))
     pointer-mode))
 (1 ((ENUM CARD8 ((0 . :Synchronous)
		  (1 . :Asynchronous)))
     keyboard-mode))
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 WINDOW)
     confine-to))
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 CURSOR)
     cursor))
 (1 ((or (ENUM CARD8 ((0 . :AnyButton)))
	 BUTTON)
     button))
 (1 {unused})
 (2 ((or (ENUM CARD16 ((#x8000 . :AnyModifier)))
	 #.SETofKEYMASK)
     modifiers)))

(:UngrabButton
 (1 {opcode} 29)
 (1 ((or (ENUM CARD8 ((0 . :AnyButton)))
	 BUTTON)
     button))
 (2 {request-length})
 (4 (WINDOW grab-window))
 (2 ((or (ENUM CARD16 ((#x8000 . :AnyModifier)))
	 #.SETofKEYMASK)
     modifiers))
 (2 {unused}))

(:ChangeActivePointerGrab
 (1 {opcode} 30)
 (1 {unused})
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 CURSOR)
     cursor))
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time))
 (2 (#.SETofPOINTEREVENT event-mask))
 (2 {unused}))

(:GrabKeyboard
 (1 {opcode} 31)
 (1 (BOOL owner-events))
 (2 {request-length})
 (4 (WINDOW grab-window))
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time))
 (1 ((ENUM CARD8 ((0 . :Synchronous)
		  (1 . :Asynchronous)))
     pointer-mode))
 (1 ((ENUM CARD8 ((0 . :Synchronous)
		  (1 . :Asynchronous)))
     keyboard-mode))
 (2 {unused}))

(:UngrabKeyboard
 (1 {opcode} 32)
 (1 {unused})
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time)))

(:GrabKey
 (1 {opcode} 33)
 (1 (BOOL owner-events))
 (2 {request-length})
 (4 (WINDOW grab-window))
 (2 ((or (ENUM CARD16 ((#x8000 . :AnyModifier)))
	 #.SETofKEYMASK)
     modifiers))
 (1 ((or (ENUM CARD8 ((0 . :AnyKey)))
	 KEYCODE)
     key))
 (1 ((ENUM CARD8 ((0 . :Synchronous)
		  (1 . :Asynchronous)))
     pointer-mode))
 (1 ((ENUM CARD8 ((0 . :Synchronous)
		  (1 . :Asynchronous)))
     keyboard-mode))
 (3 {unused}))

(:UngrabKey
 (1 {opcode} 34)
 (1 ((or (ENUM CARD8 ((0 . :AnyKey)))
	 KEYCODE)
     key))
 (2 {request-length})
 (4 (WINDOW grab-window))
 (2 ((or (ENUM CARD16 ((#x8000 . :AnyModifier)))
	 #.SETofKEYMASK)
     modifiers))
 (2 {unused}))

(:AllowEvents
 (1 {opcode} 35)
 (1 ((ENUM CARD8 ((0 . :AsyncPointer)
		  (1 . :SyncPointer)
		  (2 . :ReplayPointer)
		  (3 . :AsyncKeyboard)
		  (4 . :SyncKeyboard)
		  (5 . :ReplayKeyboard)
		  (6 . :AsyncBoth)
		  (7 . :SyncBoth)))
     mode))
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time)))

(:GrabServer
 (1 {opcode} 36)
 (1 {unused})
 (2 {request-length}))

(:UngrabServer
 (1 {opcode} 37)
 (1 {unused})
 (2 {request-length}))

(:QueryPointer
 (1 {opcode} 38)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:GetMotionEvents
 (1 {opcode} 39)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window))
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     start))
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     stop)))

(:TranslateCoordinates
 (1 {opcode} 40)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW src-window))
 (4 (WINDOW dst-window))
 (2 (INT16 src-x))
 (2 (INT16 src-y)))

(:WarpPointer
 (1 {opcode} 41)
 (1 {unused})
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 WINDOW)
     src-window))
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 WINDOW)
     dst-window))
 (2 (INT16 src-x))
 (2 (INT16 src-y))
 (2 (CARD16 src-width))
 (2 (CARD16 src-height))
 (2 (INT16 dst-x))
 (2 (INT16 dst-y)))

(:SetInputFocus
 (1 {opcode} 42)
 (1 ((ENUM CARD8 ((0 . nil)
		  (1 . :PointerRoot)
		  (2 . :Parent)))
     revert-to))
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . nil)
		       (1 . :PointerRoot)))
	 WINDOW)
     focus))
 (4 ((or (ENUM CARD32 ((0 . :CurrentTime)))
	 TIMESTAMP)
     time)))

(:GetInputFocus
 (1 {opcode} 43)
 (1 {unused})
 (2 {request-length}))

(:QueryKeymap
 (1 {opcode} 44)
 (1 {unused})
 (2 {request-length}))

(:OpenFont
 (1 {opcode} 45)
 (1 {unused})
 (2 {request-length})
 (4 (FONT fid))
 (2 (COUNT16 n :temp? t)) ; (n length of name))
 (2 {unused})
 (n (STRING8 name))
 ((pad n) {unused}))

(:CloseFont
 (1 {opcode} 46)
 (1 {unused})
 (2 {request-length})
 (4 (FONT font)))

(:QueryFont
 (1 {opcode} 47)
 (1 {unused})
 (2 {request-length})
 (4 (FONTABLE font)))

;; Complicated, take the string +variable padding as a list of bytes
;; and only extract the string in the QueryTextExtents implementation
(:QueryTextExtents
 (1 {opcode} 48)
 (1 (BOOL odd-length)) ; padding = 0 or 2. True if padding = 2
 (2 {request-length})
 (4 (FONTABLE font))
 (nil ((list CARD8 1) string/pad))) ; STRING16 + padding as above

(:ListFonts
 (1 {opcode} 49)
 (1 {unused})
 (2 {request-length})
 (2 (CARD16 max-names))
 (2 (COUNT16 n :temp? t))   ; length of pattern))
 (n (STRING8 pattern))
 ((pad n) {unused}))

(:ListFontsWithInfo
 (1 {opcode} 50)
 (1 {unused})
 (2 {request-length})
 (2 (CARD16 max-names))
 (2 (COUNT16 n :temp? t))   ; length of pattern))
 (n (STRING8 pattern))
 ((pad n) {unused}))

(:SetFontPath
 (1 {opcode} 51)
 (1 {unused})
 (2 {request-length})
 (nil (list-STR/k path))) ; includes k and 2 {unused} (as below)
;; (2 (CARD16) k) ; (CARD16 number of STRs in path))
;; (2 {unused})
;; (n ((list-STR) path))
;; ((pad n) {unused}))

(:GetFontPath
 (1 {opcode} 52)
 (1 {unused})
 (2 {request-length}))

(:CreatePixmap
 (1 {opcode} 53)
 (1 (CARD8 depth))
 (2 {request-length})
 (4 (PIXMAP pid))
 (4 (DRAWABLE drawable))
 (2 (CARD16 width))
 (2 (CARD16 height)))

(:FreePixmap
 (1 {opcode} 54)
 (1 {unused})
 (2 {request-length})
 (4 (PIXMAP pixmap)))

(:CreateGC
 (1 {opcode} 55)
 (1 {unused})
 (2 {request-length})
 (4 (GCONTEXT cid))
 (4 (DRAWABLE drawable))
 (nil ((BITMASK-VALUES CARD32 #.+createGC-mask-values+)
       value-list)))

(:ChangeGC
 (1 {opcode} 56)
 (1 {unused})
 (2 {request-length})
 (4 (GCONTEXT gc))
 (nil ((BITMASK-VALUES CARD32 #.+createGC-mask-values+)
       value-list)))

(:CopyGC
 (1 {opcode} 57)
 (1 {unused})
 (2 {request-length})
 (4 (GCONTEXT src-gc))
 (4 (GCONTEXT dst-gc))
 (4 ((BITMASK CARD32 #.+copyGC-mask-values+)
	      ;; ((#x00000001 . :background-pixmap)
	      ;; 	      (#x00000002 . :background-pixel)
	      ;; 	      (#x00000004 . :border-pixmap)
	      ;; 	      (#x00000008 . :border-pixel)
	      ;; 	      (#x00000010 . :bit-gravity)
	      ;; 	      (#x00000020 . :win-gravity)
	      ;; 	      (#x00000040 . :backing-store)
	      ;; 	      (#x00000080 . :backing-planes)
	      ;; 	      (#x00000100 . :backing-pixel)
	      ;; 	      (#x00000200 . :override-redirect)
	      ;; 	      (#x00000400 . :save-under)
	      ;; 	      (#x00000800 . :event-mask)
	      ;; 	      (#x00001000 . :do-not-propagate-mask)
	      ;; 	      (#x00002000 . :colormap)
	      ;;  (#x00004000 . :cursor)))
     value-mask)))

(:SetDashes
 (1 {opcode} 58)
 (1 {unused})
 (2 {request-length})
 (4 (GCONTEXT gc))
 (2 (CARD16 dash-offset))
 (2 (CARD16 n :temp? t))  ; (n length of dashes))
 (n ((list CARD8 1) dashes))
 ((pad n) {unused}))

;; to test ? I used to think this one was complicated. But maybe it isn't.
(:SetClipRectangles
 (1 {opcode} 59)
 (1 ((ENUM CARD8 ((0 . :UnSorted)
		  (1 . :YSorted)
		  (2 . :YXSorted)
		  (3 . :YXBanded)))
     ordering))
 (2 {request-length})
 (4 (GCONTEXT gc))
 (2 (INT16 clip-x-origin))
 (2 (INT16 clip-y-origin))
 (nil ((list RECTANGLE 8) rectangles)))

(:FreeGC
 (1 {opcode} 60)
 (1 {unused})
 (2 {request-length})
 (4 (GCONTEXT gc)))

(:ClearArea
 (1 {opcode} 61)
 (1 (BOOL exposures))
 (2 {request-length})
 (4 (WINDOW window))
 (2 (INT16 x))
 (2 (INT16 y))
 (2 (CARD16 width))
 (2 (CARD16 height)))

(:CopyArea
 (1 {opcode} 62)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE src))
 (4 (DRAWABLE dst))
 (4 (GCONTEXT gc))
 (2 (INT16 src-x))
 (2 (INT16 src-y))
 (2 (INT16 dst-x))
 (2 (INT16 dst-y))
 (2 (CARD16 width))
 (2 (CARD16 height)))

(:CopyPlane
 (1 {opcode} 63)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE src))
 (4 (DRAWABLE dst))
 (4 (GCONTEXT gc))
 (2 (INT16 src-x))
 (2 (INT16 src-y))
 (2 (INT16 dst-x))
 (2 (INT16 dst-y))
 (2 (CARD16 width))
 (2 (CARD16 height))
 (4 (CARD32 bit-plane)))

(:PolyPoint
 (1 {opcode} 64)
 (1 ((ENUM CARD8 ((0 . :Origin)
		  (1 . :Previous)))
     coordinate-mode))
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (nil ((list POINT 4) points)))

(:PolyLine
 (1 {opcode} 65)
 (1 ((ENUM CARD8 ((0 . :Origin)
		  (1 . :Previous)))
     coordinate-mode))
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (nil ((list POINT 4) points)))

(:PolySegment
 (1 {opcode} 66)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (nil ((list SEGMENT 8) segments)))

(:PolyRectangle
 (1 {opcode} 67)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (nil ((list RECTANGLE 8) rectangles)))

(:PolyArc
 (1 {opcode} 68)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (nil ((list ARC 12) arcs)))

(:FillPoly
 (1 {opcode} 69)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (1 ((ENUM CARD8 ((0 . :Complex)
		  (1 . :Nonconvex)
		  (2 . :Convex)))
     shape))
 (1 ((ENUM CARD8 ((0 . :Origin)
		  (1 . :Previous)))
     coordinate-mode))
 (2 {unused})
 (nil ((list POINT 4) points)))

(:PolyFillRectangle
 (1 {opcode} 70)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (nil ((list RECTANGLE 8) rectangles)))

(:PolyFillArc
 (1 {opcode} 71)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (nil ((list ARC 12) arcs)))

(:PutImage
 (1 {opcode} 72)
 (1 ((ENUM CARD8 ((0 . :Bitmap)
		  (1 . :XYPixmap)
		  (2 . :ZPixmap)))
     format))
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (2 (CARD16 width))
 (2 (CARD16 height))
 (2 (INT16 dst-x))
 (2 (INT16 dst-y))
 (1 (CARD8 left-pad))
 (1 (CARD8 depth))
 (2 {unused})
 (nil (VECTOR data))) ; includes extra padding bytes ! Because I'm not going to compute n here.

(:GetImage
 (1 {opcode} 73)
 (1 ((ENUM CARD8 ((1 . :XYPixmap)
		  (2 . :ZPixmap)))
     format))
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (2 (INT16 x))
 (2 (INT16 y))
 (2 (CARD16 width))
 (2 (CARD16 height))
 (4 (CARD32 plane-mask)))

;; Note : the X11 _official_ protocol section about PolyText is full of errors !!
;; pad can be 0, 1, 2 or 3. If pad = 3 the first pad byte must be 0, so that the padding is not interpreted as a TextItem.
(:PolyText8
 (1 {opcode} 74)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (2 (INT16 x))
 (2 (INT16 y))
 (nil (list-TEXTITEM8 items))) ; includes padding.
;; ((pad n) {unused}))   ; (p is always 0 or 1)

(:PolyText16
 (1 {opcode} 75)
 (1 {unused})
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (2 (INT16 x))
 (2 (INT16 y))
 (nil (list-TEXTITEM16 items))) ; includes padding.
;; ((pad n) {unused})) ; (p must be 0 or 1)

(:ImageText8
 (1 {opcode} 76)
 (1 (COUNT8 n :temp? t)) ; (n length of string))
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (2 (INT16 x))
 (2 (INT16 y))
 (n (STRING8 string))
 ((pad n) {unused}))

;;untested
(:ImageText16
 (1 {opcode} 77)
 (1 (COUNT8 n :temp? t)) ; (n number of CHAR2Bs in string))
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (4 (GCONTEXT gc))
 (2 (INT16 x))
 (2 (INT16 y))
 ((* n 2) (STRING16 string))
 ((pad (* n 2)) {unused}))

(:CreateColormap
 (1 {opcode} 78)
 (1 ((ENUM CARD8 ((0 . nil)
		  (1 . :All)))
     alloc))
 (2 {request-length})
 (4 (COLORMAP mid))
 (4 (WINDOW window))
 (4 (VISUALID visual)))

(:FreeColormap
 (1 {opcode} 79)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap)))

(:CopyColormapAndFree
 (1 {opcode} 80)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP mid))
 (4 (COLORMAP src-cmap)))

(:InstallColormap
 (1 {opcode} 81)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap)))

(:UninstallColormap
 (1 {opcode} 82)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap)))

(:ListInstalledColormaps
 (1 {opcode} 83)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window)))

(:AllocColor
 (1 {opcode} 84)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap))
 (2 (CARD16 red))
 (2 (CARD16 green))
 (2 (CARD16 blue))
 (2 {unused}))

(:AllocNamedColor
 (1 {opcode} 85)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap))
 (2 (COUNT16 n :temp? t)) ;  (n length of name))
 (2 {unused})
 (n (STRING8 name))
 ((pad n) {unused}))

(:AllocColorCells
 (1 {opcode} 86)
 (1 (BOOL contiguous))
 (2 {request-length})
 (4 (COLORMAP cmap))
 (2 (CARD16 colors))
 (2 (CARD16 planes)))

(:AllocColorPlanes
 (1 {opcode} 87)
 (1 (BOOL contiguous))
 (2 {request-length})
 (4 (COLORMAP cmap))
 (2 (CARD16 colors))
 (2 (CARD16 reds))
 (2 (CARD16 greens))
 (2 (CARD16 blues)))

(:FreeColors
 (1 {opcode} 88)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap))
 (4 (CARD32 plane-mask))
 (nil ((list CARD32 4) pixels)))

(:StoreColors
 (1 {opcode} 89)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap))
 (nil ((list COLORITEM 12) items)))

(:StoreNamedColor
 (1 {opcode} 90)
 (1 ((BITMASK CARD8 ((#x01 . :do-red)
		     (#x02 . :do-green)
		     (#x04 . :do-blue)))
     do-rgb))
 (2 {request-length})
 (4 (COLORMAP cmap))
 (4 (CARD32 pixel))
 (2 (COUNT16 n :temp? t)) ; (n length of name))
 (2 {unused})
 (n (STRING8 name))
 ((pad n) {unused}))

(:QueryColors
 (1 {opcode} 91)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap))
 (nil ((list CARD32 4) pixels)))

(:LookupColor
 (1 {opcode} 92)
 (1 {unused})
 (2 {request-length})
 (4 (COLORMAP cmap))
 (2 (COUNT16 n :temp? t)) ; (n length of name))
 (2 {unused})
 (n (STRING8 name))
 ((pad n) {unused}))

(:CreateCursor
 (1 {opcode} 93)
 (1 {unused})
 (2 {request-length})
 (4 (CURSOR cid))
 (4 (PIXMAP source))
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 PIXMAP)
     mask))
 (2 (CARD16 fore-red))
 (2 (CARD16 fore-green))
 (2 (CARD16 fore-blue))
 (2 (CARD16 back-red))
 (2 (CARD16 back-green))
 (2 (CARD16 back-blue))
 (2 (CARD16 x))
 (2 (CARD16 y)))

(:CreateGlyphCursor
 (1 {opcode} 94)
 (1 {unused})
 (2 {request-length})
 (4 (CURSOR cid))
 (4 (FONT source-font))
 (4 ((or (ENUM CARD32 ((0 . nil)))
	 FONT)
     mask-font))
 (2 (CARD16 source-char))
 (2 (CARD16 mask-char))
 (2 (CARD16 fore-red))
 (2 (CARD16 fore-green))
 (2 (CARD16 fore-blue))
 (2 (CARD16 back-red))
 (2 (CARD16 back-green))
 (2 (CARD16 back-blue)))

(:FreeCursor
 (1 {opcode} 95)
 (1 {unused})
 (2 {request-length})
 (4 (CURSOR cursor)))

(:RecolorCursor
 (1 {opcode} 96)
 (1 {unused})
 (2 {request-length})
 (4 (CURSOR cursor))
 (2 (CARD16 fore-red))
 (2 (CARD16 fore-green))
 (2 (CARD16 fore-blue))
 (2 (CARD16 back-red))
 (2 (CARD16 back-green))
 (2 (CARD16 back-blue)))

(:QueryBestSize
 (1 {opcode} 97)
 (1 ((ENUM CARD8 ((0 . :Cursor)
		  (1 . :Tile)
		  (2 . :Stipple)))
     class))
 (2 {request-length})
 (4 (DRAWABLE drawable))
 (2 (CARD16 width))
 (2 (CARD16 height)))

(:QueryExtension
 (1 {opcode} 98)
 (1 {unused})
 (2 {request-length})
 (2 (COUNT16 n :temp? t)) ; (n length of name))
 (2 {unused})
 (n (STRING8 name))
 ((pad n) {unused}))

(:ListExtensions
 (1 {opcode} 99)
 (1 {unused})
 (2 {request-length}))

(:ChangeKeyboardMapping
 (1 {opcode} 100)
 (1 (COUNT8 n :temp? t)) ; (n keycode-count))
 (2 {request-length})
 (1 (KEYCODE first-keycode))
 (1 (COUNT8 keysyms-per-keycode))
 (2 {unused})
 ((* 4 n keysyms-per-keycode) ((list KEYSYM 4) keysyms)))

(:GetKeyboardMapping
 (1 {opcode} 101)
 (1 {unused})
 (2 {request-length})
 (1 (KEYCODE first-keycode))
 (1 (COUNT8 count))
 (2 {unused}))

(:ChangeKeyboardControl
 (1 {opcode} 102)
 (1 {unused})
 (2 {request-length})
 (nil ((BITMASK-VALUES CARD32 ((#x0001 . (INT8/4 key-click-percent))
			       (#x0002 . (INT8/4 bell-percent))
			       (#x0004 . (INT16/4 bell-pitch))
			       (#x0008 . (INT16/4 bell-duration))
			       (#x0010 . (CARD8/4 led))
			       (#x0020 . ((ENUM CARD8/4 ((0 . :Off)
							 (1 . :On)))
					  led-mode))
			       (#x0040 . (KEYCODE/4 key))
			       (#x0080 . ((ENUM CARD8 ((0 . :Off)
						       (1 . :On)
						       (2 . :Default)))
					  auto-repeat-mode))))
       value-list)))

(:GetKeyboardControl
 (1 {opcode} 103)
 (1 {unused})
 (2 {request-length}))

(:Bell
 (1 {opcode} 104)
 (1 (INT8 percent))
 (2 {request-length}))

(:ChangePointerControl
 (1 {opcode} 105)
 (1 {unused})
 (2 {request-length})
 (2 (INT16 acceleration-numerator))
 (2 (INT16 acceleration-denominator))
 (2 (INT16 threshold))
 (1 (BOOL do-acceleration))
 (1 (BOOL do-threshold)))

(:GetPointerControl
 (1 {opcode} 106)
 (1 {unused})
 (2 {request-length}))

(:SetScreenSaver
 (1 {opcode} 107)
 (1 {unused})
 (2 {request-length})
 (2 (INT16 timeout))
 (2 (INT16 interval))
 (1 ((ENUM CARD8 ((0 . :No)
		  (1 . :Yes)
		  (2 . :Default)))
     prefer-blanking))
 (1 ((ENUM CARD8 ((0 . :No)
		  (1 . :Yes)
		  (2 . :Default)))
     allow-exposures))
 (2 {unused}))

(:GetScreenSaver
 (1 {opcode} 108)
 (1 {unused})
 (2 {request-length}))

(:ChangeHosts
 (1 {opcode} 109)
 (1 ((ENUM CARD8 ((0 . :Insert)
		  (1 . :Delete)))
     mode))
 (2 {request-length})
 (1 ((ENUM CARD8 ((0 . :Internet)
		  (1 . :DECnet)
		  (2 . :Chaos)))
     family))
 (1 {unused})
 (2 (COUNT16 n :temp? t))  ; (n length of address))
 (n ((list CARD8 1) address))
 ((pad n) {unused}))

(:ListHosts
 (1 {opcode} 110)
 (1 {unused})
 (2 {request-length}))

(:SetAccessControl
 (1 {opcode} 111)
 (1 ((ENUM CARD8 ((0 . :Disable)
		  (1 . :Enable)))
     mode))
 (2 {request-length}))

(:SetCloseDownMode
 (1 {opcode} 112)
 (1 ((ENUM CARD8 ((0 . :Destroy)
		  (1 . :RetainPermanent)
		  (2 . :RetainTemporary)))
     mode))
 (2 {request-length}))

(:KillClient
 (1 {opcode} 113)
 (1 {unused})
 (2 {request-length})
 (4 ((or (ENUM CARD32 ((0 . :AllTemporary)))
	 CARD32)
     resource)))

(:RotateProperties
 (1 {opcode} 114)
 (1 {unused})
 (2 {request-length})
 (4 (WINDOW window))
 (2 (COUNT16 n :temp? t)) ; (n number of properties))
 (2 (INT16 delta))
 ((* 4 n) ((list ATOM 4) names)))

(:ForceScreenSaver
 (1 {opcode} 115)
 (1 ((ENUM CARD8 ((0 . :Reset)
		  (1 . :Activate)))
     mode))
 (2 {request-length}))

(:SetPointerMapping
 (1 {opcode} 116)
 (1 (COUNT8 n :temp? t)) ; (n length of map))
 (2 {request-length})
 (n ((list CARD8 1) map))
 ((pad n) {unused}))

(:GetPointerMapping
 (1 {opcode} 117)
 (1 {unused})
 (2 {request-length}))

(:SetModifierMapping
 (1 {opcode} 118)
 (1 (n keycodes-per-modifier))
 (2 {request-length})
 (nil ((list KEYCODE 1) keycodes)))

(:GetModifierMapping
 (1 {opcode} 119)
 (1 {unused})
 (2 {request-length}))

(:NoOperation
 (1 {opcode} 127)
 (1 {unused})
 (2 {request-length}))

(:BigReqEnable
 (1 {opcode} #.+bigreq-opcode+)
 (1 {extension-opcode} 0)
 (2 {request-length}))

) ; interpret-requests

(deftranslator/request :Connection
  (1 {endian})  ;; was :  ((ENUM CARD8 ((#x42 . msb) (#x6c . lsb))) endian)
  (1 {unused})
  (2 (CARD16 protocol-major-version))
  (2 (CARD16 protocol-minor-version))
  (2 (CARD16 n :temp? t))
  (2 (CARD16 d :temp? t))
  (2 {unused})
  (n (STRING8 authorization-protocol-name))
  ((pad n) {unused})
  (d (STRING8 authorization-protocol-data))
  ((pad d) {unused}))

(eval-when (:compile-toplevel :load-toplevel :execute)
;
(setf *readtable* (copy-readtable nil))
);