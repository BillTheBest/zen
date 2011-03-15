;; (C) 2011 Pierre-Yves Baccou

(require "util" "util/util")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-braces '(nil nil)))

(define-constant +error-codes+
  '((1  . :Request)
    (2  . :Value)
    (3  . :Window)
    (4  . :Pixmap)
    (5  . :XAtom)
    (6  . :Cursor)
    (7  . :Font)
    (8  . :Match)
    (9  . :Drawable)
    (10 . :Access)
    (11 . :Alloc)
    (12 . :Colormap)
    (13 . :gc)
    (14 . :Idchoice)
    (15 . :Name)
    (16 . :Length)
    (17 . :Implementation)))

;; disable sqbraces.
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (setf *readtable* (copy-readtable nil)))
