;; (C) 2011 Pierre-Yves Baccou

;; Simplifed implementation of regions inspired by this : (see miregion.c in XOrg server)

;; ----------------------------------------------------------------------------

;; Copyright 1987, 1988, 1989, 1998  The Open Group
;; Copyright 1987, 1988, 1989 by 
;; Digital Equipment Corporation, Maynard, Massachusetts. 
;; and the Massachusetts Institute of Technology, Cambridge, Massachusetts


;; The functions in this file implement the Region abstraction used extensively
;; throughout the X11 sample server. A Region is simply a set of disjoint
;; (non-overlapping) rectangles, plus an "extent" rectangle which is the
;; smallest single rectangle that contains all the non-overlapping rectangles.
;;  
;; A Region is implemented as a "y-x-banded" array of rectangles.  This array
;; imposes two degrees of order.  First, all rectangles are sorted by top side
;; y coordinate first (y1), and then by left side x coordinate (x1).
;;  
;; Furthermore, the rectangles are grouped into "bands".  Each rectangle in a
;; band has the same top y coordinate (y1), and each has the same bottom y
;; coordinate (y2).  Thus all rectangles in a band differ only in their left
;; and right side (x1 and x2).  Bands are implicit in the array of rectangles:
;; there is no separate list of band start pointers.
;;  
;; The y-x band representation does not minimize rectangles.  In particular,
;; if a rectangle vertically crosses a band (the rectangle has scanlines in 
;; the y1 to y2 area spanned by the band), then the rectangle may be broken
;; down into two or more smaller rectangles stacked one atop the other. 
;;  
;;  -----------				    -----------
;;  |         |				    |         |		    band 0
;;  |         |  --------		    -----------  --------
;;  |         |  |      |  in y-x banded    |         |  |      |   band 1
;;  |         |  |      |  form is	    |         |  |      |
;;  -----------  |      |		    -----------  --------
;;               |      |				 |      |   band 2
;;               --------				 --------
;;  
;; An added constraint on the rectangles is that they must cover as much
;; horizontal area as possible: no two rectangles within a band are allowed
;; to touch.
;;  
;; Whenever possible, bands will be merged together to cover a greater vertical
;; distance (and thus reduce the number of rectangles). Two bands can be merged
;; only if the bottom of one touches the top of the other and they have
;; rectangles in the same places (of the same width, of course).

;; ----------------------------------------------------------------------------


;; zen regions are a lot simpler. They are just a list of rects (without extents)
;; rectangle = list : (x1 y1 x2 y2) aka (l top r bot). Note top < bot because of X's coordinate system.
;; have a library ? region:complement region:add-rect etc would be nicer ?

(provide "region")

(load "util/util")

(flet ((top (rect)
	 (second rect))
       (bot (rect)
	 (fourth rect))
       (l (rect)
	 (first rect))
       (r (rect)
	 (third rect)))
;
;; horizontal cut, always returns a list of rectangles.
(defun cut (rect y)
  (bind (l top r bot)
      rect
    (if (not (< top y bot))
	(list rect)
	(list (list l top r y)
	      (list l y   r bot)))))
;
(defun coalesce-band (rects)
  (when rects
    (bind (l top r bot)
	(first rects)
      (cond ((null (rest rects))
	     rects)
	    ((< r
		(l (second rects)))
	     (list* (first rects)
		    (coalesce-band (rest rects))))
	    ((>= r
		 (r (second rects)))
	     (list* (first rects)
		    (coalesce-band (cddr rects))))
	    (t
	     (coalesce-band (list* (list l
					 top
					 (r (second rects))
					 bot)
				   (cddr rects))))))))

;; doc : p = first rectangle whose top line is not level with the first
;;     : q = first rectangle not in a band together with the first.
;; I should probably rename p <-> q so as to put the most 'specific' first
(defun coalesce (rects)
  (when rects
    (bind (l1 top1 r1 bot1)
	(first rects)
      (let ((p (position-if-not #'(lambda (rect)
				    (= top1 (top rect)))
				rects))
	    (q (position-if-not #'(lambda (rect)
				    (and (= top1 (top rect))
					 (= bot1 (bot rect))))
				rects)))
	(cond ((null q) ; we have a perfect band
	       (coalesce-band rects))
	      ((<= bot1 ; we have a standalone band (of one or many rects)
		   (top (elt rects q)))
	       (nconc (coalesce-band (subseq rects
					     0
					     q))
		      (coalesce (subseq rects
					q))))
	      ((and p
		    (= 1 p)) ; no others at same top level
	       (bind (rect-top rect-bot)
		   (cut (first rects)
			(top (second rects)))
		 (list* rect-top
			(coalesce (sort-rects (list* rect-bot
						     (rest rects)))))))
	      (t (let ((next-highest-line (if p
					      (min (top (elt rects
							     p))
						   (bot (first rects)))
					      (bot (first rects)))))
		   (coalesce (sort-rects (mapcan (rcurry #'cut
							 next-highest-line)
						 rects))))))))))

(defun sort-rects (rects)
  (sort rects #'(lambda (rect1 rect2)
		  (bind (l1 top1 r1 bot1)
		      rect1
		    (bind (l2 top2 r2 bot2)
			rect2
		      (cond ((< top1 top2) t)
			    ((> top1 top2) nil)
			    ((< bot1 bot2) t)
			    ((> bot1 bot2) nil)
			    ((< l1 l2) t)
			    ((> l1 l2) nil)
			    ((< r1 r2) t)
			    ((> r1 r2) nil)))))))

(defun rect-intersection (rect1 rect2)
  (declare (list rect1 rect2))
  (bind (l1 top1 r1 bot1)
      rect1
    (declare (fixnum l1 top1 r1 bot1))
    (bind (l2 top2 r2 bot2)
	rect2
      (declare (fixnum l2 top2 r2 bot2))
      (unless (or (>= l2 r1)
		  (>= l1 r2)
		  (>= top2 bot1)
		  (>= top1 bot2))
	(list (max l1 l2)
	      (max top1 top2)
	      (min r1 r2) 
	      (min bot1 bot2))))))

(defun degenerate? (rect)
  (declare (list rect))
  (bind (l top r bot)
    rect
    (declare (fixnum l top r bot))
    (or (= l r)
	(= top bot))))

(defun remove-rects (rect rects)
  "remove from rect the intersection between rect and rects"
  (declare (inline degenerate?))
  (acond ((null rects)
	  (list rect))
	 ((rect-intersection rect
			     (first rects))
	  (bind (li topi ri boti)
	      it
	    (bind (l top r bot)
		 rect
	      (mapcan (rcurry #'remove-rects
			      (rest rects))
		      (remove-if #'degenerate?
				 `((,l  ,top  ,r  ,topi)
				   (,l  ,topi ,li ,boti)
				   (,ri ,topi ,r  ,boti)
				   (,l  ,boti ,r  ,bot)))))))
	 (t
	  (remove-rects rect
			(rest rects)))))

;; What was the sorting for ?
;;(defun region-difference (rects1 rects2)
;;  (sort-rects (mapcan (rcurry #'remove-rects 
;;			      rects2)
;;		      rects1)))

(defun region-difference (rects1 rects2)
  "What's in region 1 and not in region 2?"
  (mapcan (rcurry #'remove-rects 
		  rects2)
	  rects1))

(defun shift-region (dx dy rects)
  (mapcar #'(lambda (rect)
	      (bind (x1 y1 x2 y2)
		  rect
		(list (+ x1 dx) (+ y1 dy) (+ x2 dx) (+ y2 dy))))
	  rects))
		     
(defun extents (rects)
  (let ((ex1 (apply #'min (mapcar #'first  rects)))
	(ey1 (apply #'min (mapcar #'second rects)))
	(ex2 (apply #'max (mapcar #'third  rects)))
	(ey2 (apply #'max (mapcar #'fourth rects))))
    (list ex1 ex2 ey1 ey2)))
;
) ; flet (top bot l r)

#|
;; If I ever go back to XOrg-type regions...
(defun add-rect (rect region)
    (bind (x1 y1 x2 y2)
	rect
      (bind (ex1 ey1 ex2 ey2) ; extents
	  (first region)
	(list* (list (min x1 ex1)
		     (min y1 ey1)
		     (max x2 ex2)
		     (max y2 ey2)) ; new extents
	       rect
	       (rest region)))))
|#
