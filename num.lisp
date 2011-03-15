;; (C) 2011 Pierre-Yves Baccou

(provide "num")

(defun uint16 (seq &optional (endian :lsb))
  (ecase endian
    (:lsb (+ (ash (elt seq 0) 0)
	     (ash (elt seq 1) 8)))
    (:msb (+ (ash (elt seq 1) 0)
	     (ash (elt seq 0) 8)))))

(defun uint32 (seq &optional (endian :lsb))
  (ecase endian
    (:lsb (+ (ash (elt seq 0) 0)
	     (ash (elt seq 1) 8)
	     (ash (elt seq 2) 16)
	     (ash (elt seq 3) 24)))
    (:msb (+ (ash (elt seq 3) 0)
	     (ash (elt seq 2) 8)
	     (ash (elt seq 1) 16)
	     (ash (elt seq 0) 24)))))

(defun int16 (seq &optional (endian :lsb))
  (let ((i (uint16 seq endian)))
    (if (= (ash i -15) 1)
	(- i #x10000)
	i)))

(defun int32 (seq &optional (endian :lsb))
  (let ((i (uint32 seq endian)))
    (if (= (ash i -31) 1)
	(- i #x100000000)
	i)))

(defun pad (n)
  (let ((n (if (numberp n)
	       n
	       (length n))))
    (- (* 4
	  (ceiling n
		   4))
       n)))
