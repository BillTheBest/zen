;; Includes code from Alexandria, and from PG' On Lisp.

; Would be useful : anonymous function syntax a la Clojure :   #(> length %) 2) is  (lambda (x) (> (length x) 2))
(provide "util")

(sb-ext:unlock-package 'sb-ext) ; to allow (defstruct gc)

;; Allow [] as parens. FIXME
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;  (set-macro-character #\] (get-macro-character #\) ))
;;  (set-macro-character #\( ...))

; PG

(defmacro <- (&body body) `(lambda (_) ,@body)) ; see also #F ?

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

(abbrev dbind destructuring-bind)
(abbrev bind destructuring-bind)
(abbrev mvbind multiple-value-bind)

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

; lists

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun carat (x)
  (if (consp x) (car x) x))

(defun flatten (x)
  (mapcan #'(lambda (x)
	      (if (atom x) (mklist x) (flatten x)))
	  x))

; functional
(defun compose (&rest fns)
  (bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f)
		    (funcall f v))
		rest
		:initial-value (apply fn1 args)))))

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

; anamorphic

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                 (case (length args)
                   (0 nil)
                   (1 (car args))
                   (t `(let ((it ,(car args)))
                         ,(self (cdr args))))))
               args)))

; queue

(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q) 
  (pop (car q)))

(defun queue-elt (q i)
  (elt (car q) i))

(defun empty-queue (q)
  (null (car q)))

; symbols

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr
			 args))))

; slightly modified
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym ,(mkstr s))))
                 syms)
     ,@body))


; also in Alexandria
(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (intern (string name) :keyword))

(defmacro if-let (bindings &body (then-form &optional else-form))
    "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defmacro when-let (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as an
implicit PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))


;PYB. same as replace ?
(defun copy-vector (s1 s2 &key (start1 0) (start2 0) end1 end2 single)
  "Copy elements from simple vector s1 to s2. Return s2"
  (let ((end1 (or end1 (length s1))) ; not error checking on arguments.
	(end2 (or end2 (length s2))))
    (do ((i start1 (1+ i))
	 (j start2 (1+ j)))
	((or (>= i end1)
	     (>= j end2))
	 s2)
      (if single
	  (setf (svref s2 j) (svref s1 i))
	  (setf (aref s2 j) (aref s1 i))))))

; from On Lisp + support vectors
(defun group (source n)
  (when (zerop n)
    (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (if (consp source)
			     (nthcdr n
				     source)
			     (subseq source
				     (min (length source)
					  n)))))
               (if (or (consp rest)
		       (/= 0 (length rest)))
                   (rec rest
			(cons (subseq source
				      0
				      n)
			      acc))
                   (nreverse (cons source
				   acc))))))
    (when (or (consp source)
	      (and (vectorp source)
		   (/= 0 (length source))))
      (rec source nil))))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache) 
                    (apply fn args)))))))

(defmacro memoize-with-reset (fn reset)
  `(let ((cache (make-hash-table :test #'equal)))
     #'(lambda (&rest args)
	 (when ,reset
	   (setf cache (make-hash-table :test #'equal)
		 ,reset nil))
	 (multiple-value-bind (val win) (gethash args cache)
	   (if win
	       val
	       (setf (gethash args cache) 
		     (apply ,fn args)))))))

#|
(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access) 
                       (get-setf-method place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))
|#

(defmacro fsetf (name fn)
  "setf for functions."
  `(setf (symbol-function ',name)
	 ,fn))


; useful for SBCL (see SBCL manual sec. 2.3.4)
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name
     (if (boundp ',name)
	 (symbol-value ',name)
	 ,value)
     ,@(when doc (list doc))))

; used only in RotateProperties
(defun rotate-list (lst d)
  "Return a copy of list rotated by d spaces"
  (let ((head (mod d
		   (length lst))))
    (nconc (copy-list (nthcdr head
			      lst))
	   (copy-list (subseq lst
			      0
			      head)))))

(defun read-all-from-string (str &optional (i 0))
  "Reads and returns all objects from string str."
  (multiple-value-bind (val k)
      (read-from-string str nil nil :start i)
    (when val
      (cons val 
	    (read-all-from-string str
				  k)))))

; fixme : better name : (defstruct-as-class ... ??)
(defmacro defclass/struct (name parents &rest args)
  "Define a class with defstruct syntax.
  (Like defclass, but, as in defstruct, define default accessors 'name-arg' and 'parent-arg' for each parent, and default initform.)"
  `(defclass ,name ,parents
     (,@(mapcar #'(lambda (arg)
		    (let* ((arg (mklist arg))
			   (arg-name (first arg))
			   (default (second arg)))
		      `(,arg-name :initarg ,(make-keyword arg-name)
				  ,@(mapcan #'(lambda (name)
						`(:accessor ,(symb name "-" arg-name)))
					    (list* name parents))
				  :initform ,default)))
		args))))

(defun enable-braces (&optional expr)
  "Read macro : Replace {...} with expr"
  (set-macro-character #\} (get-macro-character #\)))
  (set-macro-character #\{
		       #'(lambda (stream char)
			   (read-delimited-list #\} stream t)
			   expr)))

(defmacro with-fields ((&rest fields) (obj struct) &body body)
  "Similar to with-slots, for structs"
  (setf fields
	(mapcar #'mklist
		fields))
  (let ((bindings (mapcar #'(lambda (item)
			      (bind (name &optional field)
				  item
				`(,name (,(symb struct
						"-"
						(or field
						    name)) 
					  ,obj))))
			  fields)))
    `(symbol-macrolet ,bindings
       ,@body)))

(defun ninsert (elt lst p)
  "Return list with elt destructively inserted at position p."
  (nconc (subseq lst
		 0 
		 p)
	 (list elt)
	 (subseq lst
		 p)))

;; To consider

(defun assocify (source)  ; PG
  (labels ((rec (source acc)
             (let ((rest (cddr source)))
               (if (consp rest)
                   (rec rest (cons (cons (car source) (cadr source)) acc))
                   (nreverse (cons (cons (car source) (cadr source)) acc))))))
    (if source (rec source nil) nil)))

(defmacro setf-nil (place val)
  `(unless ,place
     (setf ,place ,val)))

;; unused
(defun split-string (string char)
  (let ((p (position char string)))
    (if (null p)
	(list string)
	(cons (subseq string 0 p)
	      (split-string (subseq string (1+ p))
			    char)))))
