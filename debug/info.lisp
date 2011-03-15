(defvar *resources* nil)

(defun attr-info (attr)
  (with-slots (background-pixel border-pixmap border-pixel event-masks do-not-propagate-mask)
      attr
    (format t "background-pixel ~A border-pixel ~A border-pixmap ~A event-masks ~A do-not-propagate-mask ~A~%" background-pixel border-pixel border-pixmap event-masks do-not-propagate-mask)))
    
(defmethod resource-info (obj)
  nil)

(defmethod resource-info ((window window))
  (with-slots (parent id mapped viewable x y width height attr properties)
      window
    (format t "Window ~A : id ~A parent ~A mapped ~A viewable ~A x ~A y ~A  width  ~A height ~A~%" window id parent mapped viewable x y width height)
    (attr-info attr)))

(defun nfo ()
  (mapcar #'resource-info
	  *resources*)
  (values))

