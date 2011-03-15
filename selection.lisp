;; (C) 2011 Pierre-Yves Baccou

(defstruct selection
  atom
  owner
  last-change-time)

(defvar *selections* nil)

;; event SelectionNotify : never sent by the server, always by clients via SendEvent.

(defevent :SelectionClear (owner selection (time (server-time)))
  :mode :custom :custom-clients	(list (selection-previous-owner selection)))

(defevent :SelectionRequest (owner selection target property requestor (time (server-time)))
  :mode :custom :custom-clients (list (selection-owner selection)))


(defrequest :GetSelectionOwner ((sel xatom))
  (reply :owner nil))

(unimplemented :SetSelectionOwner t)
(unimplemented :ConvertSelection t)

#| unfinished

(defrequest SetSelectionOwner ((sel xatom) (owner window (nil)) time)
  (let ((selection (find sel
			 *selections*
			 :key #'selection-atom)))
    (unless selection
      (setf selection
	    (make-selection :atom sel
			    :time (server-time)))
      (push selection
	    *selections*))
|#  
			    