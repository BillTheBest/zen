(defun print-client2 (c &rest args)
  (with-fields (in-buffer out-bufqueue outbuf-lock in-fill-pointer out-fill-pointer address port socket fd endian initialized request-length seqnum cur-out-buf)
      (c client)
    (format t
	    "outbuf-lock ~A~% in-fill-pointer ~A~% out-fill-pointer ~A~% address ~A~% port ~A~% socket ~A~% fd ~A~% endian ~A~% initialized ~A~% request-length ~A~% seqnum ~A inbuf ~A ...~% ~A outbufs with ~A, cur outbuf ~A~%"
	    outbuf-lock in-fill-pointer out-fill-pointer address port socket fd endian initialized request-length seqnum
	    (subseq in-buffer 0 30)
	    (length (first out-bufqueue))
	    (awhen (first (first out-bufqueue))
	      (subseq it 0 (min 30 (length it))))
	    cur-out-buf)))
