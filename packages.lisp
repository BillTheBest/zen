;; (C) 2011 Pierre-Yves Baccou

(in-package "COMMON-LISP-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
;
(require 'cffi)

(defpackage "ZENGL" 
  (:use "COMMON-LISP")
  (:export *get-proc-address*
	   init
	   flush
	   make-framebuffer
	   create-framebuffer
	   draw-framebuffer
	   read-framebuffer
	   read-origin
	   draw-origin
	   shift-draw-origin
	   shift-read-origin
	   add-stencil
	   copy-area
	   points
	   lines
	   line-strip
	   rectangle
	   polygon
	   pixels))  

(defpackage "MYGLX"
  (:use "COMMON-LISP" "CFFI")
  (:export XOpenDisplay XCloseDisplay XFlush
	   %make-current
	   %swap-buffers
	   %choose-fb-config
	   %get-fb-config-attrib
	   %get-visual-from-fb-config
	   %create-window
	   %create-pbuffer
	   %create-new-context
	   %make-context-current
	   wait-X
	   wait-GL
	   %get-proc-address
	   %choose-visual
	   %create-context
	   %destroy-context
	   %copy-context
	   %create-pixmap
	   %destroy-pixmap
	   %query-extension
	   %query-version
	   %is-direct
	   %get-config
	   %get-current-context
	   %getcurrent-drawable
	   %use-x-font
	   %query-extensions-string
	   %query-server-string
	   %get-client-string
	   %get-current-display
	   %get-fb-configs
	   %destroy-window
	   %create-pixmap
	   %destroy-pixmap
	   %destroy-pbuffer
	   %query-drawable
	   %get-current-read-drawable
	   %query-context
	   %select-event
	   %get-selected-event
	   %bind-tex-image-ext
	   %release-tex-image-ext))
;
) ; eval-when