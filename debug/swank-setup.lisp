(require 'swank)
(setf swank:*globally-redirect-io* t)
(swank:create-server)


; in emacs :
; (slime-connect "127.0.0.1" 4005)