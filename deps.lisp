(provide "deps")

(eval-when (:compile-toplevel :load-toplevel :execute)
;
(require 'sb-bsd-sockets)
(use-package 'sb-bsd-sockets)
;
(require 'cffi)
(require 'bordeaux-threads)
(load "util/util")
(load "util/unix")
(load "num")
;
)