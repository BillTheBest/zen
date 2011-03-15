;; (C) 2011 Pierre-Yves Baccou

;; not used in zen 0.5

(require 'cffi)

(load "/home/pyb/zen/glx/constants.lisp")

(cffi:define-foreign-library libxpm
  (t (:default "libXpm")))
(cffi:use-foreign-library libxpm)

(cffi:defcstruct xpmimage
  (width :uint)
  (height :uint)
  (cpp :uint)
  (ncolor :uint)
  (color-table :pointer)
  (data :pointer))

;; What to do with these non-GLX functions ?
(cffi:defcfun "XpmReadFileToXpmImage" :int
  (filename :pointer)
  (image :pointer)
  (info :pointer))
