(require 'cl-opengl)
(in-package #:cl-opengl)

(import-export %gl:is-renderbuffer %gl:bind-renderbuffer)

(export 'is-renderbuffer)
(export 'bind-renderbuffer)
(export 'delete-renderbuffers)
(export 'gen-renderbuffers)
(export 'renderbuffer-storage)
(export 'is-framebuffer)
(export 'bind-framebuffer)
(export 'delete-framebuffers)
(export 'gen-framebuffers)
(export 'check-framebuffer-status)
(export 'framebuffer-texture-1d)
(export 'framebuffer-texture-2d)
(export 'framebuffer-texture-3d)
(export 'framebuffer-renderbuffer)


