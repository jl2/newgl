;; newgl.asd
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(setf *features* (remove :3D-VECTORS-DOUBLE-FLOATS *features*))

(asdf:defsystem #:newgl
  :description "New opengl graphics library."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria
               #:j-utils
               #:cl-glfw3
               #:cl-opengl

               #:3d-vectors
               #:3d-matrices

               #:cl-ppcre

               #:blend2d

               #:bordeaux-threads
               #:trivial-main-thread)

  :components ((:file "package")
               (:file "common")
               (:file "newgl")
               (:file "viewer")
               (:file "layouts")
               (:file "uniforms")
               (:file "gl-shader")
               (:file "opengl-object")
               (:file "geometry")
               (:file "instanced-geometry")
               (:file "parametric-surfaces")
               (:file "texture")
               (:file "blend2d-texture")
               (:file "tri-mesh")
               (:file "point-cloud")
               (:file "line-segments")
               (:file "quad")
               (:file "keyframe-sequence")
               (:file "testing")
               (:file "keyframe-viewer")
               )
  :in-order-to ((test-op (test-op newgl.test))))
