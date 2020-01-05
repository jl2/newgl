;;;; package.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


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
(defpackage :newgl
  (:use #:cl #:j-utils #:alexandria #:glfw #:3d-vectors #:3d-matrices)
  (:export #:opengl-object
           #:vertex-object
           #:ebos
           #:vbos
           #:vertices
           #:indices
           #:shader-program
           #:program
           #:gl-shader
           #:rebuild-shaders
           #:set-uniforms
           #:build-shader-program
           #:fill-buffers
           #:reload-object
           #:cleanup
           #:to-gl-array
           #:to-gl-float-array

           #:shaders
           #:shader-type
           #:source-file
           #:layout
           #:shader

           #:update
           #:render
           #:handle-key
           #:handle-scroll
           #:handle-drag
           #:handle-click
           #:handle-resize

           #:tri-mesh
           #+stl-to-open-gl #:view-stl

           #:point-cloud
           #:add-point

           #:viewer

           #:quad
           #:make-uv-quad

           #:*previous-mouse-drag*
           #:*mouse-press-info*
           #:*mouse-release-info*
           #:mouse-click
           #:cursor-pos
           #:mod-keys
           #:action
           #:button
           #:time
           #:viewer-thread-function))
