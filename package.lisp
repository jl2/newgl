;; package.lisp
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
(defpackage :newgl
  (:use #:cl #:j-utils #:alexandria #:glfw #:3d-vectors #:3d-matrices)
  (:export #:opengl-object
           #:vertex-object
           #:vao
           #:ebos
           #:vbos
           #:primitive-type
           #:xform
           #:vertices
           #:indices
           #:shader-program
           #:program
           #:gl-shader
           #:set-uniforms
           #:build-shader-program
           #:bind-buffers
           #:fill-buffers
           #:reload-object
           #:cleanup
           #:to-gl-array
           #:to-gl-float-array

           #:ensure-vao-bound
           #:build-shader-program
           #:use-shader-program
           #:enable-layout
           #:compile-shader
           #:use-layout
           #:get-source
           #:cleanup
           #:render
           #:update
           #:fill-buffers
           #:handle-key
           #:handle-click
           #:handle-scroll
           #:handle-drag
           #:handle-resize
           #:reload-object
           #:make-plastic-program
           #:set-uniform
           #:shader-type
           #:source-file
           #:layout
           #:shader
           #:view-xform

           #:tri-mesh
           #:add-point-tm
           #:parametric-tri-mesh
           #+stl-to-open-gl #:view-stl

           #:make-layout
           #:make-layout-entry

           #:point-cloud
           #:make-point-cloud
           #:add-point-pc
           #:make-square
           #:random-point-cloud
           #:parametric-point-cloud

           #:make-line-segments

           #:line-segments
           #:add-line
           #:add-line-2
           #:add-line-by-pt-index
           #:add-line-by-pt-index-2
           #:add-line-by-index
           #:random-line-cloud
           #:fractal-tree

           #:display

           #:scene

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
           #:viewer-thread-function

           #:shader-from-file
           #:make-shader-program

           #:canvas
           #:draw-line
           #:draw-triangle

           #:test-scene
           #:show-test-scene

           #:keyframe
           #:create-keyframe
           #:create-keyframe-sequence
           #:keyframe-sequence
           #:keyframe-count
           #:value-at
           #:create-simple-keyframe-sequence
           ))
