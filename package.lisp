;; package.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


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

  (:nicknames #:ngl)

  (:use #:cl
        #:j-utils
        #:alexandria
        #:3d-vectors
        #:3d-matrices
        )

  (:export #:display
           #:rotating-display

           #:create-keyframe-sequence
           #:create-keyframe
           #:create-rotating-viewer

           #:gl-shader
           #:gl-file-shader
           #:shader-from-file
           #:newgl-shader
           #:set-uniform
           #:draw-image
           #:buffer
           #:usage
           #:fill-textures
           #:fill-uniforms

           #:opengl-object
           #:show-info
           #:primitive-type
           #:initialize
           #:initialize-buffers
           #:initialize-shaders
           #:initialize-textures
           #:initialize-uniforms
           #:pointer
           #:add-buffer
           #:to-gl-array
           #:reload
           #:cleanup
           #:update
           #:render
           #:shaders
           #:buffers
           #:objects
           #:handle-key
           #:handle-3d-mouse-event
           #:gl-set
           #:allocate-gl-array
           #:free-gl-array

           #:attribute-buffer
           #:index-buffer

           #:viewer
           #:info-viewer

           #:st-quad
           #:plastic
           #:painted-plastic
           #:circled-plastic
           #:point-shader
           #:blend2d-texture
           #:make-st-quad
           #:simple-texture


           #:sphere
           #:psurf
           #:torus
           #:quad
           #:sombrero
           #:parametric-surface
           #:make-parametric-with-instance-data
           ))
