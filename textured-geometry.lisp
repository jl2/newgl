;; textured-geometry.lisp
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

(in-package :newgl)

(defclass textured-geometry (geometry)
  ((textures :initform nil))
  (:documentation "Base class for all objects that can be rendered in a scene."))


(defgeneric transfer-texture (object width height
                              data
                              &key
                                target
                                level
                                internal-format
                                border
                                format
                                type
                                raw))

(defmethod transfer-texture ((object textured-geometry) width height data
                             &key (target :texture-2d)
                               (level 0)
                               (internal-format :rgba)
                               (border 0)
                               (format :rgba)
                               (type :unsigned-byte)
                               (raw))
  (declare (ignorable object))
  (gl:tex-image-2d target level internal-format  width height  border format type data :raw raw)
  (gl:generate-mipmap :texture-2d))

(defgeneric draw-texture (obj))

(defmethod draw-texture ((obj textured-geometry))
  ;; Draw an image and call transfer-texture
  (transfer-texture obj 2 2 #(0 0 0 255 0 0 0 0 255 0 0)))

(defmethod fill-buffers ((object textured-geometry))
  (call-next-method)
  (with-slots (textures) object
    (when textures
      (error "fill-buffers called twice!"))
    (setf textures (gl:gen-textures 1))
    (gl:bind-texture :texture-2d (car textures))
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-base-level 0)
    (gl:tex-parameter :texture-2d :texture-max-level 8)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (draw-texture object)))

(defmethod newgl:bind-buffers ((object textured-geometry))
  (call-next-method)
  (with-slots (textures) object
    (gl:bind-texture :texture-2d (car textures))))

(defmethod newgl:cleanup ((object textured-geometry))
  (with-slots (textures) object
    (when textures
      (gl:bind-texture :texture-2d 0)
      (gl:delete-textures textures)
      (setf textures nil)))
  (call-next-method))
