;; texture.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass texture ()
  ((tex-type :initform :texture-2d :initarg :type)
   (textures :initform nil :type (or null list))
   (parameters :initform '((:texture-wrap-s . :repeat)
                           (:texture-wrap-t . :repeat)
                           (:texture-base-level . 0)
                           (:texture-max-level . 8)
                           (:texture-min-filter . :linear-mipmap-linear)
                           (:texture-mag-filter . :linear)))))



(defgeneric fill-texture (obj))

(defmethod fill-texture ((obj texture))
  (with-slots (size tex-type textures) obj
    (let ((color (allocate-gl-array :unsigned-byte 4)))
      (gl-set color 0 255 :unsigned-byte)
      (gl:tex-image-2d tex-type 0 :rgba 1 1 0 :rgba :unsigned-byte color)
      (gl:generate-mipmap tex-type))))


(defmethod bind-buffers ((object texture))
  (with-slots (textures) object
    (when textures
      (gl:bind-texture :texture-2d (car textures)))))

(defmethod fill-buffers ((object texture))
  (with-slots (parameters tex-type textures) object
    (when textures
      (error "fill-buffers called twice!"))
    (setf textures (gl:gen-textures 1))
    (gl:bind-texture tex-type (car textures))
    (dolist (param parameters)
      (gl:tex-parameter tex-type (car param) (cdr param)))
    (fill-texture object)))

(defmethod cleanup ((obj texture))
  "Delete a shader on the GPU."
  (with-slots (textures tex-type) obj
    (when textures
      (gl:bind-texture tex-type 0)
      (gl:delete-textures textures)
      (setf textures nil))))

(defun painted-plastic ()
  (list
   (shader-from-file (newgl-shader "uv-normal-position-vertex.glsl"))
   (shader-from-file (newgl-shader "textured-plastic-fragment.glsl"))))
