;; texture.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(defmethod bind ((object texture))
  (with-slots (tex-type textures) object
    (when textures
      (gl:bind-texture tex-type (car textures)))))

(defmethod initialize ((tex texture) &key)
  (with-slots (textures) tex
    (when textures
      (error "Initializing texture twice ~a" tex))
    (setf textures (gl:gen-textures 1))
    (fill-texture tex)))

(defmethod fill-texture ((object texture))
  (with-slots (parameters tex-type textures) object
    (gl:bind-texture tex-type (car textures))
    (dolist (param parameters)
      (gl:tex-parameter tex-type (car param) (cdr param)))
    (gl:tex-image-2d tex-type 0 :rgba 1 1 0 :rgba :unsigned-byte #(255 0 0 255))
    (gl:generate-mipmap tex-type)))

(defmethod cleanup ((obj texture))
  "Delete a shader on the GPU."
  (with-slots (textures tex-type) obj
    (when textures
      (gl:bind-texture tex-type 0)
      (gl:delete-textures textures)
      (setf textures nil))))
