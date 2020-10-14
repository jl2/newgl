;; blend2d-shader.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass blend2d-shader (textured-shader)
  ((size :initarg :size :initform 2048)))

(defgeneric draw-image (obj img ctx size))

(defmethod draw-image ((obj blend2d-shader) img ctx size)
  (declare (ignorable obj img ctx size))
  (bl:with-objects
      ((circle bl:circle))
    (dotimes (i 2000)
        (let* ((sx (random (coerce size 'double-float)))
               (sy (random (coerce size 'double-float)))
               (radius (coerce (random (/ size 50.0)) 'double-float)))

          (setf (bl:circle.cx circle) sx)
          (setf (bl:circle.cy circle) sy)
          (setf (bl:circle.r circle) radius)
          (bl:lookup-error (bl:context-set-comp-op ctx bl:+comp-op-src-over+))
          (bl:lookup-error (bl:context-set-fill-style-rgba32 ctx (random #16rffffffff)))
          (bl:lookup-error (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle))))))



(defmethod fill-texture ((obj blend2d-shader))
  (with-slots (size tex-type textures) obj
    (bl:with-memory-image-context*
        (img ctx :width size :height size)
        ((data bl:image-data))
      (draw-image obj img ctx size)
      (bl:image-get-data img data)
      (gl:tex-image-2d tex-type 0 :rgba size size 0 :rgba :unsigned-byte (bl:image-data.pixel-data data))
      (gl:generate-mipmap tex-type))))

(defun blend2d-painted-plastic ( &optional (shader 'blend2d-shader))
  (make-shader-program shader
                       (shader-from-file (newgl-shader "painted-plastic-vertex.glsl"))
                       (shader-from-file (newgl-shader "painted-plastic-fragment.glsl"))))
