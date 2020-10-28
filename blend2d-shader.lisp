;; blend2d-shader.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass blend2d-shader (textured-shader)
  ((size :initarg :size :initform 2048)))

(defgeneric draw-image (obj img ctx size))

(defmethod draw-image ((obj blend2d-shader) img ctx size)
  (declare (ignorable obj img ctx size))
  (bl:context-set-fill-style-rgba32 ctx #16r000000ff)
  (bl:context-fill-all ctx)

  (bl:with-objects
      ((circle bl:circle))
    (dotimes (idx 200)
      (dotimes (i size)
        (let* ((sx i)
               (sy (+ (* (/ size 2) (sin (/ i 30)))
                      (/ size 2)
                      (* idx 20)
                      (random (/ size 100.0))))
               (radius (random 5.0d0)))

          (setf (bl:circle.cx circle) (coerce sx 'double-float))
          (setf (bl:circle.cy circle) (coerce sy 'double-float))
          (setf (bl:circle.r circle) (coerce radius 'double-float))
          (bl:lookup-error (bl:context-set-comp-op ctx bl:+comp-op-src-over+))
          (bl:lookup-error (bl:context-set-fill-style-rgba32 ctx (random #16rffffffff)))
          (bl:lookup-error (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle)))))))



(defmethod fill-texture ((obj blend2d-shader))
  (with-slots (size tex-type textures) obj
    (bl:with-memory-image-context*
        (img ctx :width size :height size)
        ((data bl:image-data))
      (draw-image obj img ctx size)
      (bl:image-get-data img data)
      (gl:tex-image-2d tex-type 0 :rgba size size 0 :rgba :unsigned-byte (bl:image-data.pixel-data data))
      (gl:generate-mipmap tex-type))))

(defun blend2d-painted-plastic ( &key (shader 'blend2d-shader) (size 1024))
  (make-instance shader
                 :size size
                 :shaders (list
                           (shader-from-file (newgl-shader "painted-plastic-vertex.glsl"))
                           (shader-from-file (newgl-shader "painted-plastic-fragment.glsl")))))
