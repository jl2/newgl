;; blend2d-texture.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass blend2d-texture (texture)
  ((tex-type :initform :texture-2d)
   (size :initarg :size :initform 2048)))

(defgeneric draw-image (obj img ctx size))

(defmethod draw-image ((obj blend2d-texture) img ctx size)
  (declare (ignorable obj img ctx size))
  (bl:context-set-fill-style-rgba32 ctx #16r00ff00ff)
  (bl:context-fill-all ctx)
  (format t "Drawing texture...~%")
  (bl:with-objects
      ((rect bl:round-rect)
       (grad bl:gradient-core)
       (linear bl:linear-gradient-values))

    (setf (bl:linear-gradient-values.x0 linear) 0.0d0)
    (setf (bl:linear-gradient-values.y0 linear) 0.0d0)
    (setf (bl:linear-gradient-values.x1 linear) 0.0d0)
    (setf (bl:linear-gradient-values.y1 linear) 480.0d0)

    (bl:lookup-error (bl:gradient-init-as grad
                                          bl:+gradient-type-linear+
                                          linear
                                          bl:+extend-mode-pad+
                                          (bl:nullp) 0  (bl:nullp)))
    (bl:lookup-error (bl:gradient-add-stop-rgba32 grad 0.0d0 #16rffffffff))
    (bl:lookup-error (bl:gradient-add-stop-rgba32 grad 0.5d0 #16rff5fafdf))
    (bl:lookup-error (bl:gradient-add-stop-rgba32 grad 1.0d0 #16rff2f5fdf))

    (bl:lookup-error (bl:context-set-comp-op ctx bl:+comp-op-src-over+))
    (bl:lookup-error (bl:context-set-fill-style-object ctx grad))

    (setf (bl:round-rect.x rect) 40.0d0)
    (setf (bl:round-rect.y rect) 40.0d0)
    (setf (bl:round-rect.w rect) 400.0d0)
    (setf (bl:round-rect.h rect) 400.0d0)
    (setf (bl:round-rect.rx rect) 45.0d0)
    (setf (bl:round-rect.ry rect) 45.0d0)

    (bl:lookup-error (bl:context-fill-geometry ctx bl:+geometry-type-round-rect+ rect))))

(defmethod fill-texture ((obj blend2d-texture))
  (with-slots (size tex-type textures) obj
    (bl:with-memory-image-context*
        (img ctx :width size :height size)
        ((data bl:image-data))
      (draw-image obj img ctx size)
      (bl:image-get-data img data)
      (gl:tex-image-2d tex-type 0 :rgba size size 0 :rgba :unsigned-byte (bl:image-data.pixel-data data))
      (gl:generate-mipmap tex-type))))
