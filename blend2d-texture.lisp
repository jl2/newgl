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
  (bl:context-set-fill-style-rgba32 ctx #16r000000ff)
  (bl:context-fill-all ctx)

  (bl:with-objects
      ((circle bl:circle))
    (dotimes (i 80)
      (let* ((sx (random size))
             (sy (random size))
             (radius (random 70.0)))

        (setf (bl:circle.cx circle) (coerce sx 'double-float))
        (setf (bl:circle.cy circle) (coerce sy 'double-float))
        (setf (bl:circle.r circle) (coerce radius 'double-float))
        (bl:lookup-error (bl:context-set-comp-op ctx bl:+comp-op-src-over+))
        (bl:lookup-error (bl:context-set-fill-style-rgba32 ctx (random #16rffffffff)))
        (bl:lookup-error (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle))))))

(defmethod fill-texture ((obj blend2d-texture))
  (with-slots (size tex-type textures) obj
    (bl:with-memory-image-context*
        (img ctx :width size :height size)
        ((data bl:image-data))
      (draw-image obj img ctx size)
      (bl:image-get-data img data)
      (gl:tex-image-2d tex-type 0 :rgba size size 0 :rgba :unsigned-byte (bl:image-data.pixel-data data))
      (gl:generate-mipmap tex-type))))
