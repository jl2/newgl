;; blend2d-sombrero.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl.examples)

(defclass blend2d-cubics (newgl:blend2d-texture) ())

(defun random-blend2d-color (&optional alpha)
  (cond (alpha
         (+ (ash (loop for i below 3 summing (ash (random #16rff) (* i 8))) 8) alpha))
        (t
         (loop for i below 4 summing (ash (random #16rff) (* i 8))))))

(defmethod newgl:draw-image ((obj blend2d-cubics) img ctx size)
  (declare (ignorable obj img ctx size))
  (bl:with-objects
      ((path bl:path-core)
       (linear bl:linear-gradient-values)
       (grad bl:gradient-core))
    ;; (bl:context-set-fill-style-rgba32 ctx #16r000000ff)
    ;; (bl:context-fill-all ctx)
    (setf (bl:linear-gradient-values.x0 linear) 0.0d0)
    (setf (bl:linear-gradient-values.y0 linear) 0.0d0)
    (setf (bl:linear-gradient-values.x1 linear) 0.0d0)
    (setf (bl:linear-gradient-values.y1 linear) 480.0d0)

    (bl:lookup-error (bl:gradient-init-as grad
                                          bl:+gradient-type-linear+
                                          linear
                                          bl:+extend-mode-pad+ (cffi:null-pointer) 0  (cffi:null-pointer)))
    (bl:lookup-error (bl:gradient-add-stop-rgba32 grad 0.0d0 #16rffffffff))
    (bl:lookup-error (bl:gradient-add-stop-rgba32 grad 1.0d0 #16rff1f7fff))


    (bl:lookup-error (bl:path-init path))
    (bl:lookup-error (bl:path-move-to path 119.0d0 49.0d0))
    (bl:lookup-error (bl:path-cubic-to path
                                       259.0d0 29.0d0
                                       99.0d0 279.0d0
                                       275.0d0 267.0d0))
    (bl:lookup-error (bl:path-cubic-to path
                                       537.0d0 245.0d0
                                       300.0d0 -170.0d0
                                       274.0d0 430.0d0))

    (bl:lookup-error (bl:context-set-comp-op ctx bl:+comp-op-src-over+))
    (bl:lookup-error (bl:context-set-stroke-style-object ctx grad))
    (bl:lookup-error (bl:context-set-stroke-width ctx 15.0d0))
    (bl:lookup-error (bl:context-set-stroke-cap ctx bl:+stroke-cap-position-start+ bl:+stroke-cap-round+))
    (bl:lookup-error (bl:context-set-stroke-cap ctx bl:+stroke-cap-position-end+ bl:+stroke-cap-butt+))
    #+sbcl(sb-int:with-float-traps-masked (:invalid) (bl:lookup-error (bl:context-stroke-geometry ctx bl:+geometry-type-path+ path)))
    #-sbcl (bl:lookup-error (bl:context-stroke-geometry ctx bl:+geometry-type-path+ path))
    
    ;; (flet ((rp ()
    ;;          (coerce (j-utils:random-between (* 0.0 size) (* 1.0 size)) 'double-float)))
    ;;   (dotimes (n 20)
    ;;     (bl:path-init path)

    ;;     (let ((ox (rp))
    ;;           (oy (rp)))
    ;;       (bl:path-move-to path ox oy)

    ;;       (dotimes (i (+ 2 (random 3)))
    ;;         (bl:path-line-to path (rp) (rp)))
    ;;       (bl:path-line-to path ox oy))

    ;;     (bl:path-cubic-to path
    ;;                       (rp) (rp)
    ;;                       (rp) (rp)
    ;;                       (rp) (rp))
    ;;     (bl:path-cubic-to path
    ;;                       (rp) (rp)
    ;;                       (rp) (rp)
    ;;                       (rp) (rp))

    ;;     (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
    ;;     (bl:context-set-stroke-style-rgba32 ctx #16rff0000ff)
    ;;     (bl:context-stroke-path-d ctx path)
    ;;     (bl:context-set-fill-style-rgba32 ctx (random-blend2d-color))
    ;;     (bl:context-fill-path-d ctx path)

    ;;     ))
    ))

(defun show-blend2d-sombrero ()
  (make-instance 'newgl:sombrero
                 :height 0.25
                 :u-min -6.0 :u-max 6.0
                 :u-steps 80
                 :v-min -6.0 :v-max 6.0
                 :v-steps 80
                 :shaders (newgl:painted-plastic)
                 :textures (list (make-instance 'blend2d-cubics))))

(defun create-blend2d-tori ()
   (make-instance 'newgl:torus
                  :inner 0.5
                  :outer 1.65
                  :u-steps 16
                  :v-steps 16
                  ;; :xform (3d-matrices:m* (3d-matrices:mtranslation
                  ;;                         (3d-vectors:vec3 (- (* 3 i 1.0) 9)
                  ;;                                          0.0
                  ;;                                          (- (* 3 j 1.0) 9)))
                  ;;                        (3d-matrices:mrotation 3d-vectors:+vy+ (* 0.25 pi j)))
                  :shaders (newgl:painted-plastic)
                  :textures (list (make-instance 'blend2d-cubics :size 256))))

(defun create-blend2d-sphere ()
   (make-instance 'newgl:sphere
                  :radius 1.0
                  :u-steps 4
                  :v-steps 3
                  ;; :xform (3d-matrices:m* (3d-matrices:mtranslation
                  ;;                         (3d-vectors:vec3 (- (* 3 i 1.0) 9)
                  ;;                                          0.0
                  ;;                                          (- (* 3 j 1.0) 9)))
                  ;;                        (3d-matrices:mrotation 3d-vectors:+vy+ (* 0.25 pi j)))
                  :shaders (newgl:simple-texture)
                  :textures (list (make-instance 'blend2d-cubics :size 256))))


