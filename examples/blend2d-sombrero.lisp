;; blend2d-sombrero.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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
      ((path bl:path-core))
    (bl:context-set-fill-style-rgba32 ctx #16r000000ff)
    (bl:context-fill-all ctx)
    (flet ((rp ()
             (coerce (j-utils:random-between (* 0.0 size) (* 1.0 size)) 'double-float)))
      (dotimes (n 20)
        (bl:path-init path)

        (let ((ox (rp))
              (oy (rp)))
          (bl:path-move-to path ox oy)

          (dotimes (i (+ 2 (random 3)))
            (bl:path-line-to path (rp) (rp)))
          (bl:path-line-to path ox oy))

        (bl:path-cubic-to path
                          (rp) (rp)
                          (rp) (rp)
                          (rp) (rp))
        (bl:path-cubic-to path
                          (rp) (rp)
                          (rp) (rp)
                          (rp) (rp))

        (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
        (bl:context-set-stroke-style-rgba32 ctx #16rff0000ff)
        (bl:context-stroke-path-d ctx path)
        (bl:context-set-fill-style-rgba32 ctx (random-blend2d-color))
        (bl:context-fill-path-d ctx path)

        ))))

(defun show-blend2d-sombrero ()
  (newgl:rotating-display
   (make-instance 'newgl:sombrero
                  :height 0.25
                  :u-min -6.0 :u-max 6.0
                  :u-steps 80
                  :v-min -6.0 :v-max 6.0
                  :v-steps 80
                  :shaders (newgl:painted-plastic)
                  :textures (list (make-instance 'blend2d-cubics)))
   :radius 16
   :cam-height 8
   :dt (/ 0.125 12)))

(defun show-blend2d-tori ()
  (newgl:rotating-display
   (let ((objects ()))
     (loop for i below 7 do
           (loop for j below 7 do
                 (push (make-instance 'newgl:torus
                                      :inner 0.25
                                      :outer 0.65
                                      :u-steps 16
                                      :v-steps 16
                                      :xform (3d-matrices:m* (3d-matrices:mtranslation
                                                              (3d-vectors:vec3 (- (* 3 i 1.0) 9)
                                                                               0.0
                                                                               (- (* 3 j 1.0) 9)))
                                                             (3d-matrices:mrotation 3d-vectors:+vy+ (* 0.25 pi j)))
                                      :shaders (newgl:painted-plastic)
                                      :textures (list (make-instance 'blend2d-cubics :size 256)))
                       objects))
           )
     objects)
   :radius 16
   :cam-height 12
   :dt (/ 0.125 4)))


