;; blend2d-sombrero.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl.examples)

(defclass blend2d-cubics (newgl:blend2d-shader) ())

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
             (coerce (j-utils:random-between -50.0 (+ size 50.0)) 'double-float)))
      (dotimes (n 10)
        (bl:path-init path)

        (bl:path-move-to path (rp) (rp))

        (bl:path-cubic-to path
                          (rp) (rp)
                          (rp) (rp)
                          (rp) (rp))
        (bl:path-cubic-to path
                          (rp) (rp)
                          (rp) (rp)
                          (rp) (rp))
        (bl:path-cubic-to path
                          (rp) (rp)
                          (rp) (rp)
                          (rp) (rp))
        (bl:path-cubic-to path
                          (rp) (rp)
                          (rp) (rp)
                          (rp) (rp))
        (bl:path-cubic-to path
                          (rp) (rp)
                          (rp) (rp)
                          (rp) (rp))
        (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
        (bl:context-set-fill-style-rgba32 ctx (random-blend2d-color))
        (bl:context-fill-path-d ctx path)))))

(defun show-blend2d-sombrero ()
  (newgl:display-in-rotating-viewer
   (make-instance 'newgl:sombrero
                  :height 0.25
                  :u-steps 128
                  :v-steps 128
                  :u-min -4.0 :u-max 4.0
                  :v-min -4.0 :v-max 4.0
                  :shader-program (newgl:blend2d-painted-plastic
                                   :size 512
                                   :shader 'blend2d-cubics))
   :radius 6
   :cam-height 4
   :dt (/ 0.125 4)
   :debug t))

(defun show-blend2d-tori ()
  (newgl:display-in-rotating-viewer
   (let ((objects ()))
     (loop for i below 7 do
           (loop for j below 7 do
                 (push (make-instance 'newgl:torus
                                      :inner 0.25
                                      :outer 0.65
                                      :u-steps 16
                                      :v-steps 16
                                      :xform (m* (3d-matrices:mtranslation (vec3 (- (* 3 i 1.0) 9)
                                                                                 0.0
                                                                                 (- (* 3 j 1.0) 9)))
                                                 (3d-matrices:mrotation +vy+ (* 0.25 pi j)))
                                      :shader-program
                                      (newgl:blend2d-painted-plastic
                                       :size 256
                                       :shader 'blend2d-cubics))
                       objects))
           )
     objects)
   :radius 12
   :cam-height 10
   :dt (/ 0.125 4)
   :debug t))


