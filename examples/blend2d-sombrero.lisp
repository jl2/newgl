;; blend2d-sombrero.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl.examples)

(defclass blend2d-cubics (newgl:blend2d-shader) ())

(defmethod newgl:draw-image ((obj blend2d-cubics) img ctx size)
  (declare (ignorable obj img ctx size))
  (bl:with-objects
      ((path bl:path-core))
    (flet ((rp ()
             (coerce (j-utils:random-between 50.0 (max (- size 50.0) 100.0)) 'double-float)))
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
        (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
        (bl:context-set-fill-style-rgba32 ctx (random #16rffffffff))
        (bl:context-fill-path-d ctx path)))))

(defun show-blend2d-sombrero ()
  (newgl:display-in-rotating-viewer
   (make-instance 'newgl:sombrero-surface
                  :height 0.25
                  :u-steps 32
                  :v-steps 32
                  :u-min -4.0 :u-max 4.0
                  :v-min -4.0 :v-max 4.0
                  :xform (3d-matrices:mtranslation (3d-vectors:vec3  2.0 0.0 0.0))
                  :shader-program (newgl:blend2d-painted-plastic 'blend2d-cubics))
   :radius 6
   :cam-height 4
   :dt (/ 0.125 4)
   :debug t))

