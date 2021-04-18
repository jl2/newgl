;; complex-window.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl.fractals)

(defclass fractal-viewer (viewer)
  ())

#+spacenav(defmethod newgl:handle-3d-mouse-event ((object fractal-viewer) (event sn:motion-event))
  (loop for obj in (newgl:objects object) do
    (newgl:handle-3d-mouse-event obj event)))

#+spacenav(defmethod newgl:handle-3d-mouse-event ((object complex-window) (event sn:motion-event))
  (with-slots (sn:x sn:z sn:y) event
    (let ((zoom-in-percent (+ 1.0f0 (/ sn:y 5000.0)))
          (xm (/ sn:x 5000.0))
          (ym (/ sn:z 5000.0))
          (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size))))
      (zoom-complex-fractal-window zoom-in-percent window-center object)
      (pan-complex-fractal-window (complex xm ym) object)))
  (update-bounds object))

(defmethod newgl:handle-key ((object complex-window) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (let* ((pan-offset 0.025)
         (zoom-in-percent 1.05)
         (zoom-out-percent 0.95)
         (iter-up-percent 1.10)
         (iter-down-percent 0.90)
         (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size)))
         (need-reload
          (cond ((and (eq key :f5) (eq action :press))
                 (with-slots (center radius) object
                   (setf center  #C(0.0 0.0))
                   (setf radius  #C(4.0 4.0))))

                ((and (eq key :page-down)  (or (eq action :press) (eq action :repeat)))
                 (zoom-complex-fractal-window zoom-in-percent window-center object))

                ((and (eq key :page-down)  (eq action :release))
                 (zoom-complex-fractal-window zoom-in-percent window-center object))

                ((and (eq key :page-up)  (or (eq action :press) (eq action :repeat)))
                 (zoom-complex-fractal-window zoom-out-percent window-center object))

                ((and (eq key :page-up)  (eq action :release))
                 (zoom-complex-fractal-window zoom-out-percent window-center object))

                ((and (eq key :down) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex 0.0 (- pan-offset)) object))

                ((and (eq key :up) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex 0.0 pan-offset) object))

                ((and (eq key :equal) (or (eq action :press) (eq action :repeat)))
                 (with-slots ( max-iterations ) object
                   (setf max-iterations (max 1 (1+ (floor (* max-iterations iter-up-percent)))))
                   (set-uniform object "maxIterations" max-iterations :int)))

                ((and (eq key :minus) (or (eq action :press) (eq action :repeat)))
                 (with-slots ( max-iterations ) object
                   (setf max-iterations (max 1 (floor (* max-iterations iter-down-percent))))
                   (set-uniform object "maxIterations" max-iterations :int)))

                ((and (eq key :left) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex (- pan-offset) 0.0) object))

                ((and (eq key :right) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex pan-offset 0.0) object))

                (t
                 (call-next-method)
                 nil))))
    (when need-reload
      (update-bounds object)
      t)))

(defclass complex-fractal-click (mouse-click)
  ((window :initarg :window)))

;; (defmethod newgl:handle-drag ((object complex-fractal) window (click complex-fractal-click) cursor-pos)
;;   (declare (ignorable window))

;;   (with-slots (newgl:vertices zoom-window) object
;;     (with-slots (center radius) zoom-window
;;       (incf center (- (cursor-position-to-complex (slot-value newgl:*previous-mouse-drag* 'newgl:cursor-pos)
;;                                                   zoom-window)
;;                       (cursor-position-to-complex cursor-pos zoom-window))))
;;     (setf newgl:vertices (to-vertices zoom-window))
;;     (newgl:initialize object)
;;     (with-slots (newgl:cursor-pos newgl:mod-keys newgl:action newgl:button newgl:time) click
;;     (setf newgl:*previous-mouse-drag* (make-instance 'complex-fractal-click
;;                                                      :window zoom-window
;;                                                      :cursor-pos cursor-pos
;;                                                      :mod-keys newgl:mod-keys
;;                                                      :action newgl:action
;;                                                      :button newgl:button
;;                                                      :time newgl:time)))))


;; (defmethod handle-click ((object complex-window) window click)
;;   (with-slots (zoom-window) object
;;     (with-slots (cursor-pos mod-keys action button time) click
;;       (let ((mp (make-instance 'complex-fractal-click
;;                                               :window zoom-window
;;                                               :cursor-pos cursor-pos
;;                                               :mod-keys newgl:mod-keys
;;                                               :action newgl:action
;;                                               :button newgl:button
;;                                               :time newgl:time)))
;;         (cond ((eq newgl:action :press)
;;                (setf newgl:*previous-mouse-drag* mp)
;;                (setf newgl:*mouse-press-info* mp)
;;                (setf newgl:*mouse-release-info* nil))

;;               ((eq newgl:action :release)
;;                (setf newgl:*previous-mouse-drag* nil)
;;                (setf newgl:*mouse-press-info* nil)
;;                (setf newgl:*mouse-release-info* mp)))
;;         t))))

(defmethod handle-scroll ((object complex-window) window cpos x-scroll y-scroll)
  (declare (ignorable window x-scroll y-scroll))
  (zoom-complex-fractal-window (if (< 0 y-scroll)
                                   0.95
                                   1.05)
                               cpos
                               object)
  (let* ((win-size (glfw:get-window-size))
         (cur-width (car win-size))
         (cur-height (cadr win-size)))
    (glfw:set-cursor-position (coerce (/ cur-width 2.0) 'double-float)
                              (coerce (/ cur-height 2.0) 'double-float)))
  (newgl:initialize object)
  t)

;; (defun show-fractals ()
;;   (newgl:display (newgl:make-st-quad
;;                   :s-min -2.0f0 :t-min -1.8f0
;;                   :s-max 2.0f0 :t-max 1.8f0
;;                   :shaders (newgl:julia-set-shaders :max-iterations 1000 :real 0.35453458392313 :imag 0.393437674)))
;;   (newgl:display (newgl:make-st-quad
;;                   :s-min -2.0f0 :t-min -1.8f0
;;                   :s-max 2.0f0 :t-max 1.8f0
;;                   :shaders (newgl:mandelbrot-set-shaders :max-iterations 1000)))
;;   (newgl:display (newgl:make-st-quad
;;                   :s-min -2.0f0 :t-min -1.8f0
;;                   :s-max 2.0f0 :t-max 1.8f0
;;                   :shaders (newgl:burning-ship-shaders :max-iterations 1000)))
;;   (newgl:display (newgl:make-st-quad
;;                   :s-min -2.0f0 :t-min -1.8f0
;;                   :s-max 2.0f0 :t-max 1.8f0
;;                   :shaders (newgl:bs-js-shaders :max-iterations 1000 :real 0.35453458392313 :imag 0.393437674))))
