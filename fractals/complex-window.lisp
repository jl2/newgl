;; complex-window.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl.fractals)

(defclass complex-window (opengl-object)
  ((center :initarg :center :initform #C(0.0 0.0))
   (radius :initarg :radius :initform #C(4.0 4.0))
   (max-iterations :initarg :max-iterations :initform 100))
  (:documentation "A rectangular region in the complex plain."))

(defun update-bounds (object)
  (with-slots (newgl:buffers) object
    (let ((gl-array (slot-value (assoc-value newgl:buffers :array-buffer) 'newgl:pointer)))
      (multiple-value-bind (real-min real-max imag-min imag-max) (compute-min-max object)
        (gl-fset gl-array 3 real-min)
        (gl-fset gl-array 4 imag-max)
        (gl-fset gl-array 8 real-min)
        (gl-fset gl-array 9 imag-min)
        (gl-fset gl-array 13 real-max)
        (gl-fset gl-array 14 imag-max)
        (gl-fet gl-array 18 real-max)
        (gl-fset gl-array 19 imag-min)))
    (reload (assoc-value newgl:buffers :array-buffer))))

(defmethod newgl:initialize-buffers ((object complex-window) &key)
  (multiple-value-bind (real-min real-max imag-min imag-max) (compute-min-max object)
    (use-buffer object
                :vertices
                (make-instance
                 'attribute-buffer
                 :count (* 4 5)
                 :pointer (newgl:to-gl-array
                           :float
                           20
                           (list -1.0f0  1.0f0 0.0f0
                                       real-min imag-max

                                       -1.0f0 -1.0f0 0.0f0
                                       real-min imag-min

                                       1.0f0  1.0f0 0.0f0
                                       real-max imag-max

                                       1.0f0 -1.0f0 0.0f0
                                       real-max imag-min))
                 :attributes '(("in_position" . :vec3) ("in_uv" . :vec2))
                 :free t)))
  (use-buffer object
              :indices
              (make-instance 'newgl:index-buffer
                             :idx-count 6
                             :pointer (newgl:to-gl-array :unsigned-int #(0 1 2 1 3 2))
                             :free t))
  )

(defmethod newgl:initialize-uniforms ((object complex-window) &key)
  (with-slots (max-iterations) object
    (set-uniform object "maxIterations" max-iterations :int)))

(defun window-from-center-radius (center radius)
  (make-instance 'complex-window
                 :center center
                 :radius radius))

(defun window-from-min-max (&key real-min real-max imag-min imag-max)
  (let ((real-center (+ real-min (/ (- real-max real-min) 2)))
        (imag-center (+ imag-min (/ (- imag-max imag-min) 2)))
        (real-radius (/ (- real-max real-min) 2))
        (imag-radius (/ (- imag-max imag-min) 2)))
    (make-instance 'complex-window
                   :radius (complex real-radius imag-radius)
                   :center (complex real-center imag-center))))

(defun compute-min-max (window)
  "Returns (values real-min real-max imag-min imag-max)"
  (with-slots (center radius) window
    (let ((cr (realpart center))
          (ci (imagpart center))
          (rr (realpart radius))
          (ri (imagpart radius)))
      (values (- cr rr)
              (+ cr rr)
              (- ci ri)
              (+ ci ri)))))

(defun window-from-vertices (array)
  (window-from-min-max :real-min (aref array 3)
                       :real-max (aref array 13)
                       :imag-min (aref array 9)
                       :imag-max (aref array 4)))

(defun imag-min (window)
  (with-slots (center radius) window
    (- (imagpart center) (imagpart radius))))

(defun imag-max (window)
  (with-slots (center radius) window
    (+ (imagpart center) (imagpart radius))))

(defun real-min (window)
  (with-slots (center radius) window
    (- (realpart center) (realpart radius))))

(defun real-max (window)
  (with-slots (center radius) window
    (+ (realpart center) (realpart radius))))

(defun cursor-position-to-complex (cpos window)
  (let* ((x-pos (car cpos))
         (y-pos (cadr cpos))
         (win-size (glfw:get-window-size))
         (cur-width (car win-size))
         (cur-height (cadr win-size))
         (real-mouse (ju:map-val x-pos 0.0 cur-width (real-min window) (real-max window)))
         (imag-mouse (ju:map-val (- cur-height y-pos) 0.0 cur-height (imag-min window) (imag-max window))))
    (complex real-mouse imag-mouse)))

(defun zoom-complex-fractal-window (scale cpos fractal)
    (with-slots (center radius) fractal
      (let* ((new-radius (* radius scale))
             (new-center (cursor-position-to-complex cpos fractal)))
        (setf center new-center
              radius new-radius))))

(defun pan-complex-fractal-window (offset-percent fractal)
    (with-slots (radius center) fractal
      (incf center (complex (* (realpart radius) (realpart offset-percent))
                            (* (imagpart radius) (imagpart offset-percent))))))

