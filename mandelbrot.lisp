;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass mandel-vertex-shader (gl-shader)
  ((layout :initform
           '(((:name . "position")
              (:count . 3)
              (:type . :float))

             ((:name . "uv")
              (:count . 2)
              (:type . :float)))
           :type (or null list))

   (shader :initform 0 :type fixnum)
   (source-file :initform (merge-pathnames *shader-dir* "mandel-vertex.glsl") :type (or pathname string))
   (shader-type :initform :vertex-shader)))

(defclass mandel-fragment-shader (gl-shader)
  ((shader-type :initform :fragment-shader)
   (source-file :initform (merge-pathnames *shader-dir* "mandel-fragment.glsl"))))

(defclass mandel-program (shader-program)
  ((shaders :initform (list
                       (make-instance 'mandel-vertex-shader)
                       (make-instance 'mandel-fragment-shader)))))

(defclass complex-window ()
  ((real-min :initarg :real-min :initform -2.5)
   (real-max :initarg :real-max :initform 1.0)
   (imag-min :initarg :image-min :initform -1.5)
   (imag-max :initarg :image-max :initform 1.5))
  (:documentation "A rectangular region in the complex plain."))

(defun from-center-radius (real-center imag-center real-radius imag-radius)
  (make-instance 'complex-window
                 :real-min (- real-center real-radius)
                 :real-max (+ real-center real-radius)
                 :imag-min (- imag-center imag-radius)
                 :imag-max (+ imag-center imag-radius)))

(defun compute-center-radius (window)
  (with-slots (real-min real-max imag-min imag-max) window
    (let ((real-center (+ real-min (- real-max real-min)))
          (imag-center (+ imag-min (- imag-max imag-min)))
          (real-radius (/ (- real-max real-min) 2))
          (imag-radius (/ (- imag-max imag-min) 2)))
      (values  real-center imag-center real-radius imag-radius))))

(defun to-vertices (window)
  (with-slots (real-min real-max imag-min imag-max) window
    (make-array
     20
     :element-type 'single-float
     :initial-contents (list
                        -1.0f0  1.0f0  0.0f0 (coerce real-min 'single-float) (coerce imag-max 'single-float)
                        -1.0f0 -1.0f0  0.0f0 (coerce real-min 'single-float) (coerce imag-min 'single-float)

                        1.0f0  1.0f0  0.0f0 (coerce real-max 'single-float) (coerce imag-max 'single-float)
                        1.0f0 -1.0f0  0.0f0 (coerce real-max 'single-float) (coerce imag-min 'single-float)))))

(defclass mandelbrot (opengl-object)

  ((vertices :initarg :vertices)
   (zoom-window :initarg :zoom-window)
   (indices :initform (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2)))

   (shader-program :initform (make-instance 'mandel-program)))

  (:documentation "A Mandelbrot set."))

(defun make-mandelbrot (&key (window (make-instance 'complex-window)))
  (with-slots (real-min real-max imag-min imag-max) window
    (make-instance 'mandelbrot
                   :vertices (to-vertices window)
                   :zoom-window window)))

(defmethod rebuild-shaders ((object mandelbrot))
  (call-next-method)
  (with-slots (shader-program) object
    (build-shader-program shader-program)))

(defmethod fill-buffers ((object mandelbrot))
  (call-next-method)
  (with-slots (vao vbos ebos vertices indices) object
    (cond ((null vbos)
           (setf vbos (gl:gen-buffers 1))
           (setf ebos (gl:gen-buffers 1))
           (let ((gl-vertices (to-gl-float-array vertices))
                 (gl-indices (to-gl-array indices :unsigned-int)))

             (gl:bind-buffer :array-buffer (car vbos))
             (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
             (gl:free-gl-array gl-vertices)

             (gl:bind-buffer :element-array-buffer (car ebos))
             (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
             (gl:free-gl-array gl-indices)))
          (t
           (gl:bind-buffer :array-buffer (car vbos))
           (gl:bind-buffer :element-array-buffer (car ebos))))))

(defmethod render ((object mandelbrot))
  (with-slots (indices) object
    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length indices))))

(defmethod handle-key ((object mandelbrot) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (cond
    ((and (eq key :f5) (eq action :press))
     (with-slots (vertices zoom-window) object
       (setf zoom-window (make-instance 'complex-window))
       (setf vertices (to-vertices zoom-window))
       (setf *refill-buffers* t)
       t))
    ((and (eq key :m) (eq action :press))
     (with-slots (vertices) object
       (format t "Mandelbrot set vertices:~%~a~%" vertices))
     t)
    ((and (eq key :page-down) (eq action :press))
     (with-slots (vertices) object
       (format t "Mandelbrot set vertices:~%~a~%" vertices))
     t)
    (t
     nil)))


(defclass mandelbrot-click (mouse-click)
  ((window :initarg :window)))

(defmethod handle-drag ((object mandelbrot) window (click mandelbrot-click) current-pos)
  (declare (ignorable window))
  (with-slots (zoom-window) object
    (with-slots (real-min real-max imag-min imag-max) zoom-window
      (with-slots (cpos mod-keys action button time) click
        (with-slots (vertices) object
          (let* (
                 (win-size (glfw:get-window-size))
                 (cur-width (car win-size))
                 (cur-height (cadr win-size))

                 (old-x-pos (car cpos))
                 (old-y-pos (cadr cpos))
                 (new-x-pos (car current-pos))
                 (new-y-pos (cadr current-pos))

                 (new-real-mouse (ju:map-val old-x-pos 0.0 cur-width real-min real-max))
                 (new-imag-mouse (ju:map-val (- cur-height old-y-pos) 0.0 cur-height imag-min imag-max))

                 (old-real-mouse (ju:map-val new-x-pos 0.0 cur-width real-min real-max))
                 (old-imag-mouse (ju:map-val (- cur-height new-y-pos) 0.0 cur-height imag-min imag-max))

                 (mouse-real-diff (- new-real-mouse old-real-mouse))
                 (mouse-imag-diff (- new-imag-mouse old-imag-mouse))

                 (new-real-min (coerce (+ mouse-real-diff real-min) 'single-float))
                 (new-real-max (coerce (+ mouse-real-diff real-max) 'single-float))
                 (new-imag-min (coerce (+ mouse-imag-diff imag-min) 'single-float))
                 (new-imag-max (coerce (+ mouse-imag-diff imag-max) 'single-float)))
            (setf vertices (make-array
                            20
                            :element-type 'single-float
                            :initial-contents (list
                                               -1.0f0  1.0f0  0.0f0 new-real-min new-imag-max
                                               -1.0f0 -1.0f0  0.0f0 new-real-min new-imag-min
                                               1.0f0  1.0f0  0.0f0 new-real-max new-imag-max
                                               1.0f0 -1.0f0  0.0f0 new-real-max new-imag-min)))
            (setf real-min new-real-min
                  real-max new-real-max
                  imag-min new-imag-min
                  imag-max new-imag-max)
            (reload-object object)
            (setf *previous-mouse-drag* (make-instance 'mandelbrot-click
                                                       :window zoom-window
                                                       :cpos current-pos
                                                       :mod-keys mod-keys
                                                       :action action
                                                       :button button
                                                       :time time)))
            t)))))

(defmethod handle-click ((object mandelbrot) window click)
  (declare (ignorable window))
  (with-slots (zoom-window) object
    (with-slots (cpos mod-keys action button time) click
      (let ((mp (make-instance 'mandelbrot-click
                                              :window zoom-window
                                              :cpos cpos
                                              :mod-keys mod-keys
                                              :action action
                                              :button button
                                              :time time)))
        (cond ((eq action :press)
               (setf *previous-mouse-drag* mp)
               (setf *mouse-press-info* mp)
               (setf *mouse-release-info* nil))

              ((eq action :release)
               (setf *previous-mouse-drag* nil)
               (setf *mouse-press-info* nil)
               (setf *mouse-release-info* mp)))
        t))))

(defmethod handle-scroll ((object mandelbrot) window cpos x-scroll y-scroll)
  (declare (ignorable window x-scroll y-scroll))
  (let* ((x-pos (car cpos))
        (y-pos (cadr cpos))
        (win-size (glfw:get-window-size))
        (cur-width (car win-size))
        (cur-height (cadr win-size)))
    (with-slots (vertices zoom-window) object
      (with-slots (real-min real-max imag-min imag-max) zoom-window
        (let* (
               (real-diff (- real-max real-min))
               (imag-diff (- imag-max imag-min))

               (mag (if (< 0 y-scroll)
                        0.95
                        1.05))

               (new-real-diff (* mag 0.5 real-diff))
               (new-imag-diff (* mag 0.5 imag-diff))

               (real-mouse (ju:map-val x-pos 0.0 cur-width real-min real-max))
               (imag-mouse (ju:map-val (- cur-height y-pos) 0.0 cur-height imag-min imag-max))

               (new-real-min (coerce (- real-mouse new-real-diff) 'single-float))
               (new-real-max (coerce (+ real-mouse new-real-diff) 'single-float))

               (new-imag-min (coerce (- imag-mouse new-imag-diff) 'single-float))
               (new-imag-max (coerce (+ imag-mouse new-imag-diff) 'single-float)))

          ;; (format t "real-min ~a real-max ~a imag-min ~a imag-max ~a real-diff ~a imag-diff ~a~%"
          ;;         real-min real-max imag-min imag-max real-diff imag-diff)
          ;; (format t "new-real-diff ~a new-imag-diff ~a~%" new-real-diff new-imag-diff)
          ;; (format t "real-mouse ~a imag-mouse ~a~%" real-mouse imag-mouse)
          ;; (format t "new-real-min ~a new-real-max ~a new-imag-min ~a new-imag-max ~a~%" new-real-min new-real-max new-imag-min new-imag-max)
          (setf real-min new-real-min
                real-max new-real-max
                imag-min new-imag-min
                imag-max new-imag-max)))
      (setf vertices (to-vertices zoom-window))
      (glfw:set-cursor-position (coerce (/ cur-width 2.0) 'double-float) (coerce (/ cur-height 2.0) 'double-float))
      (reload-object object))
  t))
