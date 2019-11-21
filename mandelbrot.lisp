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
   (source-file :initform (merge-pathnames *shader-dir* "mandel-vertex.glsl") :type string)
   (shader-type :initform :vertex-shader)))

(defclass mandel-fragment-shader (gl-shader)
  ((shader-type :initform :fragment-shader)
   (source-file :initform (merge-pathnames *shader-dir* "mandel-fragment.glsl"))))

(defclass mandel-program (shader-program)
  ((shaders :initform (list
                       (make-instance 'mandel-vertex-shader)
                       (make-instance 'mandel-fragment-shader)))))

(defclass mandelbrot (opengl-object)

   ((vertices :initform (make-array
                        20
                        :element-type 'single-float
                        :initial-contents '(-1.0f0  1.0f0  0.0f0 -2.0f0 1.5f0
                                            -1.0f0 -1.0f0  0.0f0 -2.0f0 -1.5f0
                                            1.0f0  1.0f0  0.0f0 1.0f0 1.5f0
                                            1.0f0 -1.0f0  0.0f0 1.0f0 -1.5f0)))

   (indices :initform (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2)))

    (shader-program :initform (make-instance 'mandel-program)))

  (:documentation "A Mandelbrot set."))

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
     (with-slots (vertices) object
       (setf vertices (make-array
                       20
                       :element-type 'single-float
                       :initial-contents '(-1.0f0  1.0f0  0.0f0 -2.0f0 1.5f0
                                           -1.0f0 -1.0f0  0.0f0 -2.0f0 -1.5f0
                                           1.0f0  1.0f0  0.0f0 1.0f0 1.5f0
                                           1.0f0 -1.0f0  0.0f0 1.0f0 -1.5f0)))
       (setf *refill-buffers* t)
       t))
    ((and (eq key :m) (eq action :press))
     (with-slots (vertices) object
       (format t "Mandelbrot set vertices:~%~a~%" vertices))
     t)
    (t
     nil)))

(defmethod handle-scroll ((object mandelbrot) window x-pos y-pos x-scroll y-scroll)
  (with-slots (vertices) object
    (let* ((win-size (glfw:get-window-size))
           (cur-width (car win-size))
           (cur-height (cadr win-size))

           (real-min (aref vertices 3))
           (real-max (aref vertices 13))
           (imag-min (aref vertices 9))
           (imag-max (aref vertices 4))

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

      (format t "real-min ~a real-max ~a imag-min ~a imag-max ~a real-diff ~a imag-diff ~a~%"
               real-min real-max imag-min imag-max real-diff imag-diff)
      (format t "new-real-diff ~a new-imag-diff ~a~%" new-real-diff new-imag-diff)
      (format t "real-mouse ~a imag-mouse ~a~%" real-mouse imag-mouse)
      (format t "new-real-min ~a new-real-max ~a new-imag-min ~a new-imag-max ~a~%" new-real-min new-real-max new-imag-min new-imag-max)
      (setf vertices (make-array
                      20
                      :element-type 'single-float
                      :initial-contents (list
                                         -1.0f0  1.0f0  0.0f0 new-real-min new-imag-max
                                          -1.0f0 -1.0f0  0.0f0 new-real-min new-imag-min
                                          1.0f0  1.0f0  0.0f0 new-real-max new-imag-max
                                          1.0f0 -1.0f0  0.0f0 new-real-max new-imag-min)))
      (glfw:set-cursor-position (/ cur-width 2.0) (/ cur-height 2.0))
      (cleanup object)
      (fill-buffers object))))
