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
             (gl:buffer-data :array-buffer :static-draw gl-vertices)
             (gl:free-gl-array gl-vertices)

             (gl:bind-buffer :element-array-buffer (car ebos))
             (gl:buffer-data :element-array-buffer :static-draw gl-indices)
             (gl:free-gl-array gl-indices)))
          (t
           (gl:bind-buffer :array-buffer (car vbos))
           (gl:bind-buffer :element-array-buffer (car ebos))))))

(defmethod render ((object mandelbrot))
  (call-next-method)
  (with-slots (vbos ebos indices shader-program) object
    (when (and vbos ebos)
      (when (> (length indices) 0)
        (gl:bind-buffer :array-buffer (car vbos))
        (use-shader-program shader-program)
        (gl:polygon-mode :front-and-back :fill)
        (gl:bind-buffer :element-array-buffer (car ebos))
        (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length indices))))))
