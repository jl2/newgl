;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass mandelbrot (opengl-object)
  (
   (vertices :initform (make-array
                        20
                        :element-type 'single-float
                        :initial-contents '(-1.0f0  1.0f0  0.0f0 -2.0f0 1.5f0
                                            -1.0f0 -1.0f0  0.0f0 -2.0f0 -1.5f0
                                            1.0f0  1.0f0  0.0f0 2.0f0 1.5f0
                                            1.0f0 -1.0f0  0.0f0 2.0f0 1.5f0)))

   (indices :initform (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2)))

   (fill-program :initarg :fill-program
                 :initform
                       (make-instance
                        'shader-program
                        :inputs '(("position" . 3) ("uv" . 2))
                        :vertex (merge-pathnames *shader-dir* "mandel-vertex.glsl")
                        :fragment (merge-pathnames *shader-dir* "mandel-fragment.glsl"))))
  (:documentation "A Mandelbrot set."))

(defmethod rebuild-shaders ((object mandelbrot))
  (with-slots (vao fill-program) object
    (when vao
      (gl:bind-vertex-array vao)
      (build-program fill-program))))

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
  (with-slots (vbos ebos indices fill-program) object
    (when (and vbos ebos)
      (when (> (length indices) 0)
        (gl:bind-buffer :array-buffer (car vbos))
        (use-program fill-program)
        (gl:polygon-mode :front-and-back :fill)
        (gl:bind-buffer :element-array-buffer (car ebos))
        (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length indices))))))
