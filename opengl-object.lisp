;;;; opengl-object.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (vbos :initform nil :type (or null cons))
   (ebos :initform nil :type (or null cons))
   (transformation :initform (meye 4) :type mat4))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defun point-ebo (ebos)
  (car ebos))

(defun line-ebo (ebos)
  (cadr ebos))

(defun triangle-ebo (ebos)
  (caddr ebos))

(defun filled-ebo (ebos)
  (cadddr ebos))


(defgeneric fill-buffers (object))
(defgeneric render (object))

(defgeneric rebuild-shaders (object))
(defmethod rebuild-shaders ((object opengl-object)))

(defmethod fill-buffers ((object opengl-object))
  (with-slots (vao) object
    (when (= 0 vao)
      (setf vao (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao)))

(defmethod fill-buffers :after ((object opengl-object))
  (gl:bind-vertex-array 0))

(defmethod cleanup ((object opengl-object))
  (declare (ignorable object))
  (format t "Cleanuing up ~a~%" object)
  (with-slots (vao vbos ebos) object
    (when (/= 0 vao)
      (when vbos
        (gl:delete-buffers vbos))
      (when ebos
        (gl:delete-buffers ebos))
      (gl:delete-vertex-arrays (list vao)))
    (setf vao 0)
    (setf vbos nil)))

(defmethod render ((object opengl-object))
  (with-slots (vao transformation) object
    (when (/= 0 vao)
      (gl:bind-vertex-array vao))))

(defmethod render :after ((object opengl-object))
  (gl:bind-vertex-array 0))

(defun to-gl-float-array (arr)
  "Create an OpenGL float array from a CL array of numbers.
   This is a convenience function that will coerce array elments to single-float."
  (declare (optimize (speed 3))
           (type (vector single-float) arr))
  (let* ((count (length arr))
         (gl-array (gl:alloc-gl-array :float count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (coerce (aref arr i) 'single-float)))
    gl-array))

(defun to-gl-array (arr type)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3))
           (type (vector single-float) arr))
  (let* ((count (length arr))
         (gl-array (gl:alloc-gl-array type count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (aref arr i)))
    gl-array))

