;;;; gl-shader.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defparameter *shader-dir* (asdf:system-relative-pathname :newgl "shaders/"))

(defclass layout-entry ()
  ((name :initarg :name :type string)
   (count :initarg :count :type fixnum)
   (type :initarg :type)))

(defclass layout ()
  ((entries :initform nil :type (or list null)))
  (:documentation "A shader layout."))

(defun compute-stride (layout)
  (with-slots (entries) layout
    (loop for entry in layout
       summing (* (slot-value entry 'count)
                  (cffi:foreign-type-size (slot-value entry 'type))))))

(defgeneric enable-layout (layout program))

(defmethod enable-layout ((layout layout) program)
  (with-slots (entries) layout
    (loop
       with stride = (compute-stride layout)
       for offset = 0 then (incf offset entry-count)
       for entry-count = (slot-value entry 'count)
       for entry in layout 
       do
         (with-slots (count type name) entry
           (let ((entry-attrib (gl:get-attrib-location program name))
                 (type-size  (cffi:foreign-type-size type)))
             (when (>= entry-attrib 0)
               (gl:enable-vertex-attrib-array entry-attrib)
               (gl:vertex-attrib-pointer count
                                         entry-count
                                         type
                                         :false
                                         stride
                                         (* offset type-size))))))))

(defclass gl-shader ()
  ((layout :initarg :layout :initform nil :type 'layout)
   (uniforms :initarg :uniforms :initform nil :type (or null list))
   (shader :initform 0 :type fixnum)
   (shader-type :initarg :shader-type))
  (:documentation "An opengl shader class."))

(defclass file-shader (gl-shader)
  ((source-file :initarg :source-file :type (or pathname string)))
  (:documentation "An OpenGL shader whose source code is stored in a file."))


(defgeneric compile-shader (shader)
  (:documentation "Read source from source file and compile shader"))

(defgeneric use-layout (shader program)
  (:documentation "Return shader's layout."))

(defgeneric get-source (shader)
  (:documentation "Return shader source code as a string."))


(defmethod use-layout ((shader gl-shader) program )
  (with-slots (layout) shader
    (enable-layout layout program)))

(defmethod get-source ((shader gl-shader))
  "")

(defmethod get-source ((shader file-shader))
  (with-slots (source-file) shader
    (ju:read-file source-file)))
  
(defmethod cleanup ((shader gl-shader))
  (with-slots (shader) shader
    (when (> 0 shader)
      (gl:delete-shader shader))
    (setf shader 0)))

(defmethod compile-shader ((shader gl-shader))
  (with-slots (shader shader-type) shader
    (when (zerop shader)
      (setf shader (gl:create-shader shader-type)))
    (gl:shader-source shader (get-source shader))
    (gl:compile-shader shader)
    (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
    (format t "info-log ~s~%" (gl:get-shader-info-log shader))))
