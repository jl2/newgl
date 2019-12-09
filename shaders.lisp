;;;; shaders.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defparameter *shader-dir* (asdf:system-relative-pathname :newgl "shaders/"))

(defclass gl-shader ()
  ((layout :initarg :layout :initform nil :type (or null list))
   (shader :initform 0 :type fixnum)
   (source-file :initarg :source-file :initform "" :type (or pathname string))
   (shader-type :initarg :shader-type))
  (:documentation "An opengl shader class."))

(defgeneric compile-shader (shader)
  (:documentation "Read source from source file and compile shader"))

(defclass shader-program ()
  ((shaders :initarg :shaders :initform nil :type (or null list))
   (program :initform 0))
  (:documentation "An opengl shader program."))


(defgeneric build-shader-program (program)
  (:documentation "Build a shader program and all of its corresponding programs."))

(defgeneric use-shader-program (shader-program)
  (:documentation "Set uniform and layout variables."))


(defmethod cleanup ((shader gl-shader))
  (with-slots (shader) shader
    (when (> 0 shader)
      (gl:delete-shader shader))
    (setf shader 0)))

(defmethod compile-shader ((shader gl-shader))
  (with-slots (shader source-file shader-type) shader
    (when (zerop shader)
      (setf shader (gl:create-shader shader-type)))
    (gl:shader-source shader (read-file source-file))
    (gl:compile-shader shader)
    (when (not (eq t (gl:get-shader shader :compile-status)))
      (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
      (format t "info-log ~a~%" (gl:get-shader-info-log shader)))))

(defmethod cleanup ((obj shader-program))
  "Delete a shader on the GPU."
  (with-slots (shaders program) obj
    (dolist (shader shaders)
      (cleanup shader))
    (when (> 0 program)
      (gl:delete-program program))
    (setf program 0)))

(defmethod build-shader-program ((program shader-program))
  (with-slots (shaders program) program
    (dolist (gl-shader shaders)
      (with-slots (shader) gl-shader
        (when (and (not (zerop shader)) (not (zerop program)))
          (gl:detach-shader program shader))
        (compile-shader gl-shader)))
    (when (zerop program)
      (setf program (gl:create-program)))
    (dolist (shader shaders)
      (with-slots (shader) shader
        (gl:attach-shader program shader)))
    (gl:link-program program)
    (let ((status (gl:get-program program :link-status)))
      (when (not (eq status t))
        (format t "link-program: ~a~%~a~%" status (gl:get-program-info-log program))))
    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq status t))
        (format t "validate-program: ~a~%~a~%" status (gl:get-program-info-log program))))))

(defun use-layout (program layout)
  (loop
     with stride = (loop for entry in layout summing
                        (* (assoc-value entry :count)
                           (cffi:foreign-type-size (assoc-value entry :type))))

     for cur-offset = 0 then (incf cur-offset entry-count)

     for entry in layout
     for entry-count = (assoc-value entry :count)
     for entry-type = (assoc-value entry :type)
     for entry-name = (assoc-value entry :name)
     for count from 0
     do
       (let* ((entry-offset cur-offset)
              (entry-attrib (gl:get-attrib-location program entry-name))
              (type-size (cffi:foreign-type-size (assoc-value entry :type))))
         (when (>= entry-attrib 0)
           (gl:enable-vertex-attrib-array entry-attrib)
           (gl:vertex-attrib-pointer count
                                     entry-count
                                     entry-type
                                     :false
                                     stride
                                     (* entry-offset type-size))))))

(defmethod use-shader-program ((shader-program shader-program))
  (with-slots (program shaders) shader-program
    (dolist (shader shaders)
      (with-slots (layout) shader
        (when layout
          (use-layout program layout))))))

(defmethod use-shader-program :after ((shader-program shader-program))
  (with-slots (program) shader-program
    (gl:use-program program)))

