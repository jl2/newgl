;;;; shaders.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defparameter *shader-dir* (asdf:system-relative-pathname :newgl "shaders/"))

(defclass gl-shader ()
  ((layout :initarg layout :initform nil :type (or null list))
   (shader :initform 0 :type fixnum)
   (source-file :initform "" :type string)
   (shader-type :initarg :type))
  (:documentation "An opengl shader class."))

(defclass shader-program ()
  ((shaders :initarg :shaders :initform nil :type (or null list))
   (program :initform 0))
  (:documentation "An opengl shader program."))

(defgeneric compile-shader (shader)
  (:documentation "Read source from source file and compile shader"))

(defmethod compile-shader ((shader gl-shader))
  (with-slots (shader source-file type) shader
    (when (zerop shader)
      (setf shader (gl:create-shader type)))
    (gl:shader-source shader (read-file source-file))
    (gl:compile-shader shader)
    (when (not (eq t (gl:get-shader shader :compile-status)))
      (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
      (format t "info-log ~a~%" (gl:get-shader-info-log shader)))))


(defmethod cleanup ((shader gl-shader))
  (with-slots (shader) shader
    (when (> 0 shader)
      (gl:delete-shader shader))
    (setf shader 0)))

(defmethod cleanup ((obj shader-program))
  "Delete a shader on the GPU."
  (with-slots (shaders program) obj
    (dolist (shader shaders)
      (cleanup shader))
    (when (> 0 program)
      (gl:delete-program program))
    (setf program 0)))

(defgeneric build-shader-program (program)
  (:documentation "Build a shader program and all of its corresponding programs."))

(defmethod build-shader-program ((program shader-program))
  (with-slots (shaders program) program
    (dolist (shader shaders)
      (with-slots (shader) shader
        (if (zerop shader)
            (compile-shader shader)
            (when (not (zerop program))
              (gl:detach-shader program shader)))
        (compile-shader shader)))
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

(defgeneric use-program (shader-program)
  (:documentation "Set uniform and layout variables."))

(defmethod use-program (shader-program))

(defmethod use-program :after ((shader-program shader-program))
  (with-slots (program) shader-program
    (gl:use-program program)))
