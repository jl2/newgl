;;;; shader-program.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass shader-program ()
  ((shaders :initarg :shaders :initform nil :type (or null list))
   (program :initform 0))
  (:documentation "An opengl shader program."))

(defgeneric use-shader-program (program)
  (:documentation "Set uniform and layout variables for all shaders in the program."))

(defun make-shader-program (&rest shaders)
  (make-instance 'shader-program :shaders shaders))


(defmethod set-uniform ((obj shader-program) name value)
  (with-slots (shaders) obj
    (dolist (shader shaders)
      (set-uniform shader name value))))

(defmethod cleanup ((obj shader-program))
  "Delete a shader on the GPU."
  (with-slots (shaders program) obj
    (dolist (shader shaders)
      (cleanup shader))
    (when (> 0 program)
      (gl:delete-program program))
    (setf program 0)))

(defmethod build-shader-program ((program shader-program))
  "Compile and link shader program, including validation."

  (with-slots (shaders program) program
    ;; Compile all shaders
    (dolist (gl-shader shaders)
      (with-slots (shader) gl-shader
        (when (and (not (zerop shader)) (not (zerop program)))
          (gl:detach-shader program shader))
        (compile-shader gl-shader)))

    ;; Create program and attach shaders
    (when (zerop program)
      (setf program (gl:create-program)))
    (dolist (shader shaders)
      (with-slots (shader) shader
        (gl:attach-shader program shader)))

    ;; Link
    (gl:link-program program)

    ;; Check for errors and validate program
    (let ((status (gl:get-program program :link-status)))
      (when (not (eq t status))
        (format t "program ~a link-program: ~a~%Info log ~s~%"
                program
                status
                (gl:get-program-info-log program))))

    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq t status))
        (format t "program ~a validate-program: ~a~%~a~%"
                program
                status
                (gl:get-program-info-log program))))))


(defmethod use-shader-program :before ((shader-program shader-program))
  ;; Set variables here, if necessary...
  (with-slots (program shaders) shader-program
    (dolist (shader shaders)
      (use-shader-layout shader program))))

(defmethod use-shader-program ((shader-program shader-program))
  (with-slots (program) shader-program
    (gl:use-program program)))

(defmethod use-shader-program :after ((shader-program shader-program))
  (with-slots (program shaders) shader-program
    (dolist (shader shaders)
      (use-shader-uniforms shader program))))

