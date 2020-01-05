;;;; shader-program.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass shader-program ()
  ((shaders :initarg :shaders :initform nil :type (or null list))
   (program :initform 0))
  (:documentation "An opengl shader program."))

(defgeneric build-shader-program (program)
  (:documentation "Build a shader program and all of its corresponding shaders."))

(defgeneric use-shader-program (program)
  (:documentation "Set uniform and layout variables for all shaders in the program."))

(defgeneric set-uniforms (program)
  (:documentation "Assign uniform shader variables associated with this shader program."))


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
      (format t "link-program: ~a~%Info log ~s~%"
              status
              (gl:get-program-info-log program)))

    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (format t "validate-program: ~a~%~a~%"
              status
              (gl:get-program-info-log program)))))

(defmethod use-shader-program :before ((shader-program shader-program))
  (with-slots (program shaders) shader-program
    (dolist (shader shaders)
      (use-layout shader program))))

(defmethod use-shader-program ((shader-program shader-program)))

(defmethod use-shader-program :after ((shader-program shader-program))
  (with-slots (program) shader-program
    (gl:use-program program)))

