;; shader-program.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass shader-program ()
  ((shaders :initarg :shaders :initform nil :type (or null list))
   (program :initform 0))
  (:documentation "An opengl shader program."))

(defgeneric use-shader-program (program)
  (:documentation "Set uniform and layout variables for all shaders in the program."))

(defgeneric get-layout-descriptor (shader))

(defmethod get-layout-descriptor ((shader shader-program))
  (with-slots (shaders) shader
    (let ((descriptors (mapcar #'layout (remove-if-not #'identity shaders :key #'layout))))
      (if descriptors
          (get-layout-descriptor (car descriptors))
          nil))))

(defun make-shader-program (shader-type &rest shaders)
  "Create a shader program using the specified shaders."
  (make-instance shader-type :shaders shaders))

(defmethod bind-buffers ((object shader-program)))

(defmethod fill-buffers ((object shader-program)))

(defmethod set-uniform ((obj shader-program) name value)
  "Set a uniform value for the shader program."
  (with-slots (shaders) obj
    (dolist (shader shaders)
      (set-uniform shader name value))))

(defmethod assign-uniforms ((program shader-program) &optional (view-xform (meye 4)))
  (declare (ignorable program view-xform))
  t)

(defmethod cleanup ((obj shader-program))
  "Delete a shader on the GPU."
  (with-slots (shaders program) obj
    (dolist (shader shaders)
      (cleanup shader))
    (when (> 0 program)
      (gl:delete-program program))
    (setf program 0)))

(define-condition shader-link-error (shader-error) ())
(define-condition shader-validate-error (shader-error) ())

(defmethod build-shader-program ((program shader-program))
  "Compile and link a shader program, including validation."

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
        (error 'shader-link-error
               :status status
               :object program
               :info-log (gl:get-program-info-log program))))

    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq t status))
        (restart-case
            (error 'shader-link-error
                   :status status
                   :object program
                   :info-log (gl:get-program-info-log program))
          (ignore-validation-error () t)))))
  program)


(defmethod use-shader-program :before ((shader-program shader-program))
  ":before method that ensures all of the program's layouts are used."
  (with-slots (program shaders) shader-program
    (dolist (shader shaders)
      (use-shader-layout shader program))))

(defmethod use-shader-program ((shader-program shader-program))
  "Use a shader program."
  (with-slots (program) shader-program
    (gl:use-program program)))

(defmethod use-shader-program :after ((shader-program shader-program))
  ":after method that ensures all of the program's uniforms are used."
  (with-slots (program shaders) shader-program
    (dolist (shader shaders)
      (use-shader-uniforms shader program))))


(defun plastic ()
  (make-shader-program 'shader-program
                       (shader-from-file (newgl-shader "plastic-vertex.glsl"))
                       (shader-from-file (newgl-shader "plastic-fragment.glsl"))))
