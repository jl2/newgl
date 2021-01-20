;; uniforms.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

;; TODO: Consider creating float-uniform, mat4-uniform, etc. subclasses that call the
;; correct gl:uniform* functions, and remove the big cond in use-uniform.
(defclass uniform ()
  ((name :initarg :name :type string)
   (type :initarg :type)
   (value :initarg :value :initform nil :type t)
   (modified :initform t))
  (:documentation "A uniform variable parameter to a shader."))

(defgeneric use-uniform (uniform program)
  (:documentation "Pass the uniform's value into the OpenGL program."))

(defmethod use-uniform ((uniform uniform) program)
  "Bind the uniform's value in the program."

  (with-slots (name type value modified) uniform
    (when modified
      (let ((location (gl:get-uniform-location program name)))

        ;; Only assign values to uniforms that are used by the program
        (when (and modified (>= location 0))
          (cond ((eq :mat4 type)
                 (gl:program-uniform-matrix
                  program location
                  4
                  (vector (marr4 value))
                  t))
                ((eq :mat3 type)
                 (gl:program-uniform-matrix
                  program location
                  3
                  (vector (marr3 value))
                  t))
                ((eq :int type)
                 (gl:program-uniformi program location value))
                ((eq :float type)
                 (gl:program-uniformf program location value))
                ((eq :vec2 type)
                 (gl:program-uniformf program location (vx value) (vy value)))
                ((eq :vec3 type)
                 (gl:program-uniformf program location (vx value) (vy value) (vz value)))
                ((eq :vec4 type)
                 (gl:program-uniformf program location (vx value) (vy value) (vz value) (vw value)))
                (t
                 (error "Don't know how to set type ~a" type)))
          (setf modified nil))))))


(defmethod set-value (uniform new-value)
  "Set a uniform's value."
  (with-slots (value modified) uniform
    (setf modified t)
    (setf value new-value)))
