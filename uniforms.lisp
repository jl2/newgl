;;;; uniforms.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass uniform ()
  ((name :initarg :name :type string)
   (type :initarg :type)
   (value :initarg :value :initform nil :type t))
  (:documentation "A uniform variable parameter to a shader."))

(defgeneric use-uniform (uniform program)
  (:documentation "Pass the uniform's value into the OpenGL program."))


(defmethod use-uniform ((uniform uniform) program)
  (with-slots (name type value) uniform
    (let ((location (gl:get-uniform-location program name)))
      (when (>= location 0)
        (when (null value)
          (format t "value of ~a is nil, using default.~%" name))
        (cond ((eq :mat4 type)
               (gl:uniform-matrix location
                                  4
                                  (vector (marr4 (if value
                                                     value
                                                     (meye 4))))
                                  t))
              ((eq :mat3 type)
               (gl:uniform-matrix location
                                  3
                                  (vector (marr3 (if value
                                                     value
                                                     (meye 3))))
                                  t))
              ((eq :int type)
               (gl:uniformi location (if value value 0)))
              ((eq :float type)
               (gl:uniformf location (if value value 0.0)))
              ((eq :vec2 type)
               (gl:uniformfv location (if value value (vec2 0.0 0.0))))
              ((eq :vec3 type)
               (gl:uniformfv location (if value value (vec3 0.0 0.0 0.0))))
              ((eq :vec4 type)
               (gl:uniformfv location (if value value (vec4 0.0 0.0 0.0 0.0)))))))))

(defun set-value (uniform new-value)
  (with-slots (value) uniform
    (setf value new-value)))
