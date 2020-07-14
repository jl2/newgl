;; uniforms.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

;; TODO: Consider creating float-uniform, mat4-uniform, etc. subclasses that call the
;; correct gl:uniform* functions, and remove the big cond in use-uniform.
(defclass uniform ()
  ((name :initarg :name :type string)
   (type :initarg :type)
   (value :initarg :value :initform nil :type t))
  (:documentation "A uniform variable parameter to a shader."))

(defgeneric use-uniform (uniform program)
  (:documentation "Pass the uniform's value into the OpenGL program."))

(defgeneric set-value (uniform new-value)
  (:documentation "Assign a new value to a uniform."))

(defmethod use-uniform ((uniform uniform) program)
  "Bind the uniform's value in the program."

  (with-slots (name type value) uniform
    (let ((location (gl:get-uniform-location program name)))

      ;; Only assign values to uniforms that are used by the program
      (when (>= location 0)
        (when (and (null value) *debug-stream*)
          (format *debug-stream* "value of ~a is nil, using default.~%" name))
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
               (gl:uniformf location (if value value 0.0f0)))
              ((eq :vec2 type)
               (let ((val (if value value (vec3 0.0f0 0.0f0 0.0f0))))
                 (gl:uniformfv location (make-array 2 :element-type 'single-float
                                                    :initial-contents (list (vx val) (vy val))))))
              ((eq :vec3 type)
               (let ((val (if value value (vec3 0.0f0 0.0f0 0.0f0))))
                 (gl:uniformfv location (make-array 3 :element-type 'single-float
                                                    :initial-contents (list (vx val) (vy val) (vz val))))))
              ((eq :vec4 type)
               (let ((val (if value value (vec3 0.0f0 0.0f0 0.0f0))))
                 (gl:uniformfv location (make-array 3 :element-type 'single-float
                                                    :initial-contents
                                                    (list (vx val) (vy val) (vz val) (vw val)))))))))))


(defmethod set-value ((uniform uniform) new-value)
  "Set a uniform's value."
  (with-slots (value) uniform
    (setf value new-value)))
