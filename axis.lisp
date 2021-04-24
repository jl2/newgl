;; axis.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass line-axis (opengl-object)
  ((shaders :initform (point-shader))
   (primitive-type :initform :lines)
   (matrices :initform (list (meye 4)) :initarg :matrices)))

(defmethod show-info ((object line-axis) &key (indent 0))
  (call-next-method)
  (let ((this-ws (indent-whitespace (+ 1 indent))))
    (show-slots this-ws object '(shaders primitive-type matrices))))

(defmethod initialize-buffers ((obj line-axis) &key)
  (let* (
         (vertices (allocate-gl-array :float (* 8 12)))
         (indices (allocate-gl-array :unsigned-int 12 )))
    (fill-buffer (list (vec3 1.0 0.0 0.0) (vec4 1.0 0.0 0.0 1.0)
                       (vec3 0.0 0.0 0.0) (vec4 1.0 0.0 0.0 1.0)

                       (vec3 -1.0 0.0 0.0) (vec4 0.75 0.0 0.0 1.0)
                       (vec3 0.0 0.0 0.0) (vec4 0.75 0.0 0.0 1.0)

                       (vec3 0.0 1.0 0.0) (vec4 0.0 1.0 0.0 1.0)
                       (vec3 0.0 0.0 0.0) (vec4 0.0 1.0 0.0 1.0)

                       (vec3 0.0 -1.0 0.0) (vec4 0.0 0.75 0.0 1.0)
                       (vec3 0.0 0.0 0.0) (vec4 0.0 0.75 0.0 1.0)

                       (vec3 0.0 0.0 1.0) (vec4 0.0 0.0 1.0 1.0)
                       (vec3 0.0 0.0 0.0) (vec4 0.0 0.0 1.0 1.0)

                       (vec3 0.0 0.0 -1.0) (vec4 0.0 0.0 0.75 1.0)
                       (vec3 0.0 0.0 0.0) (vec4 0.0 0.0 0.75 1.0))
                 vertices
                 0)
    (fill-buffer (loop for i below 12 collecting i)
                 indices
                 0)
    (use-buffer obj :vertices (make-instance 'attribute-buffer
                                             :pointer vertices
                                             :attributes '(("in_position" . :vec3)
                                                           ("in_color" . :vec4))
                                             :free t))
    (use-buffer obj :indices (make-instance 'index-buffer
                                            :idx-count 12
                                            :pointer indices
                                            :free t))
      (with-slots (matrices instance-count) obj
        (setf instance-count (length matrices))
        (use-buffer obj :transforms (make-instance 'instance-buffer
                                                   :pointer (to-gl-array :float (* 16 instance-count) matrices)
                                                   :free t)))))
#+spacenav
(defmethod handle-3d-mouse-event ((obj line-axis) (event sn:motion-event))
  (with-slots (matrices instance-count) obj
    (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
      (let* (
             (radial-scale (/  1.0 10000.0)))

             (loop for matrix in matrices do
               (nm* matrix
                    (mrotation (vec3  1 0 0)
                               (* radial-scale sn:rx))
                    (mrotation (vec3 0 0 1)
                               (* radial-scale sn:rz))
                    (mrotation (vec3 0 1 0)
                               (* 1.0 radial-scale sn:ry))))))
    (use-buffer obj :transforms (make-instance 'instance-buffer
                                               :pointer (to-gl-array :float (* 16 instance-count) matrices)
                                               :free t)))

  (call-next-method))
