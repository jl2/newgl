;; test-viewer.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass test-viewer (viewer)
  ((eye-pos :initform
            (newgl:create-keyframe-sequence (list
                                             (newgl:create-keyframe (vec3  1.0  1.0 1.0) 0.0)
                                             (newgl:create-keyframe (vec3  1.0  1.0 -10.0) 1.0)
                                             (newgl:create-keyframe (vec3 -10.0  1.0 -10.0) 2.0)
                                             (newgl:create-keyframe (vec3 -10.0  1.0 1.0) 4.0)
                                             (newgl:create-keyframe (vec3  1.0  1.0 1.0) 8.0))))
   ))

(defmethod update :before ((viewer test-viewer) elapsed-seconds)
  (with-slots (view-xform eye-pos) viewer
    (setf view-xform
          (m*
           (mperspective 30.0 1.0 1.0 10000.0)
           (mlookat (value-at eye-pos elapsed-seconds) (vec3 0 0 0) +vy+)
           ))))

(defun show-test-viewer ()
  (let ((tm (make-instance 'newgl:line-segments)))
    (add-line tm
                       (vec3 0 0 0)  (vec4 0 1 0 1)
                       (vec3 0 2 0)  (vec4 0 1 0 1))
    (add-line tm
                       (vec3 0 0 0)  (vec4 1 0 0 1)
                       (vec3 2 0 0)  (vec4 1 0 0 1))
    (add-line tm
                       (vec3 0 0 0)  (vec4 0 0 1 1)
                       (vec3 0 0 2)  (vec4 0 0 1 1))
    (display (make-instance 'test-viewer
                            :objects (list tm)
                            :xform (meye 4)) :debug nil)))
