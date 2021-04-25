;; 3d-mouse-nav-viewer.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defun reset-view (viewer)
  (with-slots (view-changed objects aspect-ratio camera-position camera-forward camera-up camera-right view-xform) viewer
    (setf camera-position (vec3 0.0 0.0 -1.0))
    (setf camera-forward (vec3 0.0 0.0 1.0))
    (setf camera-up (vec3 0.0 1.0 0.0))
    (setf camera-right (vec3 1.0 0.0 0.0))
    (setf view-xform (m* (mperspective 60.0 aspect-ratio 0.1 1000.0)
                         (mlookat camera-position
                                  (vxyz (v+ camera-position camera-forward))
                                  camera-up)))
    (format t "~a~%" view-xform)
    (loop
      for object in objects
      do
         (set-uniform object "view_transform" view-xform :mat4))
    (setf view-changed t)))

(defclass 3d-mouse-nav-viewer (viewer)
  ((camera-position :initform (vec3 0.0 0.0 -1.0))
   (camera-forward :initform (vec3 0.0 0.0 1.0))
   (camera-up :initform (vec3 0.0 1.0 0.0))
   (camera-right :initform (vec3 1.0 0.0 0.0))
   (view-xform :initform (m* (mperspective 60.0 1.0 0.1 1000.0)
                             (mlookat (vec3 0 0 -1)
                                      (vec3 0 0 0)
                                      (vec3 0 1 0)))
               :type mat4))
  (:documentation "A viewer with 3d mouse camera navigation."))

(defmethod handle-key ((viewer 3d-mouse-nav-viewer) window key scancode action mod-keys)
  (cond
    ((and (eq key :f5) (eq action :press))
     (reset-view viewer)
     (with-slots (view-changed) viewer
       (setf view-changed t)
     t))
    (t
     (call-next-method))))
(defun big-enough (val &optional (tol 0.0001))
  (> (abs val) tol))

#+spacenav
(defmethod handle-3d-mouse-event ((viewer 3d-mouse-nav-viewer) (event sn:motion-event))
  (with-slots (aspect-ratio view-xform camera-position camera-up camera-forward camera-right view-changed) viewer
    (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
      ;;(format t "~a ~a ~a    ~a ~a ~a~%" sn:x sn:y sn:z  sn:rx sn:ry sn:rz)
      (setf
       sn:rx 0
       sn:rz 0
       )
      (let* ((linear-scale (/ 1.0 500))
             (radial-scale (/  1.0 (* (min 10.0 (vlength camera-position)) 250.0)))
             (xang (* 1.0 radial-scale sn:rx))
             (yang (* 1.0 radial-scale sn:ry))
             (zang (* 1.0 radial-scale sn:rz))
             (for-mat (m*
                       (if (big-enough yang)
                           (mrotation camera-up yang)
                           (meye 4))
                       (if (big-enough xang)
                           (mrotation camera-right xang)
                           (meye 4))))
             (up-mat (m*
                       (if (big-enough xang)
                           (mrotation camera-right xang)
                           (meye 4))
                       (if (big-enough zang)
                           (mrotation camera-forward zang)
                           (meye 4))))
             (right-mat (m*
                         (if (big-enough yang)
                             (mrotation camera-up yang)
                             (meye 4))
                         (if (big-enough zang)
                             (mrotation camera-forward zang)
                             (meye 4)))))

        (setf camera-forward
              (vxyz (vunit (m*
                            for-mat
                            (vxyz_ camera-forward)))))
        (setf camera-up
              (vxyz (vunit (m*
                            up-mat
                            (vxyz_ camera-up)))))
        (setf camera-right
              (vxyz (vunit (m*
                            right-mat
                            (vxyz_ camera-right)))))

        (setf camera-position
              (v+ camera-position
                  (v* (* linear-scale sn:z) camera-forward)
                  (v* (* 1.0 linear-scale sn:y) camera-up)
                  (v* (* -1.0 linear-scale sn:x) camera-right)))
        ;;(format t "pos ~a~%for ~a~%up ~a~%right ~a~%" camera-position camera-forward camera-up camera-right)
        ;; up/down -> translate along 'up'
        ;; forward/back -> translate along 'direction'
        ;; left/right -> translate orthogonal to camera-forward and +up+
        ;; vcross
        (setf view-changed t)
        (setf view-xform
              (m* (mperspective 20.0 aspect-ratio 0.001 1000.0)
                  (mlookat camera-position
                           (v+ camera-position camera-forward)
                           camera-up)))
        (setf view-changed t)))))
                  ;; (mlookat camera-position
                  ;;          (v+ camera-position camera-forward)
                  ;;          camera-up
                  ;;          )
