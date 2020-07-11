;; scene.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass scene ()
  ((objects :initform nil :initarg :objects :type (or null cons) :accessor objects)
   (view-xform :initform (meye 4) :initarg :xform :type mat4 :accessor viewport))
  (:documentation "A collection of objects and a viewport."))

(defmethod render ((scene scene) xform)
  (with-slots (objects view-xform) scene
    (loop
       for object in objects
       do
         (render object (m* view-xform xform)))))

(defmethod cleanup ((scene scene))
  (loop
     for object in (objects scene)
     do
       (cleanup object)))

(defmethod update ((scene scene))
  (loop
     for object in (objects scene)
     do
       (update object)))

(defmethod handle-key ((scene scene) window key scancode action mod-keys)
  (loop
     for object in (objects scene)
     do
       (handle-key object window key scancode action mod-keys)))


(defmethod handle-resize ((scene scene) window width height)
  (loop
     for object in (objects scene)
     do
       (handle-resize object window width height)))

(defmethod handle-click ((scene scene) window click-info)
  (loop
     for object in (objects scene)
     do
       (handle-click object window click-info)))

(defmethod handle-scroll ((scene scene) window cpos x-scroll y-scroll)
  (loop
     for object in (objects scene)
     do
       (handle-scroll object window cpos x-scroll y-scroll)))

(defmethod handle-drag ((scene scene) window first-click-info current-pos)
  (loop
     for object in (objects scene)
     do
       (handle-drag object window first-click-info current-pos)))
