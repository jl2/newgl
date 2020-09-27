;; viewer.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass viewer ()
  ((objects :initform nil :initarg :objects :type (or null cons) :accessor objects)
   (view-xform :initform (meye 4) :initarg :xform :type mat4 :accessor viewport))
  (:documentation "A collection of objects and a viewport."))

(defmethod render ((viewer viewer) xform)
  (with-slots (objects view-xform) viewer
    (loop for object in objects
          do
          (render object (m* xform view-xform)))))

(defmethod cleanup ((viewer viewer))
  (loop for object in (objects viewer)
        do
        (cleanup object)))

(defmethod update ((viewer viewer) elapsed-seconds)
  (loop for object in (objects viewer)
        do
        (update object elapsed-seconds)))

(defmethod reload-object ((viewer viewer))
  (dolist (object (objects viewer))
    (reload-object object)))

(defmethod handle-key ((viewer viewer) window key scancode action mod-keys)
  (cond
    ;; ESC to exit
    ((and (eq key :escape) (eq action :press))
     (set-window-should-close)
     t)

    ;; r to rebuild shaders
    ((and (eq key :r) (eq action :press))
     (format t "Rebuilding shaders...~%")
     (with-slots (objects) viewer
       (dolist (object objects)
         (build-shader-program object)))
     (format t " Done.~%")
     t)

    ;; f to refill buffers
    ((and (eq key :b) (eq action :press))
     (reload-object viewer)
     t)

    ;; i to show gl info
    ((and (eq key :i) (eq action :press))
     (show-open-gl-info)
     t)

    ;; s to show gl state
    ((and (eq key :s) (eq action :press))
     (show-gl-state)
     t)

    ;; c to toggle printing fps
    ((and (eq key :f) (eq action :press))
     (setf *show-fps* (if *show-fps* nil t))
     t)

    ;; w to toggle wireframe
    ((and (eq key :w) (eq action :press))
     (setf *wire-frame* (if *wire-frame* nil t))
     t)

    ;; f1
    ((and (eq key :f1) (eq action :press))
     (setf *cull-face* (if (eq *cull-face* :cull-face)
                           nil
                           :cull-face))
     t)

    ((and (eq key :f2) (eq action :press))
     (setf *front-face* (if (eq *front-face* :cw)
                            :ccw
                            :cw))
     t)
    (t
     (funcall #'some #'identity
              (loop for object in (objects viewer)
                    collect (handle-key object window key scancode action mod-keys))))))


(defmethod handle-resize ((viewer viewer) window width height)
  (loop
        for object in (objects viewer)
        do
        (handle-resize object window width height)))

(defmethod handle-click ((viewer viewer) window click-info)
  (loop
        for object in (objects viewer)
        do
        (handle-click object window click-info)))

(defmethod handle-scroll ((viewer viewer) window cpos x-scroll y-scroll)
  (loop
        for object in (objects viewer)
        do
        (handle-scroll object window cpos x-scroll y-scroll)))

(defmethod handle-drag ((viewer viewer) window first-click-info current-pos)
  (loop
        for object in (objects viewer)
        do
        (handle-drag object window first-click-info current-pos))
  t)