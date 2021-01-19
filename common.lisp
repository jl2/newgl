;; common.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(deftype point ()
  'vec3)

(deftype normal ()
  'vec3)

(deftype color ()
  'vec4)


(defgeneric initialize (object &key)
  (:documentation "Initialize an OpenGL object."))

(defgeneric render (object)
  (:documentation "Render an OpenGL object."))

(defgeneric update (object elapsed-seconds)
  (:documentation "Called on an object *before* rendering to update for the next animation frame."))

(defgeneric cleanup (object)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

;; Input handlers
(defgeneric handle-key (object window key scancode action mod-keys)
  (:documentation "Handle a GLFW key press.  Return non-nil if handled."))

(defgeneric handle-click (object window click-info)
  (:documentation "Handle mouse move."))

(defgeneric handle-scroll (object window cpos x-scroll y-scroll)
  (:documentation "Handle scrolling."))

(defgeneric handle-resize (object window width height)
  (:documentation "Handle window resize."))
