;; common.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defgeneric render (object view-xform)
  (:documentation "Make OpenGL API calls to render the object.  Binding correct VAO is handled by before and after methods."))

(defgeneric update (object)
  (:documentation "Called on an object *before* rendering to update for the next animation frame."))

(defgeneric handle-key (object window key scancode action mod-keys)
  (:documentation "Handle a GLFW key press.  Return non-nil if handled."))

(defgeneric handle-click (object window click-info)
  (:documentation "Handle mouse move."))

(defgeneric handle-scroll (object window cpos x-scroll y-scroll)
  (:documentation "Handle scrolling."))

(defgeneric handle-drag (object window first-click-info current-pos)
  (:documentation "Handle mouse drag."))

(defgeneric handle-resize (object window width height)
  (:documentation "Handle window resize."))
