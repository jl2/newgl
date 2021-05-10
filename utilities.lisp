;; style.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defun reset-viewer ()
  (defparameter *obj* (make-instance 'newgl:stl :file-name "/home/jeremiah/data/3d-models/torus-thing.stl"))
  (defparameter *viewer* (newgl:create-rotating-viewer :radius 6 :dt 0.125))
  (show-viewer))

(defun show-viewer ()
  (tmt:with-body-in-main-thread() (newgl:display *obj* *viewer*)))
