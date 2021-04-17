;; tess-test.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass tess-test (opengl-object)
  ((primitive-type :initform :points)))

(defmethod show-info ((object psurf) &key (indent 0))
  (call-next-method)
  (let ((this-ws (indent-whitespace (+ 1 indent))))
    (show-slots this-ws object '(primitive-type))))
