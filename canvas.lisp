;; canvas.lisp
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass canvas (line-segments)
  ()
  (:documentation "Canvas."))

(defun draw-line (canvas
                  pt-1
                 pt-2
                  color-1
                  &optional color-2)
  (add-line-2 canvas :p1 pt-1 :c1 color-1 :p2 pt-2 :c2 (if color-2 color-2 color-1)))

(defun draw-triangle (canvas pt-1 pt-2 pt-3 color)
  (add-line-2 canvas :p1 pt-1 :c1 color :p2 pt-2 :c2 color)
  (add-line-2 canvas :p1 pt-2 :c1 color :p2 pt-3 :c2 color)
  (add-line-2 canvas :p1 pt-3 :c1 color :p2 pt-1 :c2 color))

