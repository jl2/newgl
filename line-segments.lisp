;; point-cloud.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)


(defun set-vert (array idx &key (vert) (color) (uv) (normal))
  (when vert
    (setf (gl:glaref array idx 'x) (vx vert))
    (setf (gl:glaref array idx 'y) (vy vert))
    (setf (gl:glaref array idx 'z) (vz vert)))
  (when vert
    (setf (gl:glaref array idx 'x) (vx vert))
    (setf (gl:glaref array idx 'y) (vy vert))
    (setf (gl:glaref array idx 'z) (vz vert)))
  (when color
    (setf (gl:glaref array idx 'r) (vx color))
    (setf (gl:glaref array idx 'g) (vy color))
    (setf (gl:glaref array idx 'b) (vz color))
    (setf (gl:glaref array idx 'a) (vw color)))
  (when uv
    (setf (gl:glaref array idx 'u) (vx uv))
    (setf (gl:glaref array idx 'v) (vy uv))))

(defclass line-segments (opengl-object)
  ((vertices :initform (make-array 0
                                   :element-type 'vec3
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (colors :initform (make-array 0
                                   :element-type 'vec4
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (indices :initform (make-array 0
                                   :element-type 'fixnum
                                   :initial-contents '()
                                   :adjustable t
                                   :fill-pointer 0))
   (vert-pointer :initform nil)
   (idx-pointer :initform nil)
   (primitive-type :initform :lines))
  (:documentation "A collection of lines segment using the same shader program."))

(defun to-vertex-buffer (verts colors)
  (let ((count (+ (* (length verts) 3) (* (length colors) 4))))
    (make-array count
                :initial-contents
                (loop for v across verts
                      for c across colors
                      collect (vx v)
                      collect (vy v)
                      collect (vz v)
                      collect (vx c)
                      collect (vy c)
                      collect (vz c)
                    collect (vw c)))))


(defmethod initialize-buffers ((obj line-segments) &key)
  (with-slots (needs-update vert-pointer idx-pointer colors vertices indices) object
    (when (and needs-update
               (> (length vertices) 0)
               (> (length indices) 0))

      (when vert-pointer
        (free-gl-array vert-pointer))

      (when idx-pointer
        (free-gl-array idx-pointer))

      (setf vert-pointer (allocate-gl-array  :float (* 7 (length vertices))))
      (setf idx-pointer (allocate-gl-array :unsigned-int (length indices)))

      (loop for vert across vertices
            for color across colors
            for idx from 0
            do
               (set-vert vert-pointer idx vert color)
            )
      (loop for idx across indices
            for i from 0 do
              (setf (gl:glaref idx-pointer i) idx)))
    (values vert-pointer idx-pointer)))

(defmethod cleanup ((lines line-segments))
  (with-slots (needs-update vert-pointer idx-pointer) lines
    (when vert-pointer
      (free-gl-array vert-pointer))
    (when idx-pointer
      (free-gl-array idx-pointer))
    (setf vert-pointer nil)
    (setf idx-pointer nil)
    (setf needs-update t)))

(defun add-line (lines p1 c1 p2 c2)
  (with-slots (vertices colors indices) lines

    (let ((index  (length vertices)))
      (vector-push-extend p1 vertices)
      (vector-push-extend p2 vertices)
      (vector-push-extend c1 colors)
      (vector-push-extend c2 colors)
      (vector-push-extend index indices)
      (vector-push-extend (1+ index) indices)
      (values index (1+ index)))))


(defun add-line-by-index (lines idx0 idx1)
  (with-slots (indices) lines
    (vector-push-extend idx0 indices)
    (vector-push-extend idx1 indices)
    (values idx0 idx1)))


(defun random-line-cloud (&optional (n 100))
  (let ((lines (make-instance 'line-segments)))
    (dotimes (i n)
      (add-line lines
                (vec3-random -0.98 0.98)
                (vec4-random 0.2 0.8)
                (vec3-random -0.98 0.98)
                (vec4-random 0.2 0.8)))
    lines))


(defun make-square (color)
  (let ((ls (make-instance 'line-segments)))
    (add-line ls
              (vec 0.0 0.0 0.0)
              color
              (vec 1.0 0.0 0.0)
              color)
    (add-line ls
              (vec3 1.0 0.0 0.0)
              color
              (vec3 1.0 1.0 0.0)
              color)
    (add-line ls
              (vec3 1.0 1.0 0.0)
              color
              (vec3 0.0 1.0 0.0)
              color)
    (add-line ls
              (vec3 0.0 1.0 0.0)
              color
              (vec3 0.0 0.0 0.0)
              color)
    ls))


(defun create-axis (size &key (color t)
                           (half nil))
  (let* ((axis (make-instance 'line-segments))
         x-color
         y-color
         z-color
         (min-value(if half 0 (- size)))
         (max-value size))
    (cond ((eq color :black)
           (setf x-color (vec4 0 0 0 1)
                 y-color (vec4 0 0 0 1)
                 z-color (vec4 0 0 0 1)))
          ((eq color :white)
           (setf x-color (vec4 1 1 1 1)
                 y-color (vec4 1 1 1 1)
                 z-color (vec4 1 1 1 1)))
          ((eq (type-of color) 'vec4)
           (setf x-color color
                 y-color color
                 z-color color))
          (t
           (setf x-color (vec4 1 0 0 1)
                 y-color (vec4 0 1 0 1)
                 z-color (vec4 0 0 1 1))))
    (add-line axis
                 (vec3 min-value 0 0) x-color
                 (vec3 max-value 0 0) x-color)
    (add-line axis
                 (vec3 0 min-value 0) y-color
                (vec3 0 max-value 0) y-color)
    (add-line axis
                 (vec3 0 0 min-value) z-color
                 (vec3 0 0 max-value) z-color)
    axis))

(defun read-lines-from-text-file (filename)
  (let ((ls (make-instance 'line-segments))
        (pts (with-input-from-file (inf filename)
               (loop for xc = (read inf nil nil)
                     for yc = (read inf nil nil)
                     while (and xc yc)
                     collect (vec3 (coerce xc 'double-float)
                                   0.0
                                   (coerce yc 'double-float)
                                   )))))
    (loop for first in pts
          for second in (cdr pts)

          do
          (add-line ls
                       first
                       (vec4 0 1 0 1)
                       second
                       (vec4 0 1 0 1))

          maximizing (vx first) into max-x
          maximizing (vy first) into max-y
          maximizing (vz first) into max-z

          minimizing (vx first) into min-x
          minimizing (vy first) into min-y
          minimizing (vz first) into min-z

          finally (add-line ls
                               second
                               (vec4 0.0 1.0 0.0 1.0)
                               (car pts)
                               (vec4 0.0 1.0 0.0 1.0))
          finally (return (values ls (vec3 min-x -1.0 min-z) (vec3 max-x 1.0 max-z))))))


(defun draw-triangle (container pt-1 pt-2 pt-3 color)
  (add-line container pt-1 color pt-2 color)
  (add-line container pt-2 color pt-3 color)
  (add-line container pt-3 color pt-1 color))
