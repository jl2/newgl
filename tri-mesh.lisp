;;;; primitives.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass tri-mesh (opengl-object)
  (
   (filled-vertex-data :initform (make-array 0
                                             :element-type 'single-float
                                             :initial-contents '()
                                             :adjustable t
                                             :fill-pointer 0))
   (filled-triangles   :initform (make-array 0 :element-type 'fixnum
                                             :initial-contents '()
                                             :adjustable t
                                             :fill-pointer 0))
   (fill-program       :initarg :fill-program
                       :initform
                       (make-instance
                        'shader-program
                        :inputs '(("position" . 3) ("normal" . 3) ("color" . 4))
                        :vertex (merge-pathnames *shader-dir* "default-filled-vertex.glsl")
                        :fragment (merge-pathnames *shader-dir* "default-fragment.glsl")))
   (changed :initform t))
  (:documentation "A triangle mesh."))

(defmethod rebuild-shaders ((object tri-mesh))
  (with-slots (vao fill-program) object
    (when vao
      (gl:bind-vertex-array vao)
      (build-program fill-program))))

;; (defun tol-equal (a b &optional (tolerance 0.001))
;;   (< (abs (- a b )) tolerance))

(defun insert-pnc-in-buffer (buffer pt normal color)
  (let ((olen (length buffer)))
    (insert-vect buffer pt)
    (insert-vect buffer normal)
    (insert-vect buffer color)
    (floor (/ olen 10))))

(defun triangle-normal (pt1 pt2 pt3)
  "Compute the normal of a triangle."
  (declare (type point pt1 pt2 pt3))
  (vc (v- pt2 pt1) (v- pt1 pt3)))


(defun add-triangle (object pt1 pt2 pt3 color &optional norm)

  (declare (type tri-mesh object)
           (type point pt1 pt2)
           (type color color))

  (let ((normal (if norm norm (triangle-normal pt1 pt2 pt3))))
    (with-slots (filled-vertex-data filled-triangles changed) object
      (vector-push-extend (insert-pnc-in-buffer filled-vertex-data
                                                pt1
                                                normal
                                                color)
                          filled-triangles)
      (vector-push-extend (insert-pnc-in-buffer filled-vertex-data
                                                pt2
                                                normal
                                                color)
                          filled-triangles)
      (vector-push-extend (insert-pnc-in-buffer filled-vertex-data
                                                pt3
                                                normal
                                                color)
                          filled-triangles)
      (setf changed t))))


(defmethod fill-buffers ((object tri-mesh))
  (call-next-method)
  (with-slots (vao vbos ebos filled-triangles filled-vertex-data changed) object
    (when (null vbos)
      (setf changed t)
      (setf vbos (gl:gen-buffers 1))
      (setf ebos (gl:gen-buffers 1)))

    (cond
      ((not changed)
       (gl:bind-buffer :array-buffer (car vbos))
       (gl:bind-buffer :element-array-buffer (car ebos)))

      (t 
       (let ((gl-vertices (to-gl-float-array filled-vertex-data))
             (gl-indices (to-gl-array filled-triangles :unsigned-int)))

         (gl:bind-buffer :array-buffer (car vbos))
         (gl:buffer-data :array-buffer :static-draw gl-vertices)
         (gl:free-gl-array gl-vertices)

         (gl:bind-buffer :element-array-buffer (car ebos))
         (gl:buffer-data :element-array-buffer :static-draw gl-indices)
         (gl:free-gl-array gl-indices))))
    (setf changed nil)))

(defmethod render ((object tri-mesh))
  (call-next-method)
  (with-slots (vbos ebos transformation filled-triangles fill-program) object
    (when (and vbos ebos)
      (when (> (length filled-triangles) 0)
        (gl:bind-buffer :array-buffer (car vbos))
        (use-program fill-program :transformation transformation)
        (gl:polygon-mode :front-and-back :fill)
        (gl:bind-buffer :element-array-buffer (car ebos))
        (gl:draw-elements :triangles
                          (gl:make-null-gl-array :unsigned-int)
                          :count (length filled-triangles))))))
