;;;; primitives.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)


(defclass plastic-vertex-shader (gl-shader)
  ((layout :initform
           '(((:name . "position")
              (:count . 3)
              (:type . :float))

             ((:name . "normal")
              (:count . 3)
              (:type . :float))

             ((:name . "color")
              (:count . 4)
              (:type . :float)))
           :type (or null list))

   (shader :initform 0 :type fixnum)
   (source-file :initform (merge-pathnames *shader-dir* "plastic-vertex.glsl") :type string)
   (shader-type :initform :vertex-shader)))

(defclass plastic-fragment-shader (gl-shader)
  ((shader-type :initform :fragment-shader)
   (source-file :initform (merge-pathnames *shader-dir* "plastic-fragment.glsl"))))

(defclass plastic-program (shader-program)
  ((shaders :initform (list
                       (make-instance 'plastic-vertex-shader)
                       (make-instance 'plastic-fragment-shader)))))


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
   (shader-program :initform (make-instance 'mandel-program))
   (changed :initform t))
  (:documentation "A triangle mesh."))

(defmethod rebuild-shaders ((object tri-mesh))
  (call-next-method)
  (with-slots (shader-program) object
    (build-shader-program shader-program)))

;; (defun tol-equal (a b &optional (tolerance 0.001))
;;   (< (abs (- a b )) tolerance))

(defun insert-vect (buffer pt)
  (vector-push-extend (coerce (vx pt) 'single-float) buffer)
  (vector-push-extend (coerce (vy pt) 'single-float) buffer)
  (vector-push-extend (coerce (vz pt) 'single-float) buffer)
  (typecase pt
    (vec4
     (vector-push-extend (coerce (vw pt) 'single-float) buffer))))

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

(defmethod render ((object opengl-object))
  (with-slots (filled-triangles) object
    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements :triangles
                      (gl:make-null-gl-array :unsigned-int)
                      :count (length filled-triangles))))
