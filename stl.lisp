;; stl.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass stl (opengl-object)
  ((file-name :type string :initarg :file-name)
   (tri-count :type fixnum :initform 0)
   (shaders :initform (newgl:plastic) :initarg :shaders)
   (primitive-type :initform :triangles)
   (colors :initform (list (vec4 0 1 0 1)) :initarg :colors)
   (matrices :initform (list (meye 4)) :initarg :matrices)
   ))

(defmethod show-info ((object stl) &key (indent 0))
  (call-next-method)
  (let ((this-ws (indent-whitespace (+ 1 indent))))
    (show-slots this-ws object '(file-name))))

(defun get-u2 (arr idx)
  "Interpret two bytes in arr as an '(unsigned-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (unsigned-byte 32) (+ (* (aref arr (+ 1 idx)) 256) (aref arr idx))))

(defun get-u4 (arr idx)
  "Interpret the four bytes in arr as an '(unsigned-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (unsigned-byte 32) (+ (* (+ (* (+ (* (aref arr (+ 3 idx)) 256)
                                         (aref arr (+ 2 idx))) 256)
                                   (aref arr (+ 1 idx))) 256)
                             (aref arr idx))))

(defun get-s4 (arr idx)
  "Interpret four bytes in arr as an '(signed-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (signed-byte 32) (+ (* (+ (* (+ (* (aref arr (+ 3 idx)) 256)
                                       (aref arr (+ 2 idx))) 256)
                                 (aref arr (+ 1 idx))) 256)
                           (aref arr idx))))

(defun get-float (arr idx)
  "Interpret four bytes in arr as a single-float."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (simple-array (unsigned-byte 8)) arr))
  (let ((x (get-u4 arr idx)))
    #+(and :little-endian :ieee-floating-point :sbcl)
    (if (>= x #x80000000)
        (sb-kernel:make-single-float (- x #x100000000))
        (sb-kernel:make-single-float x))
    #-(and :little-endian :ieee-floating-point :sbcl)
    (ieee-floats:decode-float32 x)))

(defun get-point (arr idx)
  "Create a point using x, y, and z values read from arr."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (simple-array (unsigned-byte 8) *) arr))
  (vec3 (get-float arr idx)
        (get-float arr (+ idx *float-byte-size*))
        (get-float arr (+ idx (* 2 *float-byte-size*)))))

(defparameter *float-byte-size* 4                                 "Size of an STL float in bytes.")
(defparameter *point-byte-size* (* 3 *float-byte-size*)           "Size of an STL point in bytes.")
(defparameter *triangle-byte-size* (+ 2 (* 4 *point-byte-size*))  "Size of an STL triangle in bytes.")

(defun read-triangle (arr idx)
  "Read a triangle from arr."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (simple-array (unsigned-byte 8)) arr))
  (values
   (get-point arr idx)
   (get-point arr (+ (* 1 *point-byte-size*) idx))
   (get-point arr (+ (* 2 *point-byte-size*) idx))
   (get-point arr (+ (* 3 *point-byte-size*) idx))
   (coerce (get-u2 arr (+ (* 4 *point-byte-size*) idx)) 'single-float)))

(defun get-stl-info (stl-obj)
  (with-slots (file-name tri-count) stl-obj
    (with-open-file (inf file-name :element-type '(unsigned-byte 8))
      (let ((header (make-array 80 :element-type '(unsigned-byte 8)))
            (triangle-count-buffer (make-array 4 :element-type '(unsigned-byte 8))))
        (read-sequence header inf)
        (read-sequence triangle-count-buffer inf)
        (setf tri-count (get-u4 triangle-count-buffer 0))
        (format t "STL File has ~a triangles!~%" tri-count)
        (values header tri-count)))))


(defmethod initialize-buffers ((obj stl) &key)
  (with-slots (file-name tri-count) obj
    (let ((vertices )
          (indices )
          (buffer (make-array *triangle-byte-size* :element-type '(unsigned-byte 8)))
          (cur-offset 0))
      (with-open-file (inf file-name :element-type '(unsigned-byte 8))
        (let ((header (make-array 80 :element-type '(unsigned-byte 8)))
              (triangle-count-buffer (make-array 4 :element-type '(unsigned-byte 8))))
          (read-sequence header inf)
          (read-sequence triangle-count-buffer inf)
          (setf tri-count (get-u4 triangle-count-buffer 0))
          (format t "STL File has ~a triangles!~%" tri-count)
          (setf vertices (allocate-gl-array :float (* tri-count (* 3 6))))
          (setf indices (allocate-gl-array :unsigned-int (* tri-count 2 3)))
          (loop for idx below tri-count
                for offset = 0 then (* idx *triangle-byte-size*)
                for last-idx = (read-sequence buffer inf)
                while (= last-idx  *triangle-byte-size*)
                do
                   (multiple-value-bind
                         (norm p1 p2 p3 attrib) (read-triangle buffer 0)

                     (when (v- (vec3 0 0 0) norm)
                       (setf norm (vunit (vc (v- p1 p2) (v- p1 p3) ))))


                     (setf cur-offset (fill-buffer p1 vertices cur-offset))

                     (setf cur-offset (fill-buffer norm vertices cur-offset))

                     (setf cur-offset (fill-buffer p2 vertices cur-offset))

                     (setf cur-offset (fill-buffer norm vertices cur-offset))

                     (setf cur-offset (fill-buffer p3 vertices cur-offset))

                     (setf cur-offset (fill-buffer norm vertices cur-offset))))))
      (fill-buffer (loop for i below (* 2 3 tri-count) collecting i) indices 0)
      (use-buffer obj :vertices (make-instance 'attribute-buffer
                                               :pointer vertices
                                               :attributes '(("in_position" . :vec3)
                                                             ("in_normal" . :vec3))
                                               :free t))
      (use-buffer obj :indices (make-instance 'index-buffer
                                              :idx-count (* 3 tri-count)
                                              :pointer indices
                                              :free t))
      (with-slots (matrices colors instance-count) obj
        (setf instance-count (length matrices))
        (use-buffer obj :colors (make-instance 'instance-buffer
                                               :pointer (to-gl-array :float (* 4 instance-count) colors)
                                               :attributes '(("in_color" . :vec4))
                                               :free t))
        (use-buffer obj :transforms (make-instance 'instance-buffer
                                                   :pointer (to-gl-array :float (* 16 instance-count) matrices)
                                                   :free t))))))
(defun rotating-stl-viewer (&key (instance-count 100)
                              (file-name "/home/jeremiah/data/3d-models/cube.stl"))
  (newgl:display
   (make-instance
    'newgl:stl
    :colors (loop for i below instance-count collecting (v+ (vec4 0 0 0 1) (vxyz_ (vec3-random 0.5 1.0))))
    :matrices (loop for i below instance-count collecting (m* (mtranslation (vec3-random -12.0 12.0))
                                                   (mscaling (vec3 0.125 0.125 0.125))
                                                   (mrotation +vx+ (random (* 2 pi)))
                                                   (mrotation +vy+ (random (* 2 pi)))
                                                   (mrotation +vz+ (random (* 2 pi)))))
    :file-name file-name)
   ;;   (make-instance '3d-mouse-nav-viewer)))
   (newgl:create-rotating-viewer :radius 32.0 :dt (/ 1.0 2))))
