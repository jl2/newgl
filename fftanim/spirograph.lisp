;; spirograph.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl.fftanim)

(defclass spirograph (mp3-fft-animation)
  ((newgl:primitive-type :initform :lines)
   (newgl:usage :initform :dynamic-draw)
   (newgl:shaders :initform (list
                       (newgl:shader-from-file (newgl:newgl-shader "point-vertex.glsl"))
                       (newgl:shader-from-file (newgl:newgl-shader "point-fragment.glsl"))))
   (steps :initform 240 :initarg :steps)
   (color :initform (vec4 0.0 1.0 0.0 1.0) :initarg :color)
   (a-var :initarg :a-var :initform (make-animated-var :val 16.0
                                       :buckets '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))
  (b-var :initarg :b-var :initform (make-animated-var :val 7.0 :buckets '(6 7 8 9 10)))
  (h-var :initarg :h-var :initform (make-animated-var :val 9.0 :buckets '(16 16 17 17 18 18 19 19)))
  (dt-1-var :initarg :dt-1-var :initform (make-animated-var :val 1.15 :buckets '()))
   (dt-2-var :initarg :dt-2-var :initform (make-animated-var :val 0.25 :buckets '()))
   (vertices :initform nil)
   (indices :initform nil)
  (stype :initform :hypotrochoid :initarg :stype)))

(defun reset-spirograph (spiro)
  (with-slots (a-var b-var h-var dt-1-var dt-2-var) spiro
    (reset-var a-var)
    (reset-var b-var)
    (reset-var h-var)
    (reset-var dt-1-var)
    (reset-var dt-2-var)))


;; map-val is used to map logical coordinates to screen coordinates.
(declaim (ftype (cl:function (double-float double-float double-float double-float double-float) double-float) map-val)
         (ftype (cl:function (double-float double-float double-float double-float) double-float) epitrochoid-x)
         (ftype (cl:function (double-float double-float double-float double-float) double-float) epitrochoid-y)
         (ftype (cl:function (double-float double-float double-float double-float) double-float) hypotrochoid-x)
         (ftype (cl:function (double-float double-float double-float double-float) double-float) hypotrochoid-y)
         (inline map-val epitrochoid-x epitrochoid-y hypotrochoid-x hypotrochoid-y))

(defun map-val (x xmin xmax new-xmin new-xmax)
  "Map a value from the range xmin,xmax to the range new-xmin,new-xmax"
  (declare (type double-float x xmin xmax new-xmin new-xmax))
  (the double-float (+ (* (/ (- x xmin) (- xmax xmin)) (- new-xmax new-xmin)) new-xmin)))

(defun epitrochoid-x (a b h tv)
  "X component of the parametric equation for an epitrochoid curve."
  (declare (type double-float a b h tv))
  (the double-float (- (* (+ a b) (cos tv)) (* h (cos (* tv (/ (+ a b) b)))))))

(defun epitrochoid-y (a b h tv)
  "Y component of the parametric equation for an epitrochoid curve."
  (declare (type double-float a b h tv))
  (the double-float (- (* (+ a b) (sin tv)) (* h (sin (* tv (/ (+ a b) b)))))))

(defun hypotrochoid-x (a b h tv)
  "X component of the parametric equation for an hypotrochoid curve."
  (declare (type double-float a b h tv))
  (the double-float (+ (* (- a b) (cos tv)) (* h (cos (* tv (/ (- a b) b)))))))

(defun hypotrochoid-y (a b h tv)
  "Y component of the parametric equation for an hypotrochoid curve."
  (declare (type double-float a b h tv))
  (the double-float (+ (* (- a b) (sin tv)) (* h (sin (* tv (/ (- a b) b)))))))

(defun max-radius (spiro)
  (with-slots (a-var b-var h-var stype) spiro
    (if (eql stype :epitrochoid)
             (* 1.05d0
                (+ (with-slots (val offset) a-var (+ val offset))
                   (with-slots (val offset) b-var (+ val offset))
                   (with-slots (val offset) h-var (+ val offset))))
             (* 1.05d0
                (+ (- (with-slots (val offset) a-var (+ val offset))
                      (with-slots (val offset) b-var (+ val offset)))
                   (with-slots (val offset) h-var (+ val offset)))))))

(defmethod newgl:initialize ((object spirograph))
  (call-next-method)
  (with-slots (vertices indices steps) object
    (setf vertices (allocate-gl-array :float (* 2 7 steps)))
    (setf indices (allocate-gl-array :unsigned-int (*  2 steps)))))

(defmethod newgl:cleanup ((object spirograph))
  (with-slots (vertices indices) object
    (when vertices
      (gl:free-gl-array vertices))
    (when indices
      (gl:free-gl-array indices))
    (setf vertices nil)
    (setf indices nil))
  (call-next-method))

(defmethod newgl:update ((object spirograph) current-time)
  (call-next-method)
  (with-slots (a-var b-var h-var dt-1-var dt-2-var left-fft-data right-fft-data) object
    (step-var a-var 0.00005d0 left-fft-data right-fft-data)
    (step-var b-var 0.00005d0 left-fft-data right-fft-data)
    (step-var h-var 0.00005d0 left-fft-data right-fft-data)
    (step-var dt-1-var 0.000005d0 left-fft-data right-fft-data)
    (step-var dt-2-var 0.000005d0 left-fft-data right-fft-data))
  (newgl:update-buffers object))

(defmethod newgl:allocate-and-fill-buffers ((obj spirograph))
  (with-slots (color steps a-var b-var h-var offset dt-2-var dt-1-var stype vertices indices) obj
    (let* (

           (cur-vert-idx 0)
           (cur-idx-idx 0)
           (win-size (glfw:get-window-size))
           (width (car win-size))
           (height (cadr win-size))
           (x-aspect-ratio (if (< height width)
                               (/ height width 1.0d0)
                               1.0d0))
           (y-aspect-ratio (if (< height width)
                               1.0d0
                               (/ width height 1.0d0)))
           ;; (x-aspect-ratio 1.0)
           ;; (y-aspect-ratio 1.0)
           (rdt-1 (+ (animated-var-val dt-1-var) (animated-var-offset dt-1-var)))

           (max-radius (max-radius obj))
           (x-fun (if (eql stype  :epitrochoid) #'epitrochoid-x #'hypotrochoid-x))
           (y-fun (if (eql stype  :epitrochoid) #'epitrochoid-y #'hypotrochoid-y)))

      (loop
        for i below steps
        for cur-t = 0.0d0 then (* i rdt-1)
        do
           (let* ((r-a (with-slots (val offset) a-var (+ val offset)))
                  (r-b (with-slots (val offset) b-var (+ val offset)))
                  (r-h (with-slots (val offset) h-var (+ val offset)))
                  (r-dt-2 (with-slots (val offset) dt-2-var (+ val offset)))

                  (x1 (map-val (* x-aspect-ratio (funcall x-fun  r-a r-b r-h cur-t))
                               (- max-radius) max-radius
                               -1.0d0 1.0d0))
                  (y1 (map-val (* y-aspect-ratio (funcall y-fun r-a r-b r-h cur-t))
                               (- max-radius) max-radius
                               -1.0d0 1.0d0))
                  (z1 0.0d0)
                  (x2 (map-val (* x-aspect-ratio (funcall x-fun r-a r-b r-h (+ r-dt-2 cur-t)))
                               (- max-radius) max-radius
                               -1.0d0 1.0d0))
                  (y2 (map-val (* y-aspect-ratio (funcall y-fun r-a r-b r-h (+ r-dt-2 cur-t)))
                               (- max-radius) max-radius
                               -1.0d0 1.0d0))
                  (z2 0.0d0))
             (gl-set vertices cur-vert-idx x1 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx y1 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx z1 'single-float)
             (incf cur-vert-idx)

             (gl-set vertices cur-vert-idx (vx color) 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx (vy color) 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx (vz color) 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx (vw color) 'single-float)
             (incf cur-vert-idx)

             (gl-set indices cur-idx-idx cur-idx-idx 'fixnum)
             (incf cur-idx-idx)

             (gl-set vertices cur-vert-idx x2 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx y2 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx z2 'single-float)
             (incf cur-vert-idx)

             (gl-set vertices cur-vert-idx (vx color) 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx (vy color) 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx (vz color) 'single-float)
             (incf cur-vert-idx)
             (gl-set vertices cur-vert-idx (vw color) 'single-float)
             (incf cur-vert-idx)

             (gl-set indices cur-idx-idx cur-idx-idx 'fixnum)
             (incf cur-idx-idx)))
      (values vertices indices))))
