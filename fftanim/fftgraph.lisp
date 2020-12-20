;; fftgraph.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl.fftanim)

;; (declaim (optimize (speed 3) (safety 1) (size 1) (debug 1)))

(defparameter *fft-window-size* 256)

(defclass mp3-fft-viz (mp3-fft-animation)
  ((newgl:primitive-type :initform :lines)
   (newgl:usage :initform :dynamic-draw)
   (newgl:shaders :initform (list
                             (newgl:shader-from-file (newgl:newgl-shader "color-position-vertex.glsl"))
                             (newgl:shader-from-file (newgl:newgl-shader "point-fragment.glsl"))))
   (vertices :initform nil)
   (indices :initform nil)
   (color :initform (vec4 0.0 1.0 0.0 1.0) :initarg :color)
   (steps :initform 240 :initarg :steps))
  (:documentation "An animation that uses FFT data computed from an MP3 file."))


(defmethod newgl:initialize ((object mp3-fft-viz))
  (call-next-method)
  (with-slots (vertices indices steps) object
    (setf vertices (allocate-gl-array :float (* 2 7 steps)))
    (setf indices (allocate-gl-array :unsigned-int (*  2 steps)))))

(defmethod newgl:update ((object mp3-fft-viz) current-time)
  (call-next-method)
  (newgl:update-buffers object))


(defmethod newgl:cleanup ((object mp3-fft-viz))
  (with-slots (vertices indices) object
    (when vertices
      (gl:free-gl-array vertices))
    (when indices
      (gl:free-gl-array indices))
    (setf vertices nil)
    (setf indices nil))
  (call-next-method))

(defmethod newgl:allocate-and-fill-buffers ((obj mp3-fft-viz))
  (with-slots (color steps left-fft-data right-fft-data window-size vertices indices) obj
    (let* (
           (cur-vert-idx 0)
           (cur-idx-idx 0)
           (win-size (glfw:get-window-size))
           (width (car win-size))
           ;; (height (cadr win-size))
           (dw (/ 7 width))
           ;; (x-aspect-ratio (if (< height width)
           ;;                     (/ height width 1.0d0)
           ;;                     1.0d0))
           ;; (y-aspect-ratio (if (< height width)
           ;;                     1.0d0
           ;;                     (/ width height 1.0d0)))
           )

      (loop
        for i below steps
        for left across left-fft-data
        for right across right-fft-data
        for cur-t = 0.0d0 then (* i dw)
        do
           (let* (

                  (x1 (- cur-t 1))
                  (y1 (- (/ (abs left) (/ window-size 8)) 1))
                  (z1 0.0d0)
                  (x2 (- (+ dw cur-t) 1))
                  (y2 (- (/ (abs left) (/ window-size 8)) 1))
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
             (incf cur-idx-idx))))
      (values vertices indices)))
    
  
