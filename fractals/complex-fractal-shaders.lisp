;; complex-fractal-shaders.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl.fractals)

(defparameter *fractal-shader-dir* (asdf:system-relative-pathname :newgl.fractals "fractals/shaders/")
  "Directory containing newgl shaders.")

(defun cfractal (name)
  (list (shader-from-file (merge-pathnames *fractal-shader-dir* (format nil "~a-fragment.glsl" name)))
        (shader-from-file (merge-pathnames *fractal-shader-dir* "complex-vertex.glsl"))))

(defclass mandelbrot (complex-window)
  ((newgl:shaders :initform (cfractal "mandel"))))

(defclass mandelbrot-boundary (complex-window)
  ((newgl:shaders :initform (cfractal "mandel-boundary"))))


(defun mandelbrot-viewer (&key (max-iterations 100))
  (make-instance 'fractal-viewer
                 :objects (list (make-instance 'mandelbrot))))

;; (defun mandelbrot-viewer (&key (max-iterations 100))
;;   (make-instance 'fractal-viewer
;;                  :objects (list (make-instance 'complex-window
;;                                                :shaders ))))


;; (defun burning-ship-shaders (&key (max-iterations 100))
;;   (cfractal "burning-ship"))

;; (defun burning-ship-viewer (&key (max-iterations 100))
;;   (make-instance 'fractal-viewer
;;                  :objects (list (make-instance 'complex-window
;;                                          :shaders (burning-ship-shaders)))))

;; (defun julia-set-shaders (&key (max-iterations 100) (real 0.0f0) (imag 0.0f0))
;;   (let ((shaders (cfractal "julia-set")))
;;     (dolist (shader shaders)
;;       (set-uniform shader "maxIterations" max-iterations :int)
;;       (set-uniform shader "cReal" real :float)
;;       (set-uniform shader "cImag" imag :float))
;;     shaders))


;; (defun bs-js-shaders (&key (max-iterations 100) (real 0.0f0) (imag 0.0f0))
;;   (let ((shaders (cfractal "bs-js")))
;;     (dolist (shader shaders)
;;       (set-uniform shader "maxIterations" max-iterations :int)
;;       (set-uniform shader "cReal" real :float)
;;       (set-uniform shader "cImag" imag :float))
;;     shaders))
