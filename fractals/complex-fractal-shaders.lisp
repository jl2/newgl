;; complex-fractal-shaders.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl.fractals)

(defparameter *fractal-shader-dir* (asdf:system-relative-pathname :newgl.fractals "fractals/shaders/")
  "Directory containing newgl shaders.")

(defun cfractal (name)
  (list (shader-from-file (merge-pathnames *fractal-shader-dir* (format nil "~a-fragment.glsl" name)))
        (shader-from-file (merge-pathnames *fractal-shader-dir* "complex-vertex.glsl"))))

(defun set-iterations (fractal iterations)
  (newgl:set-uniform fractal "maxIterations" iterations))


(defun mandelbrot-set-shaders (&key (max-iterations 100))
  (let ((shaders (cfractal "mandel")))
    (dolist (shader shaders)
      (set-uniform shader "maxIterations" max-iterations))
    shaders))

(defun mandelbrot-viewer (&key (max-iterations 100))
  (make-instance 'fractal-viewer
                 :objects (list (make-instance 'complex-window
                                         :shaders (mandelbrot-set-shaders
                                                   :max-iterations max-iterations)))))


(defun burning-ship-shaders (&key (max-iterations 100))
  (let ((shaders (cfractal "burning-ship")))
    (dolist (shader shaders)
      (set-uniform shader "maxIterations" max-iterations))
    shaders))

(defun burning-ship-viewer (&key (max-iterations 100))
  (make-instance 'fractal-viewer
                 :objects (list (make-instance 'complex-window
                                         :shaders (burning-ship-shaders
                                                   :max-iterations max-iterations)))))

(defun julia-set-shaders (&key (max-iterations 100) (real 0.0f0) (imag 0.0f0))
  (let ((shaders (cfractal "julia-set")))
    (dolist (shader shaders)
      (set-uniform shader "maxIterations" max-iterations)
      (set-uniform shader "cReal" real)
      (set-uniform shader "cImag" imag))
    shaders))


(defun bs-js-shaders (&key (max-iterations 100) (real 0.0f0) (imag 0.0f0))
  (let ((shaders (cfractal "bs-js")))
    (dolist (shader shaders)
      (set-uniform shader "maxIterations" max-iterations)
      (set-uniform shader "cReal" real)
      (set-uniform shader "cImag" imag))
    shaders))
