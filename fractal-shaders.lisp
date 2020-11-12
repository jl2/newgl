;; fractal-shaders.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defparameter *fractal-shader-dir* (asdf:system-relative-pathname :newgl "shaders/fractals/")
  "Directory containing newgl shaders.")

(defun set-iterations (fractal iterations)
  (newgl:set-uniform fractal "maxIterations" iterations))

(defun mandelbrot-set-shaders (max-iterations)
  (let ((shaders (list (shader-from-file (merge-pathnames *fractal-shader-dir* "mandel-fragment.glsl"))
                       (shader-from-file (merge-pathnames *fractal-shader-dir* "complex-vertex.glsl")))))
    (set-uniform (car shaders) "maxIterations" max-iterations)
    (set-uniform (cadr shaders) "maxIterations" max-iterations)
    shaders))
