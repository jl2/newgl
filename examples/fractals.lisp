;; fractals.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :newgl.examples)

(defun show-fractals ()
  (newgl:display (newgl:make-st-quad
                  :s-min -2.0f0 :t-min -1.8f0
                  :s-max 2.0f0 :t-max 1.8f0
                  :shaders (newgl:julia-set-shaders :max-iterations 1000 :real 0.35453458392313 :imag 0.393437674)))
  (newgl:display (newgl:make-st-quad
                  :s-min -2.0f0 :t-min -1.8f0
                  :s-max 2.0f0 :t-max 1.8f0
                  :shaders (newgl:mandelbrot-set-shaders :max-iterations 1000)))
  (newgl:display (newgl:make-st-quad
                  :s-min -2.0f0 :t-min -1.8f0
                  :s-max 2.0f0 :t-max 1.8f0
                  :shaders (newgl:burning-ship-shaders :max-iterations 1000)))
  (newgl:display (newgl:make-st-quad
                  :s-min -2.0f0 :t-min -1.8f0
                  :s-max 2.0f0 :t-max 1.8f0
                  :shaders (newgl:bs-js-shaders :max-iterations 1000 :real 0.35453458392313 :imag 0.393437674))))

