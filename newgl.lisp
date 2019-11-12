;;;; newgl.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :newgl)

(deftype point ()
  '(or vec3))

(deftype normal ()
  '(or vec3))

(deftype color ()
  '(or vec4))

(defun red (color)
  (vx color))

(defun green (color)
  (vy color))

(defun blue (color)
  (vz color))

(defun alpha (color)
  (vw color))

(defparameter *rebuild-shaders* nil
  "Flag set by keyboard handler to signal rebuilding shaders.")
(defparameter *refill-buffers* nil
  "Flag set by keyboard handler to signal refill buffers.")
(defparameter *show-fps* nil
  "Flag to turn on or off printing FPS.")

(defparameter *wire-frame* t
  "Toggle filled or wireframe drawing.")

(defparameter *cull-face* :cull-face)
(defparameter *front-face* :cw)

(defun show-gl-state ()
  (loop
     for field in '(:active-texture
                    :array-buffer-binding
                    :blend
                    :current-program
                    :line-width
                    :vertex-array-binding
                    :viewport)
     do
       (format t "~a : ~a~%" field (gl:get-integer field))))

;; (defun show-program-state (program)
;;   (loop
;;      for field in '(:link-status
;;                     :attached-shaders)
;;      do
;;        (format t "~a : ~a~%" field (gl:get-program program field))))

(defun show-open-gl-info ()
  (loop
     for field in '(:max-combined-texture-image-units
                    :max-cube-map-texture-size
                    :max-draw-buffers
                    :max-fragment-uniform-components
                    :max-texture-size
                    ;; :max-varying-floats
                    :max-vertex-attribs
                    :max-vertex-texture-image-units
                    :max-vertex-uniform-components
                    :max-viewport-dims
                    :stereo)
     do
       (format t "~a : ~a~%" field (gl:get-integer field))))

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (format t "Keypress: ~a ~a ~a ~a ~a~%" window key scancode action mod-keys)
  (cond

    ;; ESC to exit
    ((and (eq key :escape) (eq action :press))
     (set-window-should-close))

    ;; r to rebuild shaders
    ((and (eq key :r) (eq action :press))
     (setf *rebuild-shaders* t))

    ;; f to refill buffers
    ((and (eq key :f) (eq action :press))
     (setf *refill-buffers* t))

    ;; i to show gl info
    ((and (eq key :i) (eq action :press))
     (show-open-gl-info))

    ;; s to show gl state
    ((and (eq key :s) (eq action :press))
     (show-gl-state))

    ;; c to toggle printing fps
    ((and (eq key :c) (eq action :press))
     (setf *show-fps* (if *show-fps* nil t)))

    ;; w to toggle wireframe
    ((and (eq key :w) (eq action :press))
     (setf *wire-frame* (if *wire-frame* nil t)))

    ((and (eq key :f1) (eq action :press))
     (setf *cull-face* (if (eq *cull-face* :cull-face) nil :cull-face)))

    ((and (eq key :f2) (eq action :press))
     (setf *front-face* (if (eq *front-face* :cw) :ccw :cw)))
    ))

(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (declare (ignorable window button action mod-keys))
  (let ((cpos (glfw:get-cursor-position window)))
    (format t "Mouse click at ~a ~a ~a ~a ~a~%" cpos window button action mod-keys)))

(def-scroll-callback scroll-handler (window x y)
  (let ((cpos (glfw:get-cursor-position window)))
    (format t "Scroll at ~a ~a ~a ~a ~%" cpos window x y)))

(def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

(def-framebuffer-size-callback resize-handler (window width height)
  (declare (ignorable window))
  (format t "Resize...~%")
  (gl:viewport 0 0 width height))

(defun viewer-thread-function ( object )
  (set-error-callback 'error-callback)
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height))
           (previous-seconds 0.0)
           (frame-count 1))
      (with-window (:title (format nil "OpenGL Scene Viewer (~,3f)" 0.0)
                           :width (/ cur-width 2)
                           :height (/ cur-height 2)
                           :decorated t
                           ;; :monitor monitor
                           :opengl-profile :opengl-core-profile
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-forward-compat t
                           :samples 4
                           :resizable t)
        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'quit-on-escape)
        (set-mouse-button-callback 'mouse-handler)
        (set-scroll-callback 'scroll-handler)
        (set-framebuffer-size-callback 'resize-handler)

        ;; (gl:viewport 0 0 cur-width cur-height)

        (gl:enable :line-smooth
                   :polygon-smooth
                   :depth-test)
        (gl:depth-func :less)

        (gl:clear-color 0.7f0 0.7f0 0.7f0 1.0)
        (fill-buffers object)
        (rebuild-shaders object)
        ;; The event loop
        (loop
           until (window-should-close-p)

           for current-seconds = (get-time)
           for elapsed-seconds = (- current-seconds  previous-seconds)

           when (> elapsed-seconds 0.25) do
             (setf previous-seconds current-seconds)
             (when *show-fps*
               (format t "OpenGL Scene Viewer (~,3f)~%" (/ frame-count elapsed-seconds)))
             (set-window-title (format nil "OpenGL Scene Viewer (~,3f)" (/ elapsed-seconds frame-count)))
             (setf frame-count 0)


           when *rebuild-shaders* do
             (format t "Rebuilding shaders...")
             (rebuild-shaders object)
             (format t " Done.~%")
             (setf *rebuild-shaders* nil)

           when *refill-buffers* do
             (format t "Refilling buffers...")
             (cleanup object)
             (fill-buffers object)
             (format t " Done.~%")
             (setf *refill-buffers* nil)

           do
             (gl:clear :color-buffer :depth-buffer)
             (if *cull-face*
                 (gl:enable :cull-face)
                 (gl:disable :cull-face))
             (gl:front-face *front-face*)
             (if *wire-frame*
                 (gl:polygon-mode :front-and-back :line)
                 (gl:polygon-mode :front-and-back :fill))
             (render object)
             (incf frame-count)
           do (swap-buffers)
           do (poll-events))))))

(defun show (object &optional (in-thread nil))
  (if in-thread
      (viewer-thread-function object)
      (trivial-main-thread:with-body-in-main-thread ()
        (viewer-thread-function object))))

(defun hello ()
  (let ((mandel (make-instance 'mandelbrot)))

    (show mandel)))
