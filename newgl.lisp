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

;; Flags to communicate between handlers and main loop.
(defparameter *rebuild-shaders* nil
  "Flag set by keyboard handler to signal rebuilding shaders.  Trigger with 'r' key.")

(defparameter *refill-buffers* nil
  "Flag set by keyboard handler to signal refill buffers.  Trigger with 'f' key.")

(defparameter *show-fps* nil
  "Flag to turn on or off printing FPS.  Toggle with 'c' key.")

(defparameter *wire-frame* t
  "Toggle filled or wireframe drawing. Trigger with 'w' key.")

(defparameter *cull-face* :cull-face
  "Enable face culling when non-nil.  Toggle with 'F1' key.")

(defparameter *front-face* :ccw
  "Front face direction setting.  Toggle between :ccw (default) and :cw with 'F2' key.")

(defparameter *debug-stream* nil
  "Show or hide debug messages.  Toggle with 'd' key.")

(defparameter *mouse-press-position* nil
  "Location of last mouse press.")

(defparameter *mouse-release-position* nil
  "Location of last mouse release.")


(defparameter *objects* nil
  "Objects being displayed.")

(defgeneric cleanup (obj)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

#+(or windows linux) (defparameter *want-forward-context* nil)
#+darwin (defparameter *want-forward-context* t)

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
       (when *debug-stream* (format *debug-stream* "~a : ~a~%" field (gl:get-integer field)))))

(defun show-program-state (program)
  (loop
     for field in '(:link-status :attached-shaders)
     do
       (when *debug-stream* (format *debug-stream* "~a : ~a~%" field (gl:get-program program field)))))

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
       (when *debug-stream* (format *debug-stream* "~a : ~a~%" field (gl:get-integer field)))))

(defun top-key-handler (window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (cond

    ;; ESC to exit
    ((and (eq key :escape) (eq action :press))
     (set-window-should-close)
     t)

    ;; r to rebuild shaders
    ((and (eq key :r) (eq action :press))
     (setf *rebuild-shaders* t)
     t)

    ;; f to refill buffers
    ((and (eq key :f) (eq action :press))
     (setf *refill-buffers* t)
     t)

    ;; i to show gl info
    ((and (eq key :i) (eq action :press))
     (show-open-gl-info)
     t)

    ;; d to toggle debug printing
    ((and (eq key :d) (eq action :press))
     (setf *debug-stream* (if *debug-stream* nil t))
     t)

    ;; s to show gl state
    ((and (eq key :s) (eq action :press))
     (show-gl-state)
     t)

    ;; c to toggle printing fps
    ((and (eq key :c) (eq action :press))
     (setf *show-fps* (if *show-fps* nil t))
     t)

    ;; w to toggle wireframe
    ((and (eq key :w) (eq action :press))
     (setf *wire-frame* (if *wire-frame* nil t))
     t)

    ((and (eq key :f1) (eq action :press))
     (setf *cull-face* (if (eq *cull-face* :cull-face) nil :cull-face))
     t)

    ((and (eq key :f2) (eq action :press))
     (setf *front-face* (if (eq *front-face* :cw) :ccw :cw))
     t)
    (t
     nil)))

(def-key-callback keyboard-handler (window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (when *debug-stream* (format *debug-stream* "Keypress: ~a ~a ~a ~a ~a~%" window key scancode action mod-keys))
  (when (not (top-key-handler window key scancode action mod-keys))
    (loop for object in *objects*
       until (handle-key object window key scancode action mod-keys))))

(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (declare (ignorable window button action mod-keys))
  (let ((cpos (glfw:get-cursor-position window)))
    (when *debug-stream* (format *debug-stream* "Mouse click at ~a ~a ~a ~a ~a~%" cpos window button action mod-keys))

    (when (eq action :press)
      (setf *mouse-press-position* cpos))

    (when (eq action :release)
      (setf *mouse-release-position* cpos))

    (loop for object in *objects*
       until (handle-click object window button cpos action mod-keys))))

(def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (let ((cpos (glfw:get-cursor-position window)))
    (when *debug-stream* (format *debug-stream* "Scroll at ~a ~a ~a ~a ~%" cpos window x-scroll y-scroll))
    (loop for object in *objects*
       until (handle-scroll object window cpos x-scroll y-scroll))))

(def-error-callback error-callback (message)
  (when *debug-stream* (format *debug-stream* "Error: ~a~%" message)))

(def-framebuffer-size-callback resize-handler (window width height)
  (declare (ignorable window))
  (gl:viewport 0 0 width height))

(defun viewer-thread-function ( objects )
  (set-error-callback 'error-callback)
  (setf *objects* (ensure-list objects))

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
                           :context-version-major 4
                           :context-version-minor 0
                           :opengl-forward-compat *want-forward-context*
                           :samples 4
                           :resizable t)
        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'keyboard-handler)
        (set-mouse-button-callback 'mouse-handler)
        (set-scroll-callback 'scroll-handler)
        (set-framebuffer-size-callback 'resize-handler)

        (gl:enable :line-smooth
                   :polygon-smooth
                   :depth-test)
        (gl:depth-func :less)

        (gl:clear-color 0.7f0 0.7f0 0.7f0 1.0)
        (dolist (object *objects*)
          (fill-buffers object)
          (rebuild-shaders object))
        ;; The event loop
        (loop
           until (window-should-close-p)

           for current-seconds = (get-time)
           for elapsed-seconds = (- current-seconds  previous-seconds)

           when (> elapsed-seconds 0.25) do
             (setf previous-seconds current-seconds)
             (when *show-fps*
               (format t "OpenGL Scene Viewer (~,3f)~%" (/ frame-count elapsed-seconds)))
             (set-window-title (format nil "OpenGL Scene Viewer (~,3f)" (/ frame-count elapsed-seconds)))
             (setf frame-count 0)


           when *rebuild-shaders* do
             (format t "Rebuilding shaders...")
             (dolist (object *objects*)
               (rebuild-shaders object))
             (format t " Done.~%")
             (setf *rebuild-shaders* nil)

           when *refill-buffers* do
             (format t "Refilling buffers...")
             (dolist (object *objects*)
               (cleanup object)
               (fill-buffers object))
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

             (dolist (object *objects*)
               (render object))
             (incf frame-count)
           do (swap-buffers)
           do (poll-events))
        (dolist (object *objects*)
          (cleanup object))))))

(defun viewer (&key (objects (make-instance 'mandelbrot)) (in-thread nil) (show-traces nil))
  ;; Some traces that are helpful for debugging
  (when show-traces
    (trace
     gl:bind-buffer
     gl:bind-vertex-array
     gl:draw-elements
     gl:enable-vertex-attrib-array
     gl:gen-vertex-array
     gl:get-attrib-location
     gl:polygon-mode
     gl:use-program
     gl:vertex-attrib-pointer

     newgl::build-shader-program
     newgl::ensure-vao-bound
     newgl::fill-buffers
     newgl::render
     newgl::use-layout
     newgl::use-shader-program))

  (if in-thread
      (viewer-thread-function objects)
      (trivial-main-thread:with-body-in-main-thread ()
        (viewer-thread-function objects))))

