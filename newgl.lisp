;; newgl.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


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
  'vec3)

(deftype normal ()
  'vec3)

(deftype color ()
  'vec4)

;; Flags to communicate between handlers and main loop.
(defparameter *rebuild-shaders* nil
  "Flag set by keyboard handler to signal shaders should be rebuilt.  Trigger with 'r' key.")

(defparameter *refill-buffers* nil
  "Flag set by keyboard handler to signal buffers should be refilled.  Trigger with 'b' key.")

(defparameter *show-fps* nil
  "Flag to turn on or off printing FPS.  Toggle with 'f' key.")

(defparameter *wire-frame* t
  "Toggle filled or wireframe drawing. Trigger with 'w' key.")

(defparameter *cull-face* :cull-face
  "Enable face culling when non-nil.  Toggle with 'F1' key.")

(defparameter *front-face* :ccw
  "Front face direction setting.  Toggle between :ccw (default) and :cw with 'F2' key.")

(defclass mouse-click ()
  ((cursor-pos :initarg :cursor-pos)
   (button :initarg :button)
   (action :initarg :action)
   (mod-keys :initarg :mod-keys)
   (time :initarg :time))
  (:documentation "Mouse click information."))

(defparameter *mouse-press-info* nil
  "Location of last mouse press.")

(defparameter *previous-mouse-drag* nil
  "Location of last mouse drag update.")

(defparameter *mouse-release-info* nil
  "Location of last mouse release.")


(defparameter *viewer* nil
  "Current viewer being displayed.")

(defgeneric cleanup (obj)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

(defparameter *want-forward-context*
  #+(or windows linux bsd :freebsd) nil #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")

(defun show-gl-state ()
  "Print debug information about the OpenGL state."
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

(defun show-program-state (program)
  "Print shader program state."
  (loop
        for field in '(:link-status :attached-shaders)
        do
        (format t "~a : ~a~%" field (gl:get-program program field))))

(defun show-open-gl-info ()
  "Print OpenGL limits"
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
                       :texture-binding-2d
                       :stereo)
        do
        (format t "~a : ~a~%" field (gl:get-integer field))))



;; Keyboard callback.
;; Implements top-level key handler, and forwards unhandled events to *viewer*
(def-key-callback keyboard-handler (window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (when *viewer*
    (handle-key *viewer* window key scancode action mod-keys)))

;; Mouse handler callback
;; Forwards mouse events to *viewer*
(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (declare (ignorable window button action mod-keys))
  
  (let* ((cpos (glfw:get-cursor-position window))
         (click-info (make-instance 'mouse-click
                                    :cursor-pos cpos
                                    :mod-keys mod-keys
                                    :action action
                                    :button button
                                    :time (get-time))))
    ;; If no objects handle mouse clicks then save the click and release positions for later.
    ;; TODO: Update gl-fractals/complex-fractal.lisp to save clicks to complex-fractal member variable
    ;; and get rid of *mouse-release-info* and *mouse-press-info*
    (when (not (handle-click *viewer* window click-info))
      (when (eq action :press)
        (setf *mouse-release-info* nil)
        (setf *mouse-press-info* click-info))
      (when (eq action :release)
        (setf *mouse-press-info* nil)
        (setf *mouse-release-info* click-info)))))

;; GLFW scroll handler
;; Forwards scroll events to *viewer*
(def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (let ((cpos (glfw:get-cursor-position window)))
    (handle-scroll *viewer* window cpos x-scroll y-scroll)))

;; GLFW error callback
(def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

;; Resize event handler
;; Forwards events to *viewer*
(def-framebuffer-size-callback resize-handler (window width height)
  (declare (ignorable window))
  (gl:viewport 0 0 width height)

  (handle-resize *viewer*  window width height))


(defun viewer-thread-function (viewer
                               &key
                                 (background-color (vec4 0.7f0 0.7f0 0.7f0 1.0)))
  "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
   and runs an event loop."

  (set-error-callback 'error-callback)

  (setf *viewer* viewer)

  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height))
           (previous-seconds 0.0)
           (frame-count 1))

      (with-window (:title (format nil "OpenGL Viewer Viewer (~,3f)" 0.0)
                           :width (/ cur-width 2)
                           :height (/ cur-height 2)
                           :decorated t
                           ;; :monitor monitor
                           :opengl-profile :opengl-core-profile
                           :context-version-major 4
                           :context-version-minor 0
                           :opengl-forward-compat *want-forward-context*
                           :samples 1
                           :resizable t)
        ;; GLFW Initialization
        (setf %gl:*gl-get-proc-address* #'get-proc-address)

        (set-key-callback 'keyboard-handler)
        (set-mouse-button-callback 'mouse-handler)
        (set-scroll-callback 'scroll-handler)
        (set-framebuffer-size-callback 'resize-handler)

        ;; Initialize OpenGL state
        (gl:enable :line-smooth
                   :polygon-smooth
                   :depth-test
                   )

        (gl:depth-func :less)

        (gl:clear-color (vx background-color)
                        (vy background-color)
                        (vz background-color)
                        (vw background-color))

        ;; Load objects for the first time
        (reload-object *viewer*)

        ;; The event loop
        (loop
              with start-time = (get-time)

              until (window-should-close-p)

              for current-seconds = (get-time)
              for elapsed-seconds = (- current-seconds  previous-seconds)
              for elapsed-time = (- (get-time) start-time)
              when (> elapsed-seconds 0.25) do
              (setf previous-seconds current-seconds)
              (when *show-fps*
                (format t "OpenGL Viewer Viewer (~,3f)~%" (/ frame-count elapsed-seconds))
                (set-window-title (format nil "OpenGL Viewer Viewer (~,3f)" (/ frame-count elapsed-seconds))))
              (setf frame-count 0)

              ;; Save info about the mouse drag.
              ;; TODO: clean this up or handle in viewer...
              when (and (not (null *mouse-press-info*))
                        (null *mouse-release-info*))
              do
              (let* ((cpos (glfw:get-cursor-position *window*))
                     (handled (handle-drag viewer *window* *previous-mouse-drag* cpos)))
                (when (not handled)
                  (setf *previous-mouse-drag* (with-slots (mod-keys action button time) *mouse-press-info*
                                                (make-instance 'mouse-click
                                                               :cursor-pos cpos
                                                               :mod-keys mod-keys
                                                               :action action
                                                               :button button
                                                               :time (get-time))))))
              do
              ;; Update for next frame
              (update *viewer* elapsed-time)
              do
              ;; Draw the viewer
              (gl:clear :color-buffer :depth-buffer)
              (if *cull-face*
                  (gl:enable :cull-face)
                  (gl:disable :cull-face))
              (gl:front-face *front-face*)
              (gl:polygon-mode :front-and-back (if *wire-frame* :line :fill))

              (render viewer (meye 4))

              (incf frame-count)

              do (swap-buffers)
              do (poll-events))

        ;; Cleanup before exit
        (cleanup *viewer*)))))

(defun display (object &key
                         (view-transform (meye 4))
                         (background-color (vec4 0.7f0 0.7f0 0.7f0 1.0))
                         (debug nil))
  "High level function to display an object or viewer."

  (let (
        ;; If object is a viewer, then it's the viewer,
        ;; otherwise if viewer is an object or list of objects,
        ;; then they're the objects in the viewer
        (viewer
             (typecase object
               (viewer object)
               ((or null t)
                (make-instance 'viewer
                               :objects (if object
                                            (ensure-list object)
                                            nil)
                               :xform view-transform)))))

    (if debug
        (viewer-thread-function viewer
                                :background-color background-color)
        (trivial-main-thread:with-body-in-main-thread ()
          (viewer-thread-function viewer
                                  :background-color background-color)))))

#+stl-to-open-gl
(defun view-stl (stl-file-name)
  (let ((stl (stl:read-stl stl-file-name)))
    (multiple-value-bind (verts idxs) (stl:to-opengl stl)
      (let* ((tm (make-instance 'newgl:tri-mesh
                                :vertices verts
                                :indices idxs))
             (xform (3d-matrices:m*
                     (3d-matrices:mrotation (3d-vectors:vec3 1.0 0.0 0.0) (/ pi 8))
                     (3d-matrices:mrotation (3d-vectors:vec3 0.0 1.0 0.0) (/ pi 8))))
             (normal-xform (3d-matrices:mtranspose
                            (3d-matrices:minv
                             (3d-matrices:mblock xform 0 0 3 3)))))
        (newgl:set-uniform tm "transform" xform)
        (newgl:set-uniform tm "normalTransform" normal-xform)
        (newgl:set-uniform tm "mode" 1)
        (newgl:display tm)))))
