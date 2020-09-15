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

(defparameter *debug-stream* nil
  "Show or hide debug messages.  Toggle with 'd' key.")

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


(defparameter *scene* nil
  "Current scene being displayed.")

(defgeneric cleanup (obj)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

(defparameter *want-forward-context*
  #+(or windows linux bsd :freebsd) nil #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")

(defun show-gl-state ()
  "Print debug information about the OpenGL state to *debug-stream*."
  (when *debug-stream*
    (loop
       for field in '(:active-texture
                      :array-buffer-binding
                      :blend
                      :current-program
                      :line-width
                      :vertex-array-binding
                      :viewport)
       do
         (format *debug-stream* "~a : ~a~%" field (gl:get-integer field)))))

(defun show-program-state (program)
  "Print shader program state to *debug-stream*"
  (when *debug-stream*
    (loop
       for field in '(:link-status :attached-shaders)
       do
         (format *debug-stream* "~a : ~a~%" field (gl:get-program program field)))))

(defun show-open-gl-info ()
  "Print OpenGL limits to *debug-stream*"
  (when *debug-stream* 
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
         (format *debug-stream* "~a : ~a~%" field (gl:get-integer field)))))



;; Keyboard callback.
;; Implements top-level key handler, and forwards unhandled events to *scene*
(def-key-callback keyboard-handler (window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (when *debug-stream* (format *debug-stream* "Keypress: ~a ~a ~a ~a ~a~%" window key scancode action mod-keys))
  (when *scene*
    (handle-key *scene* window key scancode action mod-keys)))

;; Mouse handler callback
;; Forwards mouse events to *scene*
(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (declare (ignorable window button action mod-keys))
  
  (let* ((cpos (glfw:get-cursor-position window))
         (click-info (make-instance 'mouse-click
                                    :cursor-pos cpos
                                    :mod-keys mod-keys
                                    :action action
                                    :button button
                                    :time (get-time))))
    (when *debug-stream*
      (format *debug-stream* "Mouse click at ~a ~a ~a ~a ~a~%" cpos window button action mod-keys))

    ;; If no objects handle mouse clicks then save the click and release positions for later.
    ;; TODO: Update gl-fractals/complex-fractal.lisp to save clicks to complex-fractal member variable
    ;; and get rid of *mouse-release-info* and *mouse-press-info*
    (when (not (handle-click *scene* window click-info))
      (when (eq action :press)
        (setf *mouse-release-info* nil)
        (setf *mouse-press-info* click-info))
      (when (eq action :release)
        (setf *mouse-press-info* nil)
        (setf *mouse-release-info* click-info)))))

;; GLFW scroll handler
;; Forwards scroll events to *scene*
(def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (let ((cpos (glfw:get-cursor-position window)))
    (when *debug-stream*
      (format *debug-stream* "Scroll at ~a ~a ~a ~a ~%" cpos window x-scroll y-scroll))
    (handle-scroll *scene* window cpos x-scroll y-scroll)))

;; GLFW error callback
(def-error-callback error-callback (message)
  (when *debug-stream*
    (format *debug-stream* "Error: ~a~%" message)))

;; Resize event handler
;; Forwards events to *scene*
(def-framebuffer-size-callback resize-handler (window width height)
  (declare (ignorable window))
  (gl:viewport 0 0 width height)

  (when *debug-stream* (format *debug-stream* "framebuffer-size ~a ~a~%" width height))
  (handle-resize *scene*  window width height))


(defun viewer-thread-function (scene
                               &key
                                 (background-color (vec4 0.7f0 0.7f0 0.7f0 1.0)))
  "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
   and runs an event loop."

  (set-error-callback 'error-callback)

  (setf *scene* scene)

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
        (dolist (object (objects *scene*))
          (reload-object object))

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
                (format t "OpenGL Scene Viewer (~,3f)~%" (/ frame-count elapsed-seconds))
                (set-window-title (format nil "OpenGL Scene Viewer (~,3f)" (/ frame-count elapsed-seconds))))
              (setf frame-count 0)

              ;; Save info about the mouse drag.
              ;; TODO: clean this up or handle in scene...
              when (and (not (null *mouse-press-info*))
                        (null *mouse-release-info*))
              do
              (let* ((cpos (glfw:get-cursor-position *window*))
                     (handled (handle-drag scene *window* *previous-mouse-drag* cpos)))
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
              (update *scene* elapsed-time)
              do
              ;; Draw the scene
              (gl:clear :color-buffer :depth-buffer)
              (if *cull-face*
                  (gl:enable :cull-face)
                  (gl:disable :cull-face))
              (gl:front-face *front-face*)
              (gl:polygon-mode :front-and-back (if *wire-frame* :line :fill))

              (render scene (meye 4))
              (incf frame-count)

              do (swap-buffers)
              do (poll-events))

        ;; Cleanup before exit
        (cleanup *scene*)))))

(defun turn-on-traces ()
  "Trace functions useful for debugging"
  (trace
   ;; newgl defgenerics
   use-shader-program
   get-source
   compile-shader
   use-shader
   cleanup
   use-uniform
   enable-layout
   render
   build-shader-program
   set-uniform
   set-uniforms
   update
   fill-buffers
   handle-key
   handle-click
   handle-scroll
   handle-drag
   handle-resize
   reload-object


   ;; newgl defuns
   newgl::make-shader-program
   newgl::make-plastic-program
   newgl::add-point
   newgl::glsl-type-keyword
   newgl::glsl-type-size
   newgl::lookup-shader-type
   newgl::shader-from-file
   newgl::make-uv-quad
   newgl::make-uv-quad
   newgl::show-gl-state
   newgl::show-program-state
   newgl::show-open-gl-info
   newgl::use-shader-uniforms
   newgl::use-uniform
   newgl::viewer-thread-function
   newgl::viewer
   newgl::view-stl
   newgl::make-layout-entry
   newgl::make-layout
   newgl::compute-stride
   newgl::ensure-vao-bound
   newgl::to-gl-float-array
   newgl::to-gl-array

   ;; OpenGL
   gl:alloc-gl-array
   gl:attach-shader
   gl:bind-buffer
   gl:bind-vertex-array
   gl:buffer-data
   gl:clear
   gl:clear-color
   gl:compile-shader
   gl:create-program
   gl:create-shader
   gl:delete-buffers
   gl:delete-program
   gl:delete-shader
   gl:delete-vertex-arrays
   gl:depth-func
   gl:detach-shader
   gl:draw-elements
   gl:disable
   gl:enable
   gl:enable-vertex-attrib-array
   gl:free-gl-array
   gl:front-face
   gl:gen-buffers
   gl:gen-vertex-array
   gl:get-attrib-location
   gl:get-integer
   gl:get-program
   gl:get-program-info-log
   gl:get-shader
   gl:get-shader-info-log
   gl:get-uniform-location
   gl:glaref
   gl:link-program
   gl:make-null-gl-array
   gl:polygon-mode
   gl:shader-source
   gl:uniform-matrix
   gl:get-uniform-location
   gl:uniformi
   gl:uniformf
   gl:uniformfv
   gl:use-program
   gl:validate-program
   gl:vertex-attrib-pointer
   gl:viewport

   ))

(defun display (object &key
                         (view-transform (meye 4))
                         (background-color (vec4 0.7f0 0.7f0 0.7f0 1.0))
                         (show-traces nil)
                         (debug nil))
  "Higher level function to display an object or scene."

  (when show-traces
    (turn-on-traces))


  (let (
        ;; If object is a scene, then it's the scene,
        ;; otherwise if scene is an object or list of objects,
        ;; then they're the objects in the scene
        (scene
             (typecase object
               (scene object)
               ((or null t)
                (make-instance 'scene
                               :objects (if object
                                            (ensure-list object)
                                            nil)
                               :xform view-transform)))))

    ;; Debug mode runs in the calling thread, with *debug-stream* set to standard output.
    (if debug
        (let ((*debug-stream* t))
          (viewer-thread-function scene
                                  :background-color background-color))
        (trivial-main-thread:with-body-in-main-thread ()
          (viewer-thread-function scene
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
