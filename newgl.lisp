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

#+(or windows linux bsd :freebsd) (defparameter *want-forward-context* nil)
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
    ((and (eq key :b) (eq action :press))
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
    ((and (eq key :f) (eq action :press))
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
    (handle-key *scene* window key scancode action mod-keys)))

(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (declare (ignorable window button action mod-keys))
  (let* ((cpos (glfw:get-cursor-position window))
         (click-info (make-instance 'mouse-click
                                    :cursor-pos cpos
                                    :mod-keys mod-keys
                                    :action action
                                    :button button
                                    :time (get-time))))
    (when *debug-stream* (format *debug-stream* "Mouse click at ~a ~a ~a ~a ~a~%" cpos window button action mod-keys))
    (when (not (handle-click *scene* window click-info))
      (when (eq action :press)
        (setf *mouse-release-info* nil)
        (setf *mouse-press-info* click-info))
      (when (eq action :release)
        (setf *mouse-press-info* nil)
        (setf *mouse-release-info* click-info)))))

(def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (let ((cpos (glfw:get-cursor-position window)))
    (when *debug-stream* (format *debug-stream* "Scroll at ~a ~a ~a ~a ~%" cpos window x-scroll y-scroll))
    (handle-scroll *scene*  window cpos x-scroll y-scroll)))

(def-error-callback error-callback (message)
  (when *debug-stream* (format *debug-stream* "Error: ~a~%" message)))

(def-framebuffer-size-callback resize-handler (window width height)
  (declare (ignorable window))
  (gl:viewport 0 0 width height)

  (when *debug-stream* (format *debug-stream* "framebuffer-size ~a ~a~%" width height))
  (handle-resize *scene*  window width height))

(defun viewer-thread-function ( scene
                               &key
                                 (background-color (vec4 0.7f0 0.7f0 0.7f0 1.0)))
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
        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'keyboard-handler)
        (set-mouse-button-callback 'mouse-handler)
        (set-scroll-callback 'scroll-handler)
        (set-framebuffer-size-callback 'resize-handler)

        (gl:enable :line-smooth
                   :polygon-smooth
                   :depth-test)
        (gl:depth-func :less)

        (gl:clear-color (vx background-color)
                        (vy background-color)
                        (vz background-color)
                        (vw background-color))

        (dolist (object (objects *scene*))
          (reload-object object))
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
             (format t "Rebuilding shaders...~%")
             (dolist (object (objects *scene*))
               (build-shader-program object))
             (format t " Done.~%")
             (setf *rebuild-shaders* nil)

           when *refill-buffers* do
             (format t "Refilling buffers...")
             (dolist (object (objects *scene*))
               (cleanup object)
               (fill-buffers object))
             (format t " Done.~%")
             (setf *refill-buffers* nil)

           when (and (not (null *mouse-press-info*))
                     (null *mouse-release-info*))
           do (let* ((cpos (glfw:get-cursor-position *window*))
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
             (update *scene*)
           do
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
        (cleanup *scene*)))))

(defun display (object &key
                         (view-transform nil)
                         (background-color (vec4 0.7f0 0.7f0 0.7f0 1.0))
                         (show-traces nil)
                         (debug nil))
  ;; Some traces that are helpful for debugging
  ;; newgl defgenerics
  (when show-traces
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
     newgl::top-key-handler
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

  (if debug
      ;; Always print to standard out when in-thread, because it's probably for debugging...
      (let ((*debug-stream* t)
            (scene
             (if object
                 (typecase object
                   (scene object)
                   (t (make-instance 'scene
                                     :objects (list object)
                                     :xform (if view-transform
                                                view-transform
                                                (meye 4)))))
                   (make-instance 'scene
                                  :objects nil
                                  :xform (if view-transform
                                             view-transform
                                             (meye 4))))))
        (viewer-thread-function scene
                                :background-color background-color))
      (trivial-main-thread:with-body-in-main-thread ()
        (let ((scene
               (if object
                   (typecase object
                     (scene object)
                     (t (make-instance 'scene
                                       :objects (list object)
                                       :xform (if view-transform
                                                  view-transform
                                                  (meye 4)))))
                   (make-instance 'scene :objects nil
                                  :xform (if view-transform
                                             view-transform
                                             (meye 4))))))
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

(defun to-rectangular (delta)
  "Convert a length and angle (polar coordinates) into x,y rectangular coordinates."
  (vec3
   (* (vz delta) (cos (vy delta)) (cos (vx delta)))
   (* (vz delta) (sin (vy delta)) (cos (vx delta)))
   (* (vz delta) (sin (vx delta)))))

(defun fractal-tree (&key (maxdepth 4) (theta-limbs 2) (phi-limbs 2) (color (vec4 0 1 0 1)))
  "Draw a fractal tree into the specified file, recursing to maxdepth, with the specified number of limbs at each level."
  (let ((prims (make-instance 'line-segments)))
    (labels
        ((draw-tree (current delta depth)
           ;;(add-point prims current color)
           (add-line-2 prims :p1 current :c1 color :p2 (v+ current (to-rectangular delta)) :c2 color)
           (when (> depth 0)
             (let ((next-base (v+ current (to-rectangular delta)))
                   (d-phi (/ (* 2 pi) phi-limbs))
                   (d-theta (/ pi phi-limbs)))
               (dotimes (i theta-limbs)
                 (dotimes (j phi-limbs)
                   (draw-tree
                    next-base
                    (vec3 (+ (/ (vx delta) 3.0) (random 0.0125))
                          (+ (* i d-phi) (random 0.75))
                          (+ (* j d-theta) (random 0.75)))
                    (- depth 1))))))))
      (draw-tree (vec3 0 0 0) (vec3 0.5 pi 0) maxdepth))
    prims))
