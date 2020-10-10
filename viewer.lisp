;; viewer.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass viewer ()
  ((window :initform nil :initarg :window :accessor window)
   (objects :initform nil :initarg :objects :type (or null cons) :accessor objects)
   (view-xform :initform (meye 4) :initarg :xform :type mat4 :accessor viewport)
   (aspect-ratio :initform 1.0 :initarg :aspect-ratio :type real :accessor aspect-ratio)
   (show-fps :initform nil :initarg :show-fps :type t :accessor show-fps)
   (wire-frame :initform nil :initarg :wire-frame :type t :accessor wire-frame-p)
   (cull-face :initform :cull-face :initarg :cull-face :accessor cull-face)
   (front-face :initform :ccw :initarg :front-face :accessor front-face)
   (background-color :initform (vec4 0.7f0 0.7f0 0.7f0 1.0) :initarg :background :accessor background-color)
   (mouse-press-info :initform nil)
   (mouse-release-info :initform nil)
   (previous-mouse-drag :initform nil)
   (previous-seconds :initform 0.0)
   (frame-count :initform 1))
  (:documentation "A collection of objects and a viewport."))

(defmethod render ((viewer viewer) xform)
  (with-slots (objects view-xform) viewer
    (loop for object in objects
          do
          (render object (m* xform view-xform)))))

(defmethod cleanup ((viewer viewer))
  (loop for object in (objects viewer)
        do
        (cleanup object)))

(defmethod update ((viewer viewer) elapsed-seconds)
  (loop for object in (objects viewer)
        do
        (update object elapsed-seconds)))

(defmethod reload-object ((viewer viewer))
  (dolist (object (objects viewer))
    (reload-object object)))

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

(defmethod handle-key ((viewer viewer) window key scancode action mod-keys)
  (cond
    ;; ESC to exit
    ((and (eq key :escape) (eq action :press))
     (set-window-should-close window)
     t)

    ;; r to rebuild shaders
    ((and (eq key :r) (eq action :press))
     (format t "Rebuilding shaders...~%")
     (with-slots (objects) viewer
       (dolist (object objects)
         (build-shader-program object)))
     (format t " Done.~%")
     t)

    ;; f to refill buffers
    ((and (eq key :b) (eq action :press))
     (reload-object viewer)
     t)

    ;; i to show gl info
    ((and (eq key :i) (eq action :press))
     (show-open-gl-info)
     t)

    ;; s to show gl state
    ((and (eq key :s) (eq action :press))
     (show-gl-state)
     t)

    ;; c to toggle printing fps
    ((and (eq key :f) (eq action :press))
     (with-slots (show-fps) viewer
       (setf show-fps (not show-fps))
       t))

    ;; w to toggle wireframe
    ((and (eq key :w) (eq action :press))
     (with-slots (wire-frame) viewer
       (setf wire-frame (if wire-frame nil t))
       t))

    ;; f1
    ((and (eq key :f1) (eq action :press))
     (with-slots (cull-face) viewer
       (setf cull-face (if (eq cull-face :cull-face)
                           nil
                           :cull-face))
       t))

    ((and (eq key :f2) (eq action :press))
     (with-slots (front-face) viewer
       (setf front-face (if (eq front-face :cw)
                            :ccw
                            :cw))
       t))
    (t
     (funcall #'some #'identity
              (loop for object in (objects viewer)
                    collect (handle-key object window key scancode action mod-keys))))))


(defmethod handle-resize ((viewer viewer) window width height)
  (with-slots (aspect-ratio) viewer
    (gl:viewport 0 0 width height)
    (setf aspect-ratio
          (if (< width height )
              (/ height width 1.0)
              (/ width height 1.0))))
  (loop
        for object in (objects viewer)
        do
        (handle-resize object window width height)))

(defmethod handle-click ((viewer viewer) window click-info)
  (loop
        for object in (objects viewer)
        do
        (handle-click object window click-info)))

(defmethod handle-scroll ((viewer viewer) window cpos x-scroll y-scroll)
  (loop
        for object in (objects viewer)
        do
        (handle-scroll object window cpos x-scroll y-scroll)))

(defun init ()
  (glfw:initialize))

(defun terminate ()
  (glfw:terminate))

(defmethod display ((viewer viewer) &key
                                      (view-transform nil)
                                      (background-color (vec4 0.7f0 0.7f0 0.7f0 1.0))
                                      (debug nil))
  (declare (ignorable debug background-color view-transform))
  "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
   and runs an event loop."
  (init)

  (glfw:set-error-callback 'error-callback)

  (let* ((window (create-window :title (format nil "OpenGL Viewer Viewer (~,3f)" 0.0)
                                :width 1000
                                :height 1000
                                :decorated t
                                :opengl-profile :opengl-core-profile
                                :context-version-major 4
                                :context-version-minor 0
                                :opengl-forward-compat *want-forward-context*
                                :samples 1
                                :resizable t)))
    
    ;; GLFW Initialization
    (setf %gl:*gl-get-proc-address* #'get-proc-address)

    (setf (gethash (cffi:pointer-address window) *viewers*) viewer)
    (set-key-callback 'keyboard-handler window)
    (set-mouse-button-callback 'mouse-handler window)
    (set-scroll-callback 'scroll-handler window)
    (set-framebuffer-size-callback 'resize-handler window)

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
    (reload-object viewer)

    ;; The event loop
    (with-slots (previous-seconds show-fps mouse-press-info mouse-release-info previous-mouse-drag
                                  cull-face front-face wire-frame view-xform)
        viewer
      (loop
            with start-time = (get-time)
            with frame-count = 0
            until (window-should-close-p window)

            for current-seconds = (get-time)
            for elapsed-seconds = (- current-seconds  previous-seconds)
            for elapsed-time = (- (get-time) start-time)

            when (and show-fps (> elapsed-seconds 0.25))
            do
            (format t "OpenGL Viewer Viewer (~,3f)~%" (/ frame-count elapsed-seconds))
            (set-window-title (format nil "OpenGL Viewer Viewer (~,3f)" (/ frame-count elapsed-seconds)))
            (setf previous-seconds current-seconds)
            (setf frame-count 0)

            ;; This do is important...
            do
            ;; Update for next frame
            (with-context window
              (update viewer elapsed-time))

            ;; Draw the viewer
            (gl:clear :color-buffer :depth-buffer)
            (if cull-face
                (gl:enable :cull-face)
                (gl:disable :cull-face))

            (gl:front-face front-face)
            (gl:polygon-mode :front-and-back (if wire-frame :line :fill))

            (with-context window
              (render viewer (m* view-transform view-xform)))

            (incf frame-count)

            do (swap-buffers window)
            do (poll-events))
      

      ;; Cleanup before exit
      (cleanup viewer)
      (remhash window *viewers*)
      (glfw:destroy-window window))))
