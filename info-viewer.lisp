;; info-viewer.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)




(defclass info-viewer ()
  ((objects :initform nil
            :initarg :objects
            :type (or null cons)
            :accessor objects)
   (view-xform :initform (meye 4)
               :initarg :xform
               :type mat4
               :accessor viewport)
   (window :initform nil))
  (:documentation "A collection of objects and a viewport."))

(defmethod render ((viewer info-viewer))
  nil)

(defmethod cleanup ((viewer info-viewer))
  (loop for object in (objects viewer)
        do
           (cleanup object)))

(defmethod update ((viewer info-viewer) elapsed-seconds)
  (with-slots (objects view-xform) viewer
    (loop for object in objects
          do
             (update object elapsed-seconds))))

(defmethod initialize ((viewer info-viewer))
  (dolist (object (objects viewer))
    (initialize object)
    (set-uniform object "view_transform" view-xform)))

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

(defmethod handle-key ((viewer info-viewer) window key scancode action mod-keys)
  nil)


(defmethod handle-resize ((viewer info-viewer) window width height)
  nil)

(defmethod handle-click ((viewer info-viewer) window click-info)
  nil)

(defmethod handle-scroll ((viewer info-viewer) window cpos x-scroll y-scroll)
  nil)

(defmethod display ((viewer info-viewer))
  "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
   and runs an event loop."
  (glfw:set-error-callback 'error-callback)
  (init)


  (let* ((window (glfw:create-window :title "OpenGL Viewer"
                                :width 100
                                :height 100
                                :decorated nil
                                :opengl-profile :opengl-core-profile
                                :context-version-major 4
                                :context-version-minor 0
                                :opengl-forward-compat *want-forward-context*
                                :samples 0
                                :resizable t)))
    (when (null window)
      (format t "Could not create-window!")
      (error "Could not create-window!"))

    (unwind-protect
         (progn
           ;; GLFW Initialization
           (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

           (add-viewer window viewer)

           (glfw:set-key-callback 'keyboard-handler window)
           (glfw:set-mouse-button-callback 'mouse-handler window)
           (glfw:set-scroll-callback 'scroll-handler window)
           (glfw:set-framebuffer-size-callback 'resize-handler window)

           ;; Initialize OpenGL state
           (gl:enable :line-smooth
                      :polygon-smooth
                      :depth-test
                      :blend
                      )
           (gl:blend-func :src-alpha :one-minus-src-alpha)
           (gl:depth-func :less)

           ;; The event loop
           (with-slots (objects) viewer

             (gl:clear-color 0.0 0.0 0.0 1.0)
             ;; Load objects for the first time
             (initialize viewer)

             ;; Apply viewer-wide drawing settings
             (gl:clear :color-buffer :depth-buffer)

             (render viewer)
              (glfw:swap-buffers window)
             
             (glfw:poll-events)
             (glfw:set-window-should-close window)
             ;; Cleanup before exit
             (cleanup viewer)
             (rm-viewer window)))
      (glfw:destroy-window window))))
