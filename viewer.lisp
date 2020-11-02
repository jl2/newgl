;; viewer.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass mouse-click ()
  ((cursor-pos :initarg :cursor-pos)
   (button :initarg :button)
   (action :initarg :action)
   (mod-keys :initarg :mod-keys)
   (time :initarg :time))
  (:documentation "Mouse click information."))


(defparameter *want-forward-context*
  #+(or windows linux freebsd) nil
  #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")


(let ((viewers (make-hash-table :test 'equal)))

  (defun find-viewer (window)
    (gethash (cffi:pointer-address window) viewers))

  (defun add-viewer (window viewer)
    (setf (gethash (cffi:pointer-address window) viewers) viewer))

  (defun rm-viewer (window)
    (remhash (cffi:pointer-address window) viewers))

  (defun rm-all-viewers ()
    (loop for window being the hash-keys of viewers
      using (hash-value viewer)
          do
             (cleanup viewer)
             (glfw:destroy-window window))
    (setf viewers (make-hash-table :test 'equal))))

;; Keyboard callback.
(def-key-callback keyboard-handler (window key scancode action mod-keys)
  (when-let (viewer (find-viewer window))
    (handle-key viewer window key scancode action mod-keys)))

;; Mouse handler callback
(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (when-let (viewer (find-viewer window))
    (let* ((cpos (glfw:get-cursor-position window))
           (click-info (make-instance 'mouse-click
                                      :cursor-pos cpos
                                      :mod-keys mod-keys
                                      :action action
                                      :button button
                                      :time (get-time))))
      (handle-click viewer window click-info))))

;; GLFW scroll handler
(def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (when-let (viewer (find-viewer window))
    (let ((cpos (glfw:get-cursor-position window)))
      (handle-scroll viewer window cpos x-scroll y-scroll))))

;; GLFW error callback
(def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

;; Resize event handler
(def-framebuffer-size-callback resize-handler (window width height)
  (when-let (viewer (find-viewer window))
    (handle-resize viewer window width height)))



(defclass viewer ()
  ((objects :initform nil
            :initarg :objects
            :type (or null cons)
            :accessor objects)
   (view-xform :initform (meye 4)
               :initarg :xform
               :type mat4
               :accessor viewport)
   (aspect-ratio :initform 1.0
                 :initarg :aspect-ratio
                 :type real
                 :accessor aspect-ratio)
   (show-fps :initform nil
             :initarg :show-fps
             :type t
             :accessor show-fps)
   (wire-frame :initform nil
               :initarg :wire-frame
               :type t
               :accessor wire-frame-p)
   (cull-face :initform :cull-face
              :initarg :cull-face
              :accessor cull-face)
   (front-face :initform :ccw
               :initarg :front-face
               :accessor front-face)
   (background-color :initform (vec4 0.04f0 0.04f0 0.04f0 1.0)
                     :initarg :background
                     :accessor background-color)

   (window :initform nil)
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
     t)

    ;; f to refill buffers
    ((and (eq key :b) (eq action :press))
     (format t "Reloading buffers ~%")
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
       (format t "Wire-frame ~a~%" wire-frame)
       t))

    ;; f1
    ((and (eq key :f1) (eq action :press))
     (with-slots (cull-face) viewer
       (setf cull-face (if (eq cull-face :cull-face)
                           nil
                           :cull-face))
       (format t "Cull face: ~a~%" cull-face)
       t))

    ((and (eq key :f2) (eq action :press))
     (with-slots (front-face) viewer
       (setf front-face (if (eq front-face :cw)
                            :ccw
                            :cw))
       (format t "Front face: ~a~%" front-face)
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

(defmethod display ((object t))
  "High level function to display an object or viewer."

  (let ((viewer (make-instance 'viewer
                               :objects (ensure-list object))))
    (display viewer)))

(defmethod display ((viewer viewer))
  "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
   and runs an event loop."
  (init)

  (glfw:set-error-callback 'error-callback)

  (let* ((window (create-window :title "OpenGL Viewer"
                                :width 1000
                                :height 1000
                                :decorated nil
                                :opengl-profile :opengl-core-profile
                                :context-version-major 4
                                :context-version-minor 0
                                :opengl-forward-compat *want-forward-context*
                                :samples 1
                                :resizable t)))
    (when (null window)
      (format t "Could not create-window!")
      (error "Could not create-window!"))

    (unwind-protect
         (progn
           ;; GLFW Initialization
           (setf %gl:*gl-get-proc-address* #'get-proc-address)

           (add-viewer window viewer)

           (set-key-callback 'keyboard-handler window)
           (set-mouse-button-callback 'mouse-handler window)
           (set-scroll-callback 'scroll-handler window)
           (set-framebuffer-size-callback 'resize-handler window)

           ;; Initialize OpenGL state
           (gl:enable :line-smooth
                      :polygon-smooth
                      :depth-test
                      :blend
                      )
           (gl:blend-func :src-alpha :one-minus-src-alpha)
           (gl:depth-func :less)

           ;; The event loop
           (with-slots (previous-seconds show-fps
                        cull-face front-face wire-frame view-xform background-color)
               viewer

             (gl:clear-color (vx background-color)
                             (vy background-color)
                             (vz background-color)
                             (vw background-color))

             ;; Load objects for the first time
             (reload-object viewer)
             (loop
               with start-time = (get-time)
               for frame-count from 0
               until (window-should-close-p window)

               for current-seconds = (get-time)
               for elapsed-seconds = (- current-seconds previous-seconds)
               for elapsed-time = (- current-seconds start-time)

               when (and show-fps (> elapsed-seconds 0.25))
                 do
                    (format t "~,3f fps~%" (/ frame-count elapsed-seconds))
                    (setf previous-seconds current-seconds)
                    (setf frame-count 0)

                    ;; This do is important...
               do
                  ;; Update for next frame
                  (update viewer elapsed-time)

                  ;; Apply viewer-wide drawing settings
                  (gl:clear :color-buffer :depth-buffer)

                  (if cull-face
                      (gl:enable :cull-face)
                      (gl:disable :cull-face))

                  (gl:front-face front-face)

                  (gl:polygon-mode :front-and-back
                                   (if wire-frame
                                       :line
                                       :fill))


                  (render viewer (meye 4))
               do (swap-buffers window)
               do (poll-events))

             ;; Cleanup before exit
             (cleanup viewer)
             (rm-viewer window)))
      (glfw:destroy-window window))))
