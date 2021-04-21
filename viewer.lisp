;; viewer.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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
    (loop
      for window being the hash-keys of viewers
        using (hash-value viewer)
      do
         (cleanup viewer)
         (glfw:destroy-window window))
    (setf viewers (make-hash-table :test 'equal))))

;; Keyboard callback.
(glfw:def-key-callback keyboard-handler (window key scancode action mod-keys)
  (when-let (viewer (find-viewer window))
    (handle-key viewer window key scancode action mod-keys)))

;; Mouse handler callback
(glfw:def-mouse-button-callback mouse-handler (window button action mod-keys)
  (when-let (viewer (find-viewer window))
    (let* ((cpos (glfw:get-cursor-position window))
           (click-info (make-instance 'mouse-click
                                      :cursor-pos cpos
                                      :mod-keys mod-keys
                                      :action action
                                      :button button
                                      :time (glfw:get-time))))
      (handle-click viewer window click-info))))

;; GLFW scroll handler
(glfw:def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (when-let (viewer (find-viewer window))
    (let ((cpos (glfw:get-cursor-position window)))
      (format t "scroll ~a ~a ~a ~a~%" window cpos x-scroll y-scroll)
      (handle-scroll viewer window cpos x-scroll y-scroll))))

;; GLFW error callback
(glfw:def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

;; Resize event handler
(glfw:def-framebuffer-size-callback resize-handler (window width height)
  (when-let (viewer (find-viewer window))
    (handle-resize viewer window width height)))

(defun reset-view (viewer)
  (with-slots (aspect-ratio camera-position camera-direction camera-up view-xform) viewer
    (setf camera-position (vec4 0.0 0.0 1.0 1.0))
    (setf camera-direction (vec4 0.0 0.0 -1.0 0.0))
    (setf camera-up (vec4 0.0 1.0 0.0 0.0))
    (setf view-xform (m* (mperspective 60.0 aspect-ratio 0.1 1000.0)
                         (mlookat (vxyz camera-position)
                                  (vxyz (v+ camera-position camera-direction))
                                  (vxyz camera-up))))))
(defclass viewer ()
  ((objects :initform nil
            :initarg :objects
            :type (or null cons)
            :accessor objects)
   (view-xform :initform (m* (mperspective 60.0 1.0 0.1 1000.0)
                             (mlookat (spherical-2-cartesian (vec3 1.0 0.0 0.0))
                                      (vec3 0 0 0)
                                      +vy+))
               :initarg :xform
               :type mat4
               :accessor viewport)

   (camera-position :initform (vec4 0.0 0.0 -1.0 1.0))
   (camera-direction :initform (vec4 0.0 0.0 1.0 0.0))
   (camera-up :initform (vec4 0.0 1.0 0.0 0.0))
   (view-changed :initform t)

   (aspect-ratio :initform 1.0
                 :initarg :aspect-ratio
                 :type real
                 :accessor aspect-ratio)

   (show-fps :initform nil
             :initarg :show-fps
             :type t
             :accessor show-fps)
   (desired-fps :initform 60
                :initarg desired-fps
                :type fixnum
                :accessor desired-fps)
   (wire-frame :initform nil
               :initarg :wire-frame
               :type t
               :accessor wire-frame-p)
   (cull-face :initform t
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

(defmethod initialize ((viewer viewer) &key)
  (with-slots (objects view-xform) viewer
    (loop
      for object in objects
      do
         (initialize object)
         (set-uniform object "view_transform" view-xform :mat4))))


(defmethod cleanup ((viewer viewer))
  (loop
    for object in (objects viewer)
    do
       (cleanup object)))

(defgeneric update-view-xform (object elapsed-seconds)
  (:documentation "Upate view-xform uniform if it has changed.")
  )
;; (defparameter *ronce* nil)
#+spacenav
(defmethod handle-3d-mouse-event ((viewer viewer) (event sn:motion-event))
  (with-slots (aspect-ratio view-xform camera-position camera-up camera-direction view-changed) viewer
    (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
      (format t "~a ~a ~a    ~a ~a ~a~%" sn:x sn:y sn:z  sn:rx sn:ry sn:rz)
      (let* ((linear-scale (/ 1.0 5000))
             (radial-scale (/  1.0 1000.0))
             (up-mat (meye 4))
             (dir-mat (meye 4)))

        (when (not (zerop sn:ry))
          (format t "~a~%" (mrotation (vunit (vxyz camera-up))
                                  (* 1.0 radial-scale sn:ry)))
          (nm* dir-mat
               (mtranslation (vxyz camera-position))
               (mrotation (vunit (vxyz camera-up))
                          (* 1.0 radial-scale sn:ry))
               (mtranslation (v- (vxyz camera-position)))))
        
        ;; (when (and (null *ronce*) (not (zerop sn:rx)))
        ;;   (setf *ronce* t)
        ;;   (format t "rotating around direction: ~a~%" (mrotation (vxyz camera-direction)
        ;;                          (* radial-scale sn:rx)))
        ;;   (nm* up-mat (mrotation (vxyz camera-direction)
        ;;                          (* radial-scale sn:rx))))
        ;; (when (not (zerop sn:rz))
        ;;   (let ((orth (vunit (vc (vxyz camera-direction)
        ;;                          (vxyz camera-up)))))
        ;;     (format t "Rotating ~a around ~a~%"
        ;;             (* radial-scale sn:rz)
        ;;             orth)

        ;;     (nm* up-mat
        ;;          (mrotation orth (* radial-scale sn:rz)))
        ;;     (nm* dir-mat
        ;;          (mrotation orth (* radial-scale sn:rz)))))

        (setf camera-direction
               (m*
                dir-mat
                camera-direction))
        (setf camera-up
               (m*
                up-mat
                camera-up))

        (setf camera-position
              (v+ camera-position
                  (vxyz_ (v* (* linear-scale sn:z)
                             camera-direction))
                  (vxyz_ (v* (* linear-scale sn:y)
                              camera-up))
                  (vxyz_ (v* (* linear-scale sn:x)
                             (vunit (vc (vxyz camera-direction)
                                        (vxyz camera-up)))))))
        (format t "~a ~a ~a~%" camera-position camera-direction camera-up)
        ;; up/down -> translate along 'up'
        ;; forward/back -> translate along 'direction'
        ;; left/right -> translate orthogonal to camera-direction and +up+
        ;; vcross
        (setf view-changed t)
        (setf view-xform
              (m* (mperspective 60.0 aspect-ratio 0.1 1000.0)
                  (mlookat (vxyz camera-position)
                           (vxyz (v+ camera-position camera-direction))
                           (vxyz camera-up)
                           )))))))

(defmethod handle-key ((viewer viewer) window key scancode action mod-keys)
  (cond
    ;; ESC to exit
    ((and (eq key :escape) (eq action :press))
     (glfw:set-window-should-close window)
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
     (initialize viewer)
     t)

    ;; i to show gl info
    ((and (eq key :i) (eq action :press))
     (show-info viewer)
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
    ((and (eq key :f5) (eq action :press))
     (reset-view viewer)
     (with-slots (view-xform objects) viewer
       (loop
         for object in objects
         do
            (set-uniform object "view_transform" view-xform :mat4)))
     t)

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


(defmethod update-view-xform ((viewer viewer) elapsed-seconds)
  (declare (ignorable viewer elapsed-seconds))
  nil)

(defmethod update ((viewer viewer) elapsed-seconds)

  (with-slots (objects view-xform view-changed) viewer
    (let ((changed (if view-changed t
                       (update-view-xform viewer elapsed-seconds))))

        (loop
          for object in objects
          do
             (when changed
               (set-uniform object "view_transform" view-xform :mat4))
             (update object elapsed-seconds)))
      (setf view-changed nil)))

(defmethod render ((viewer viewer))
  (with-slots (objects view-xform) viewer
    (loop
      for object in objects
      do
         (render object))))

(defgeneric display-in (object viewer)
  (:documentation "Display object in a viewer."))

(defmethod display-in ((object t) (viewer viewer))
  "High level function to display an object or viewer."

  ;; "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
  ;;  and runs an event loop."
  ;; (glfw:set-error-callback 'error-callback)
  (Init)

  (with-slots (objects) viewer
    (setf objects (ensure-list object)))

  (let* ((window (glfw:create-window :title "OpenGL Viewer"
                                     :width 1000
                                     :height 1000
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
           #+spacenav(sn:sn-open)
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
                      )
           (gl:depth-func :less
                          )

           ;; The event loop
           (with-slots (previous-seconds show-fps desired-fps
                        cull-face front-face wire-frame view-xform background-color)
               viewer

             (gl:clear-color (vx background-color)
                             (vy background-color)
                             (vz background-color)
                             (vw background-color))

             ;; Load objects for the first time
             (initialize viewer)
             #+spacenav(sn:sensitivity 0.5d0)
             (loop
               with start-time = (glfw:get-time)
               for frame-count from 0
               until (glfw:window-should-close-p window)

               for current-seconds = (glfw:get-time)
               for elapsed-seconds = (- current-seconds previous-seconds)
               for elapsed-time = (- current-seconds start-time)

               when (and show-fps (> elapsed-seconds 0.25))
                 do
                    (format t "~,3f fps~%" (/ frame-count elapsed-seconds))
                    (setf previous-seconds current-seconds)
                    (setf frame-count 0)

                    ;; This do is important...
               do (progn
                    (glfw:swap-buffers window)
                    #+spacenav
                    (when-let (ev (sn:poll-event))
                      #+spacenav(sn:remove-events :motion)
                      (handle-3d-mouse-event viewer ev)))
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


                  (render viewer)

               do (glfw:poll-events)
               do (let* ((now (glfw:get-time))
                         (rem-time (- (+ current-seconds (/ 1.0 desired-fps))
                                      now)))
                    ;; (format t "Start: ~a now ~a sleep ~a~%" current-seconds Now rem-time)
                    (when (> rem-time 0)
                      (sleep rem-time))
                    ))


             ;; Cleanup before exit
             (cleanup viewer)
             (rm-viewer window)))
      #+spacenav(sn:sn-close)
      (glfw:destroy-window window))))

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

(defmethod show-info ((viewer viewer) &key (indent 0))
  (let ((this-ws (indent-whitespace indent)))
    (show-slots this-ws viewer  '(objects view-xform aspect-ratio show-fps desired-fps
                                  wire-frame cull-face front-face background-color
                                  window previous-seconds frame-count))
    (with-slots (objects) viewer
      (dolist (object objects)
        (show-info object :indent (1+ indent))))))

(defgeneric add-object (viewer object))
(defmethod add-object (viewer object)
  (with-slots (objects) viewer
      (push object objects)))
