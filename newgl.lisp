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


(defparameter *viewers* (make-hash-table :test 'equal))

(defgeneric cleanup (obj)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

(defparameter *want-forward-context*
  #+(or windows linux bsd :freebsd) nil
  #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")

;; Keyboard callback.
;; Implements top-level key handler, and forwards unhandled events to *viewer*
(def-key-callback keyboard-handler (window key scancode action mod-keys)
  (when-let (viewer  (gethash (cffi:pointer-address window) *viewers*))
    (handle-key viewer window key scancode action mod-keys)))

;; Mouse handler callback
;; Forwards mouse events to *viewer*
(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (let* ((viewer (gethash (cffi:pointer-address window) *viewers*))
         (cpos (glfw:get-cursor-position window))
         (click-info (make-instance 'mouse-click
                                    :cursor-pos cpos
                                    :mod-keys mod-keys
                                    :action action
                                    :button button
                                    :time (get-time))))
    ;; If no objects handle mouse clicks then save the click and release positions for later.
    ;; TODO: Update gl-fractals/complex-fractal.lisp to save clicks to complex-fractal member variable
    ;; and get rid of *mouse-release-info* and *mouse-press-info*
    (when viewer
      (handle-click viewer window click-info))))

;; GLFW scroll handler
;; Forwards scroll events to *viewer*
(def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (when-let  (viewer (gethash (cffi:pointer-address window) *viewers*))
    (let ((cpos (glfw:get-cursor-position window)))
      (handle-scroll viewer window cpos x-scroll y-scroll))))

;; GLFW error callback
(def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

;; Resize event handler
;; Forwards events to *viewer*
(def-framebuffer-size-callback resize-handler (window width height)
  (when-let  (viewer (gethash (cffi:pointer-address window) *viewers*))
    (handle-resize viewer window width height)))
  


(defgeneric display (object &key
                              view-transform
                              background-color
                              debug)
  (:documentation "Display an object."))

(defmethod display ((object t) &key
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
    (if (not debug)
        (trivial-main-thread:with-body-in-main-thread ()
            (display viewer
                     :view-transform view-transform
                     :background-color background-color
                     :debug debug))
        (display viewer
                 :view-transform view-transform
                 :background-color background-color
                 :debug debug))))

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
