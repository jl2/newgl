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

(defparameter *rebuild-shaders* nil)
(defparameter *refill-buffers* nil)

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (format t "Keypress: ~a ~a ~a ~a ~a~%" window key scancode action mod-keys)
  (cond ((and (eq key :escape) (eq action :press))
         (set-window-should-close))
        ((and (eq key :r) (eq action :press))
         (setf *rebuild-shaders* t))
        ((and (eq key :f) (eq action :press))
         (setf *refill-buffers* t))))

(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (declare (ignorable window button action mod-keys))
  (let ((cpos (glfw:get-cursor-position window)))
    (format t "Mouse click at ~a ~a ~a ~a ~a~%" cpos window button action mod-keys)))

(def-scroll-callback scroll-handler (window x y)
  (let ((cpos (glfw:get-cursor-position window)))
    (format t "Scroll at ~a ~a ~a ~a ~%" cpos window x y)))

(def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

(defun viewer-thread-function ( object )
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height)))
      (with-window (:title "OpenGL Scene Viewer"
                           :width (/ cur-width 2)
                           :height (/ cur-height 2)
                           :decorated t
                           ;; :monitor monitor
                           :opengl-profile :opengl-core-profile
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-forward-compat t
                           :resizable t)
        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'quit-on-escape)
        (set-error-callback 'error-callback)
        (set-mouse-button-callback 'mouse-handler)
        (set-scroll-callback 'scroll-handler)
        (gl:enable :line-smooth
                   :polygon-smooth
                   :depth-test)
        (gl:depth-func :less)

        (gl:clear-color 0.7f0 0.7f0 0.7f0 1.0)
        (rebuild-shaders object)
        (fill-buffers object)
        ;; The event loop
        (loop
           until (window-should-close-p)

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
             (render object)
           do (swap-buffers)
           do (poll-events))))))

(defun show (object &optional (in-thread nil))
  (if in-thread
      (viewer-thread-function object)
      (trivial-main-thread:with-body-in-main-thread ()
        (viewer-thread-function object))))

(defun hello ()
  (let ((tri (make-instance 'primitives )))
    (add-filled-triangle tri
                         (vec3 0.0 0.5 0.0)
                         (vec3 0.5 -0.5 0.0)
                         (vec3 -0.5 -0.5 0.0)
                         (vec4 0.0 1.0 0.0 1.0))
    (show tri)))
