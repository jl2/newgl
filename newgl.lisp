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

(defun init ()
  (glfw:initialize))

(defun terminate ()
  (glfw:terminate))

(defgeneric display (object)
  (:documentation "Display an object."))

(defgeneric cleanup (obj)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

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
