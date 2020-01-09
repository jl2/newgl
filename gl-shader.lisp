;;;; gl-shader.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defparameter *shader-dir* (asdf:system-relative-pathname :newgl "shaders/"))

(defclass gl-shader ()
  ((layout :initarg :layout :initform nil :type (or null layout))
   (uniforms :initform (make-hash-table :test 'equal) :type hash-table)
   (shader :initform 0 :type fixnum)
   (shader-type :initarg :shader-type))
  (:documentation "An opengl shader class."))


(defgeneric get-source (shader)
  (:documentation "Return shader source code as a string."))

(defgeneric compile-shader (shader)
  (:documentation "Read source from source file and compile shader"))

(defgeneric use-shader-layout (shader program)
  (:documentation "Return shader's layout."))

(defgeneric use-shader-uniforms (shader program)
  (:documentation "Return shader's layout."))


(defclass gl-file-shader (gl-shader)
  ((source-file :initarg :source-file :type (or pathname string)))
  (:documentation "An OpenGL shader whose source code is stored in a file."))



(defun glsl-type-keyword (name)
  (intern (string-upcase name) "KEYWORD"))


(defun glsl-type-size (tname)
  (cond
    ((string= tname "vec3")
     (values :float 3))
    ((string= tname "vec4")
     (values :float 4))
    ((string= tname "vec2")
     (values :float 2))
    ((string= tname "float")
     (values :float 1))
    ((string= tname "int")
     (values :int 1))
    ((string= tname "double")
     (values :double 1))))

(defun lookup-shader-type (file-name)
  (let ((pn (pathname-name file-name)))
    (cond ((ends-with-subseq "-vertex" pn)
           :vertex-shader)
          ((ends-with-subseq "-fragment" pn)
           :fragment-shader)
          ((ends-with-subseq "-compute" pn)
           :compute-shader)
          ((ends-with-subseq "-geometry" pn)
           :geometry-shader)
          ((ends-with-subseq "-tess-eval" pn)
           :tess-evaluation-shader)
          ((ends-with-subseq "-tess-control" pn)
           :tess-control-shader))))

;; TODO: This is fragile, to say the least.  Improve as necessary.
(defun shader-from-file (file-name &optional type)
  "Read a shader language file and parse out basic information, like type, layout, uniforms."
  (let* (entries
         (stype (if type type (lookup-shader-type file-name)))
         (uniforms (make-hash-table :test 'equal))
         (shader (make-instance 'gl-file-shader :source-file file-name :shader-type stype))
         (source-code (get-source shader))
         (layout-rx "layout\\(location\\s*=\\s*(\\d+)\\)\\s*(in|out)\\s*(\\w+)\\\s*(\\w+);")
         (uniform-rx "uniform\\s(\\w+)\\s(\\w+);"))

    ;; "Parse" layouts
    (cl-ppcre:do-register-groups (location is-in type name) (layout-rx source-code)
      (declare (ignorable location))
      (when (string= "in" is-in)
        (multiple-value-bind (gtype count) (glsl-type-size type)
          (push (make-layout-entry name count gtype) entries))))

    (setf (slot-value shader 'layout) (make-layout (reverse entries)))

    ;; Look for uniforms
    ;; Kind of lame to scan the file a second time...
    ;; TODO: merge this loop with the previous
    (cl-ppcre:do-register-groups (type name) (uniform-rx source-code)
      (setf (gethash name uniforms) (make-instance 'uniform :name name :type (glsl-type-keyword type))))
    (setf (slot-value shader 'uniforms) uniforms)
    shader))

(defmethod use-shader-layout ((shader gl-shader) program )
  (with-slots (layout) shader
    (enable-layout layout program)))

(defmethod use-shader-uniforms ((shader gl-shader) program )
  (with-slots (uniforms) shader
    (dohash (name uniform uniforms)
      (use-uniform uniform program))))

(defmethod get-source ((shader gl-shader))
  "")

(defmethod get-source ((shader gl-file-shader))
  (with-slots (source-file) shader
    (ju:read-file source-file)))

(defmethod set-uniform ((shader gl-shader) name new-value)
  (with-slots (uniforms) shader
    (let ((old-uni (gethash name uniforms)))
      (when old-uni
        (with-slots (value) old-uni
          (setf value new-value))
        (setf (gethash name uniforms) old-uni)))))

(defmethod cleanup ((shader gl-shader))
  (with-slots (shader) shader
    (when (> 0 shader)
      (gl:delete-shader shader))
    (setf shader 0)))


(defmethod compile-shader ((shadr gl-shader))
  (with-slots (shader shader-type) shadr
    (when (zerop shader)
      (setf shader (gl:create-shader shader-type)))
    (gl:shader-source shader (get-source shadr))
    (gl:compile-shader shader)
    (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
    (format t "info-log ~s~%" (gl:get-shader-info-log shader))))
