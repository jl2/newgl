;; gl-shader.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

;; This library depends on a number of conventions to automatically handle shaders.
;; 1. Shader file names must specify the shader type as part of the file name.
;;    For example, 'plastic-fragment.glsl', 'plastic-vertex.glsl'
;; 2. Layout information is 'parsed' out of the shader using regular expressions.
;;    They work for the shader's I write, but may need improvements or replacements.

(defparameter *shader-dir* (asdf:system-relative-pathname :newgl "shaders/")
  "Directory containing newgl shaders.")

(defun newgl-shader (fname)
  (merge-pathnames *shader-dir* fname))

;; Shader
(define-condition shader-error (error)
  ((status :initarg :status :reader shader-compile-status)
   (object :initarg :object :reader shader-object)
   (info-log :initarg :info-log :reader shader-compile-info-log)))

(defmethod print-object ((shader-error shader-error) stream)
  (with-slots (status object info-log) shader-error
    (format stream "OpenGL Compiler Error: ~a~%Info log:~%==========~%~a" status info-log)))

(defclass gl-shader ()
  ((shader :initform 0 :type fixnum)
   (shader-type :initarg :shader-type))
  (:documentation "An opengl shader class."))


(defgeneric get-source (shader)
  (:documentation "Return shader source code as a string."))

(defgeneric compile-shader (shader)
  (:documentation "Read source from source file and compile shader"))

(defclass gl-file-shader (gl-shader)
  ((source-file :initarg :source-file :type (or pathname string) :accessor source-file))
  (:documentation "An OpenGL shader whose source code is stored in a file."))

;; (defmethod print-object ((shader gl-shader) stream)
;;   (format stream "(make-instance 'newgl:gl-file-shader :source-file ~s )" (source-file shader)))

(defun glsl-type-keyword (name)
  (intern (string-upcase name) "KEYWORD"))


(defparameter *glsl-type-db*
  '((:vec3 :float 3)
    (:vec4 :float 4)
    (:vec2 :float 2)

    (:mat4 :float 16)
    (:mat3 :float 9)

    (:float :float 1)
    (:double :double 1)
    (:int :int 1)

    (:dvec3 :double 3)
    (:dvec4 :double 4)
    (:dvec2 :double 2)

    (:dmat4 :double 16)
    (:dmat3 :double 9)

    ))


(defun glsl-base-type (tname)
  (when-let ((val (assoc tname
                         *glsl-type-db*)))
    (cadr val)))

(defun glsl-base-count (tname)
  (when-let ((val (assoc tname
                         *glsl-type-db*)))
    (caddr val)))

(defun glsl-type-info (tname)
  (when-let ((val (assoc tname
                         *glsl-type-db*)))
    (values  (cadr val) (caddr val))))

(defun glsl-type-size (tname)
  (multiple-value-bind (type count) (glsl-type-info  tname)
    (* count (cffi:foreign-type-size type))))

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
  "Read a shader language file and parse out basic information, like type and layout"
  (let ((stype (if type type (lookup-shader-type file-name)))
        (real-name (if (uiop:file-exists-p file-name)
                       file-name
                       (newgl-shader file-name))))
    (when (not (uiop:file-exists-p real-name))
      (error "~a ~a do not exist!" file-name real-name))
    (make-instance 'gl-file-shader :source-file real-name :shader-type stype)))

(defmethod get-source ((shader gl-shader))
  "")

(defmethod get-source ((shader gl-file-shader))
  (with-slots (source-file) shader
    (ju:read-file source-file)))

(defmethod initialize ((shader gl-shader) &key)
  (compile-shader shader))

(defmethod cleanup ((shade gl-shader))
  (with-slots (shader) shade
    (when (> shader 0)
      (gl:delete-shader shader))
    (setf shader 0)))

(define-condition shader-compile-error (shader-error) ())

(defmethod compile-shader ((shadr gl-shader))
  (with-slots (shader shader-type) shadr
    (when (zerop shader)
      (setf shader (gl:create-shader shader-type)))
    (gl:shader-source shader (get-source shadr))
    (gl:compile-shader shader)
    (let ((compile-result (gl:get-shader shader :compile-status)))
      (when (not (eq t compile-result))
        (error 'shader-compile-error
               :status compile-result
               :object shadr
               :info-log (gl:get-shader-info-log shader))))))

(defun plastic ()
  (list
   (shader-from-file "position-color-normal-vertex.glsl")
   (shader-from-file "plastic-fragment.glsl")))

(defun painted-plastic ()
  (list
   (shader-from-file "position-normal-uv-vertex.glsl")
   (shader-from-file "textured-plastic-fragment.glsl")))

(defun circled-plastic ()
  (list
   (shader-from-file "position-normal-uv-vertex.glsl")
   (shader-from-file "circle-fragment.glsl")))

(defun point-shader ()
  (list
   (shader-from-file "position-color-vertex.glsl")
   (shader-from-file "point-fragment.glsl")))
