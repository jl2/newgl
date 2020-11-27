;; gl-shader.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

;; This library depends on a number of conventions to automatically handle shaders.
;; 1. Shader file names must specify the shader type as part of the file name.
;;    For example, 'plastic-fragment.glsl', 'plastic-vertex.glsl'
;; 2. Uniforms and layout information is 'parsed' out of the shader using regular expressions.
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
  ((layout :initarg :layout :initform nil :type (or null layout) :accessor layout)
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
  ((source-file :initarg :source-file :type (or pathname string) :accessor source-file))
  (:documentation "An OpenGL shader whose source code is stored in a file."))

(defmethod print-object ((shader gl-shader) stream)
  (format stream "(make-instance 'newgl:gl-file-shader :source-file ~s )" (source-file shader)))

(defun glsl-type-keyword (name)
  (intern (string-upcase name) "KEYWORD"))


(defun glsl-type-size (tname)
  (when-let ((val (assoc tname
                         '(("vec3" :float 3)
                           ("vec4" :float 4)
                           ("vec2" :float 2)
                           ("dvec3" :float 3)
                           ("dvec4" :float 4)
                           ("dvec2" :float 2)

                           ("mat4" float 16)
                           ("mat3" :float 9)
                           ("dmat4" float 16)
                           ("dmat3" :float 9)

                           ("double" double 1)
                           ("float" :float 1)
                           ("int" s :int 1))
                         :test #'string=)))
    (values (cadr val) (caddr val))))
                  

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
         (uniform-rx "uniform\\s+(\\w+)\\s+(\\w+);"))

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
      (setf (gethash name uniforms)
            (make-instance 'uniform
                           :name name
                           :type (glsl-type-keyword type))))
    (setf (slot-value shader 'uniforms) uniforms)
    shader))

(defmethod use-shader-layout ((shader gl-shader) program )
  (with-slots (layout) shader
    (enable-layout layout program)))

(defmethod use-shader-uniforms ((shader gl-shader) program )
  (with-slots (uniforms) shader
    (loop for uniform being the hash-values of uniforms do
      (use-uniform uniform program))))

(defmethod get-source ((shader gl-shader))
  "")

(defmethod get-source ((shader gl-file-shader))
  (with-slots (source-file) shader
    (ju:read-file source-file)))

(defmethod set-uniform ((shader gl-shader) name new-value)
  (with-slots (uniforms shader shader-type) shader
    (when (gethash name uniforms)
      (set-value (gethash name uniforms) new-value))))

(defmethod cleanup ((shader gl-shader))
  (with-slots (shader uniforms) shader
    (when (> 0 shader)
      (gl:delete-shader shader))
    ;; (setf uniforms (make-hash-table :test 'equal))
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
