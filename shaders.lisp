;;;; shaders.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defparameter *shader-dir* (asdf:system-relative-pathname :newgl "shaders/"))

(defclass gl-shader ()
  ((layout :initarg layout :initform nil :type (or null list))
   (shader :initform 0 :type fixnum)
   (source-file :initform "" :type string)
   (shader-type :initarg :type))
  (:documentation "An opengl shader class."))

(defclass shader-program ()
  ((shaders :initarg :shaders :initform nil :type (or null list))
   (program :initform 0))
  (:documentation "An opengl shader program."))

(defgeneric compile-shader (shader)
  (:documentation "Read source from source file and compile shader"))

(defmethod compile-shader ((shader gl-shader))
  (with-slots (shader source-file type) shader
    (when (zerop shader)
      (setf shader (gl:create-shader type)))
    (gl:shader-source shader (read-file source-file))
    (gl:compile-shader shader)
    (when (not (eq t (gl:get-shader shader :compile-status)))
      (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
      (format t "info-log ~a~%" (gl:get-shader-info-log shader)))))


(defmethod cleanup ((shader gl-shader))
  (with-slots (shader) shader
    (when (> 0 shader)
      (gl:delete-shader shader))
    (setf shader 0)))

;; (defgeneric attach-shader (shader program)
;;   (:documentation "Attach shader from program"))

;; (defgeneric detach-shader (shader program)
;;   (:documentation "Detach shader from program"))

;; (defmethod attach-shader ((shader gl-shader) (program shader-program))
;;   (with-slots (program) program
;;     (with-slots (shader) shader
;;       (gl:attach-shader program shader))))

;; (defmethod detach-shader ((shader gl-shader) (program shader-program))
;;   (with-slots (program) program
;;     (with-slots (shader) shader
;;       (gl:detach-shader program shader))))

  
(defmethod cleanup ((obj shader-program))
  "Delete a shader on the GPU."
  (with-slots (shaders program) obj
    (dolist (shader shaders)
      (cleanup shader))
    (when (> 0 program)
      (gl:delete-program program))
    (setf program 0)))

(defgeneric build-shader-program (program)
  (:documentation "Build a shader program and all of its corresponding programs."))

(defmethod build-shader-program ((program shader-program))
  (with-slots (shaders program) program
    (dolist (shader shaders)
      (with-slots (shader) shader
        (if (zerop shader)
            (compile-shader shader)
            (when (not (zerop program))
              (gl:detach-shader program shader)))
        (compile-shader shader)))
    (when (zerop program)
      (setf program (gl:create-program)))
    (dolist (shader shaders)
      (with-slots (shader) shader
        (gl:attach-shader program shader)))
    (gl:link-program program)
    (let ((status (gl:get-program program :link-status)))
      (when (not (eq status t))
        (format t "link-program: ~a~%~a~%" status (gl:get-program-info-log program))))
    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq status t))
        (format t "validate-program: ~a~%~a~%" status (gl:get-program-info-log program))))))

(defgeneric use-program (shader-program)
  (:documentation "Set uniform and layout variables."))

(defmethod use-program (shader-program))

(defmethod use-program :after ((shader-program shader-program))
  (with-slots (program) shader-program
    (gl:use-program program)))

;; (let* ((float-size   (cffi:foreign-type-size :float))
    ;;        (stride       (* (apply #'+ (mapcar #'cdr layout)) float-size)))
    ;;   (loop
    ;;      for entry in layout
    ;;      for attrib-name = (car entry)
    ;;      for attrib-size = (cdr entry)
    ;;      for position-offset = 0 then (+ position-offset (cdr last-entry))
    ;;      for last-entry = entry
    ;;      for position-attrib = (gl:get-attrib-location program attrib-name)
    ;;      for count from 0
    ;;      do
    ;;        ;; (format t "~a is at ~a~%" attrib-name position-offset)
    ;;        (when (>= position-attrib 0)
    ;;          (gl:enable-vertex-attrib-array position-attrib)
    ;;          (gl:vertex-attrib-pointer position-attrib
    ;;                                    attrib-size
    ;;                                    :float :false
    ;;                                    stride
    ;;                                    (* float-size position-offset)))))



;; (let ((xform-location (gl:get-uniform-location program "transformationMatrix")))
    ;;   (when (> 0 xform-location)
    ;;     (gl:uniform-matrix xform-location
    ;;                        4
    ;;                        (vector (marr4 transformation))
    ;;                        t)))



;; (defclass shader-program ()
;;   ((layout :initarg :inputs)
;;    (vertex-source :initarg :vertex)
;;    (fragment-source :initarg :fragment)
;;    (vertex-shader :initform 0)
;;    (fragment-shader :initform 0)
;;    (program :initform 0)))

;; (defmethod cleanup ((obj shader-program))
;;   "Delete a shader on the GPU."
;;   (with-slots (vertex-shader fragment-shader program) obj
;;     (when (> 0 vertex-shader)
;;       (gl:delete-shader vertex-shader))
;;     (when (> 0 fragment-shader)
;;       (gl:delete-shader vertex-shader))
;;     (when (> 0 program)
;;       (gl:delete-program program))

;;     (setf vertex-shader 0)
;;     (setf fragment-shader 0)
;;     (setf program 0)))

;; (defun compile-shader (shader text)
;;   (gl:shader-source shader text)
;;   (gl:compile-shader shader)
;;   (when (not (eq t (gl:get-shader shader :compile-status)))
;;     (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
;;     (format t "info-log ~a~%" (gl:get-shader-info-log shader))))

;; (defun build-program (program)
;;   (with-slots (vertex-source fragment-source vertex-shader fragment-shader program) program
;;     (if (zerop vertex-shader)
;;         (setf vertex-shader (gl:create-shader :vertex-shader))
;;         (when (not (zerop program))
;;           (gl:detach-shader program vertex-shader)))
;;     (compile-shader vertex-shader (read-file vertex-source))
p
;;     (if (zerop fragment-shader)
;;         (setf fragment-shader (gl:create-shader :fragment-shader))
;;         (when (not (zerop program))
;;           (gl:detach-shader program fragment-shader)))
;;     (compile-shader fragment-shader (read-file fragment-source))

;;     (when (zerop program)
;;       (setf program (gl:create-program)))

;;     (gl:attach-shader program vertex-shader)
;;     (gl:attach-shader program fragment-shader)
;;     (gl:link-program program)

;;     (let ((status (gl:get-program program :link-status)))
;;       (when (not (eq status t))
;;         (format t "link-program: ~a~%~a~%" status (gl:get-program-info-log program))))

;;     (gl:validate-program program)
;;     (let ((status (gl:get-program program :validate-status)))
;;       (when (not (eq status t))
;;         (format t "validate-program: ~a~%~a~%" status (gl:get-program-info-log program))))))

;; ;; https://stackoverflow.com/questions/33917769/usage-of-allow-other-keys-in-common-lisp#33918770

;; (defun use-program (shader-program)
;;   (declare (optimize (speed 3)))
;;   (with-slots (layout program) shader-program
;;     (let* ((float-size   (cffi:foreign-type-size :float))
;;            (stride       (* (apply #'+ (mapcar #'cdr layout)) float-size)))
;;       (loop
;;          for entry in layout
;;          for attrib-name = (car entry)
;;          for attrib-size = (cdr entry)
;;          for position-offset = 0 then (+ position-offset (cdr last-entry))
;;          for last-entry = entry
;;          for position-attrib = (gl:get-attrib-location program attrib-name)
;;          for count from 0
;;          do
;;            ;; (format t "~a is at ~a~%" attrib-name position-offset)
;;            (when (>= position-attrib 0)
;;              (gl:enable-vertex-attrib-array position-attrib)
;;              (gl:vertex-attrib-pointer position-attrib
;;                                        attrib-size
;;                                        :float :false
;;                                        stride
;;                                        (* float-size position-offset)))))

;;     (gl:use-program program)))
;;     ;; (let ((xform-location (gl:get-uniform-location program "transformationMatrix")))
;;     ;;   (when (> 0 xform-location)
;;     ;;     (gl:uniform-matrix xform-location
;;     ;;                        4
;;     ;;                        (vector (marr4 transformation))
;;     ;;                        t)))

