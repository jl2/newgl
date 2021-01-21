;; buffer.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass buffer ()
  ((bo :initform 0 :initarg :bo)
   (count :initarg :count)
   (pointer :initarg :pointer)
   (target :initform :array-buffer :initarg :target)
   (usage :initform :static-draw :initarg :usage)
   (stride :initform nil :initarg :stride)
   (free :initform nil :initarg :free)))

(defmethod initialize ((buffer buffer) &key)
  (with-slots (bo target usage free pointer) buffer
    (when (null pointer)
      (error "Cannot fill buffer from nil."))
    (setf bo (car (gl:gen-buffers 1)))

    (gl:bind-buffer target bo)
    (gl:buffer-data target usage pointer)
    (when (and free pointer)
      (free-gl-array pointer)
      (setf pointer nil))))

(defmethod show-info ((object buffer) &key (indent 0))
  (let ((this-ws (indent-whitespace indent))
        (next-ws (indent-whitespace (1+ indent))))
    (format t "~a~a~%" this-ws object)
    (dolist (slot '(bo count pointer target usage stride free))
      (format t "~a~a : ~a~%" next-ws slot (slot-value object slot)))
    ))

(defmethod cleanup ((buffer buffer))
  (with-slots (bo target free pointer) buffer
    (gl:bind-buffer target 0)
    (gl:delete-buffers (list bo))
    (setf bo nil)
    (when (and free pointer)
      (free-gl-array pointer))
    (setf pointer nil)))

(defclass attribute-buffer (buffer)
  (
   (stride :initform nil)
   (target :initform :array-buffer :initarg :target)
   (attributes :initform '(("in_position" . :vec3) ("in_color" . :vec4))
               :initarg :attributes)))

(defclass index-buffer (buffer)
  ((target :initform :element-array-bufer)
   (stride :initform 1)))

(defclass uniform-buffer (buffer)
  ((block-index :initform 0 :initarg :block-index)
   (block-name :initarg :block-name)
   (program :initarg :program)
   (bind-location :initform 0 :initarg :bind-location)))

;; Use (bound-idx (gl:get-uniform-block-index program block-name))
;; (gl:uniform-block-binding program bound-idx bound-location)
;; (gl:bind-buffer-base :uniform-buffer bound-location bo)

(defmethod bind ((buffer buffer))
  (with-slots (bo target) buffer
    (if bo
        (gl:bind-buffer target bo)
        (error "Binding an uninitialized buffer!"))))


(defgeneric refill (buffer))

(defgeneric compute-stride (buffer))

(defmethod compute-stride ((buffer buffer))
  (with-slots (stride) buffer
    (when (null stride)
        (setf stride 1))
    stride))

(defmethod compute-stride ((buffer attribute-buffer))
  (with-slots (stride attributes) buffer
    (when (null stride)
      (setf stride
            (loop for attrib in attributes summing (glsl-type-size (cdr attrib)))))
    stride))

(defgeneric associate-attributes (buffer program))
(defmethod associate-attributes ((buffer buffer) program)
  (declare (ignorable buffer program)))

(defmethod associate-attributes ((buffer attribute-buffer) program)
  (with-slots (attributes) buffer
  (loop with stride = (compute-stride buffer)
        for offset = 0 then (+ offset (glsl-type-size type))
        for (name . type) in attributes
        for idx from 0
        do
           (let ((entry-attrib (gl:get-attrib-location program name)))
             (when (>= entry-attrib 0)
               (gl:enable-vertex-attrib-array entry-attrib)
               (multiple-value-bind (base-type count ) (glsl-type-info type)
                 (gl:vertex-attrib-pointer idx
                                           count
                                           base-type
                                           :false
                                           stride
                                           offset)))))))

(defmethod refill ((buffer buffer))
  (bind buffer)
  (with-slots (pointer target) buffer
    (when (null pointer)
      (error "Cannot refill buffer from nil."))
    (gl:buffer-sub-data target pointer)))


(declaim (inline allocate-gl-array free-gl-array gl-set gl-get to-gl-float-array to-gl-array))
(defun allocate-gl-array (type count)
  (gl:alloc-gl-array type count))

(defun free-gl-array (array)
  (gl:free-gl-array array))

(defun gl-set (array idx value type)
  (setf (gl:glaref array idx)
        (coerce value type)))

(defun gl-get (array idx)
  (gl:glaref array idx))

(defun to-gl-array (gl-type arr)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3))
           ;;(type (vector (or single-float double-float fixnum)) arr)
           (type (simple-array (or single-float double-float fixnum)) arr))
  (let* ((count (length arr))
         (cl-type (assoc-value '((:float . single-float)
                                 (:unsigned-int . fixnum))
                               gl-type))
         (gl-array (allocate-gl-array gl-type count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (coerce (aref arr i) cl-type)))
    gl-array))

