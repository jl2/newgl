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
   (free :initform nil :initarg :free)
   (needs-refill :initform t)))

(defclass attribute-buffer (buffer)
  ((stride :initform nil)
   (target :initform :array-buffer :initarg :target)
   (attributes :initform '(("in_position" . :vec3) ("in_color" . :vec4))
               :initarg :attributes)))

(defclass index-buffer (buffer)
  ((target :initform :element-array-buffer)
   (stride :initform 1)))

(defclass uniform-buffer (buffer)
  ((block-index :initform 0 :initarg :block-index)
   (block-name :initarg :block-name)
   (bind-location :initform 0 :initarg :bind-location)))


(defclass instance-buffer (attribute-buffer)
  ((attributes :initform '(("obj_transform" . :mat4))
               :initarg :attributes)))

(defgeneric compute-stride (buffer))

(defgeneric associate-attributes (buffer program))

;; (defgeneric bset (buffer index &rest values)
;;   (:documentation "Set buffer value  at index to values."))



(defmethod bind ((buffer buffer))
  (with-slots (bo target usage free pointer) buffer
    (when (= bo 0)
      (when (null pointer)
        (error "Cannot fill buffer from nil."))
      (setf bo (car (gl:gen-buffers 1)))
      (gl:bind-buffer target bo)
      (gl:buffer-data target usage pointer)
      (when (and free pointer)
        (free-gl-array pointer)
        (setf pointer nil)))))

(defmethod show-info ((object buffer) &key (indent 0))
  (let ((this-ws (indent-whitespace indent))
        (next-ws (indent-whitespace (1+ indent))))
    (format t "~a~a~%" this-ws object)
    (show-slots next-ws object '(bo count pointer target usage stride free))))

(defmethod show-info ((object attribute-buffer) &key (indent 0))
  (call-next-method)
  (let ((next-ws (indent-whitespace (1+ indent))))
    (show-slots next-ws object '(attributes))))

(defmethod show-info ((object attribute-buffer) &key (indent 0))
  (call-next-method)
  (let ((next-ws (indent-whitespace (1+ indent))))
    (show-slots next-ws object '(attributes))))

(defmethod cleanup ((buffer buffer))
  (with-slots (bo target free pointer) buffer
    (gl:bind-buffer target 0)
    (gl:delete-buffers (list bo))
    (setf bo nil)
    (when (and free pointer)
      (free-gl-array pointer))
    (setf pointer nil)))



(defmethod compute-stride ((buffer buffer))
  (with-slots (stride) buffer
    (when (null stride)
        (setf stride 1))
    stride))

(defmethod compute-stride ((buffer attribute-buffer))
  (with-slots (stride attributes) buffer
    (when (null stride)
      (setf stride
            (loop for (nil . type) in attributes
                  summing (glsl-byte-size type))))
    stride))


(defmethod associate-attributes ((buffer buffer) program)
  (declare (ignorable buffer program))
  t)


(defmethod associate-attributes ((buffer attribute-buffer) program)
  (with-slots (attributes) buffer
    (loop
          with stride = (compute-stride buffer)
          for offset = 0 then (+ offset byte-size)
          for (name . type) in attributes
          for (comp-type comp-count byte-size vec4-size) = (glsl-type-info type)
          do
             (let ((entry-attrib (gl:get-attrib-location program name)))
               (when (>= entry-attrib 0)
                 (loop for i below vec4-size
                       for attrib-idx = (+ entry-attrib i)
                       do
                          (gl:vertex-attrib-pointer attrib-idx
                                                    4
                                                    comp-type
                                                    :false
                                                    stride
                                                    (+ offset (* 4 4 i)))
                          (gl:enable-vertex-attrib-array attrib-idx)
                       )))))
  t)

(defmethod associate-attributes ((buffer instance-buffer) program)
  (with-slots (attributes) buffer
    (loop
          with stride = (compute-stride buffer)
          for offset = 0 then (+ offset byte-size)
          for (name . type) in attributes
          for (comp-type comp-count byte-size vec4-size) = (glsl-type-info type)
          do
             (let ((entry-attrib (gl:get-attrib-location program name)))
               (when (>= entry-attrib 0)
                 (loop for i below vec4-size
                       for attrib-idx = (+ entry-attrib i)
                       do
                          (gl:vertex-attrib-pointer attrib-idx
                                                    (min comp-count 4)
                                                    comp-type
                                                    :false
                                                    stride
                                                    (+ offset (* 4 4 i)))
                          (gl:enable-vertex-attrib-array attrib-idx)
                          (gl:vertex-attrib-divisor attrib-idx 1)
                       )))))
  t)

(defmethod reload ((buffer buffer))
  (bind buffer)
  (with-slots (pointer target) buffer
    (when (null pointer)
      (error "Cannot refill buffer from nil."))
    (gl:buffer-sub-data target pointer)))


(declaim (inline allocate-gl-array free-gl-array gl-set gl-get to-gl-float-array to-gl-array fill-buffer))
(defun allocate-gl-array (type count)
  (gl:alloc-gl-array type count))

(defun free-gl-array (array)
  (gl:free-gl-array array))

(defun gl-set (array idx value type)
  (setf (gl:glaref array idx)
        (coerce value type)))

(defun gl-get (array idx)
  (gl:glaref array idx))

(defun to-gl-array (gl-type size arr)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3)))
  (let* ((gl-array (allocate-gl-array gl-type size)))
    (fill-buffer arr gl-array 0)
    gl-array))

(defgeneric fill-buffer (data ptr offset))
(defmethod fill-buffer ((data vec3) ptr offset)
  (gl-set ptr (+ 0 offset) (vx data) 'single-float)
  (gl-set ptr (+ 1 offset) (vy data) 'single-float)
  (+ 2 offset))

(defmethod fill-buffer ((data vector) ptr offset)
  (loop for obj across data
        for off = offset then next-off
        for next-off = (fill-buffer obj ptr off)
        finally (return next-off)))

(defmethod fill-buffer ((data vec3) ptr offset)
  (gl-set ptr (+ 0 offset) (vx data) 'single-float)
  (gl-set ptr (+ 1 offset) (vy data) 'single-float)
  (gl-set ptr (+ 2 offset) (vz data) 'single-float)
  (+ 3 offset))

(defmethod fill-buffer ((data vec4) ptr offset)
  (gl-set ptr (+ 0 offset) (vx data) 'single-float)
  (gl-set ptr (+ 1 offset) (vy data) 'single-float)
  (gl-set ptr (+ 2 offset) (vz data) 'single-float)
  (gl-set ptr (+ 3 offset) (vw data) 'single-float)
  (+ 4 offset))

(defmethod fill-buffer ((data mat3) ptr offset)
  (loop for off from 0
        for d across (marr (mtranspose data))
        do
           (gl-set ptr (+ off offset) d 'single-float)
        finally (return (+ off offset))))

(defmethod fill-buffer ((data mat4) ptr offset)
  (loop for off from 0
        for d across (marr (mtranspose data))
        do
           (gl-set ptr (+ off offset) d 'single-float)
        finally (return (+ off offset))))

(defmethod fill-buffer ((data list) ptr offset)
  (loop for obj in data
        for off = offset then next-off
        for next-off = (fill-buffer obj ptr off)
        finally (return next-off)))

(defmethod fill-buffer ((data integer) ptr offset)
  (gl-set ptr offset data 'fixnum)
  (1+ offset))

(defmethod fill-buffer ((data real) ptr offset)
  (gl-set ptr offset data 'single-float)
  (1+ offset))



