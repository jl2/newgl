;; layouts.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass layout-entry ()
  ((name :initarg :name :type string :accessor layout-entry-name)
   (count :initarg :count :type fixnum :accessor layout-entry-count)
   (type :initarg :type :accessor layout-entry-type)))


(defclass layout ()
  ((entries :initarg :entries :type (or list null) :accessor layout-entries)
   (descriptor :initform nil :accessor descriptor))
  (:documentation "A shader layout."))

(defclass instanced-layout (layout)
  ()
  (:documentation "A shader layout that supports instance data in the last entry."))

(defgeneric compatible (first second)
  (:documentation "Check if two layouts are compatible. (i.e. same types and number of entries)"))

(defmethod compatible ((first layout-entry) (second layout-entry))
  (and (= (layout-entry-count first) (layout-entry-count second))
       (eq (layout-entry-type first) (layout-entry-type second))))

(defmethod compatible ((first layout) (second layout))
  (when (= (length (layout-entries first)) (length (layout-entries second)))
    (loop for fe in (layout-entries first)
          for se in (layout-entries second)
          when (not (compatible fe se))
          return nil)
    t))

(defclass layout-description ()
  ((emit-position :initform t :initarg :emit-position :accessor emit-position)
   (emit-normal :initform nil :initarg :emit-normal :accessor emit-normal)
   (emit-uv :initform nil :initarg :emit-uv :accessor emit-uv)
   (emit-color :initform nil :initarg :emit-color :accessor emit-color)))

(defun layout-stride (desc)
  (with-slots (emit-position emit-normal emit-uv emit-color) desc
    (+ (if emit-position 3 0)
       (if emit-normal 3 0)
       (if emit-uv 2 0)
       (if emit-color 4 0))))

(defmethod get-layout-descriptor ((layout layout))
  (if (descriptor layout)
      (progn
        (descriptor layout))
      (with-slots (descriptor entries) layout
        (setf descriptor (make-instance 'layout-description))
        (with-slots (emit-position emit-normal emit-uv emit-color) descriptor
          (loop for entry in entries
                do
                   (let ((lower-name (string-downcase (layout-entry-name entry))))
                     (cond ((search "position" lower-name :test #'string=)
                            (setf emit-position t))
                           ((search "normal" lower-name :test #'string=)
                            (setf emit-normal t))
                           ((or (search "uv" lower-name :test #'string=)
                                (search "st" lower-name :test #'string=))
                            (setf emit-uv t))
                           ((search "color" lower-name :test #'string=)
                            (setf emit-color t))))))
        descriptor)))

(defgeneric enable-layout (layout program)
  (:documentation "Bind a layout."))

(defmethod enable-layout ((layout null) program)
  (declare (ignorable layout program)))



(defun make-layout-entry (name count type)
  (make-instance 'layout-entry
                 :name name
                 :count count
                 :type type))

(defmethod print-object ((obj layout-entry) stream)
  (with-slots (name count type) obj
    (format stream "(make-layout-entry ~a ~a ~a)" name count type)))

(defun make-layout (entries)
  (make-instance 'layout :entries entries))

(defun compute-stride (layout)
  (with-slots (entries) layout
    (loop for entry in entries
       summing (* (slot-value entry 'count)
                  (cffi:foreign-type-size (slot-value entry 'type))))))

(defmethod enable-layout ((layout layout) program)
  (with-slots (entries) layout
    (loop
       with stride = (compute-stride layout)
       for offset = 0 then (+ offset (* (cffi:foreign-type-size (slot-value entry 'type))
                                        (slot-value entry 'count)))
       for entry in entries
       for idx from 0
          do
          (with-slots (count type name) entry
            (let ((entry-attrib (gl:get-attrib-location program name)))
              (when (>= entry-attrib 0)
                (gl:enable-vertex-attrib-array entry-attrib)
                (gl:vertex-attrib-pointer idx
                                          count
                                          type
                                          :false
                                          stride
                                          offset)))))))
