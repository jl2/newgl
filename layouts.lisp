;; layouts.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass layout-entry ()
  ((name :initarg :name :type string :accessor layout-entry-name)
   (count :initarg :count :type fixnum :accessor layout-entry-count)
   (type :initarg :type :accessor layout-entry-type)))


(defclass layout ()
  ((entries :initarg :entries :type (or list null) :accessor layout-entries)
   (descriptor :initform nil :accessor descriptor))
  (:documentation "A shader layout."))


(defgeneric enable-layout (layout program)
  (:documentation "Bind a layout."))

(defmethod enable-layout ((layout null) program)
  (declare (ignorable layout program)))



(defun make-layout-entry (name count type)
  (make-instance 'layout-entry
                 :name name
                 :count count
                 :type type))

;; (defmethod print-object ((obj layout-entry) stream)
;;   (with-slots (name count type) obj
;;     (format stream "(make-layout-entry ~a ~a ~a)" name count type)))

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
