;; keyframe-sequence.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass keyframe ()
  ((value :initarg :value :accessor keyframe-value)
   (start-time :initarg :start-time :accessor start-time)
   (interperolator :initarg :interperolator :initform #'vlerp)))

(defclass keyframe-sequence ()
  ((frames :initarg :frames)
   (before-behavior :initarg :before :initform :stop)
   (after-behavior :initarg :after :initform :stop)))

(defgeneric value-at (sequence time))

(defmethod value-at ((sequence keyframe-sequence) time)
  (with-slots (frames before-behavior end-behavior) sequence
    (let ((last-idx (- (length frames) 1)))
      (cond ((= -1 last-idx)
             ;; (format t "Empty sequence.~%")
             nil)

            ((= 0 last-idx)
             ;; (format t "Single value sequence.~%")
             (keyframe-value (aref frames 0)))

            ((and (<= time (slot-value (aref frames 0) 'start-time))
                  (eq before-behavior :stop) )
             ;; (format t "Before beginning of sequence.~%")
             (keyframe-value (aref frames 0)))

            ((and (>= time (slot-value (aref frames last-idx) 'start-time))
                  (eq before-behavior :stop))
             ;; (format t "After end of sequence.~%")
             (keyframe-value (aref frames last-idx)))

            (t
             (let* ((first-frame-idx (position time
                                               frames :test #'>=
                                               :key #'start-time
                                               :from-end t))
                    (second-frame-idx (1+ first-frame-idx))
                    (first-frame (aref frames first-frame-idx))
                    (second-frame (aref frames second-frame-idx)))
               ;; (format t "index for ~a is ~a~%" time first-frame-idx)
               ;; (format t "first-frame starts at: ~a~%" (start-time first-frame))
               ;; (format t "second-frame starts at: ~a~%" (start-time second-frame))
               (with-slots (interperolator) first-frame
                 (funcall interperolator
                          (keyframe-value first-frame)
                          (keyframe-value second-frame)
                          (/ (- time (start-time first-frame))
                             (- (start-time second-frame) (start-time first-frame)))))))))))

(defgeneric keyframe-count (sequence))
(defmethod keyframe-count ((sequence keyframe-sequence))
  (with-slots (frames) sequence
    (length frames)))

(defun create-keyframe (value time)
  (make-instance 'keyframe :value value :start-time time))

(defun create-keyframe-sequence (frames &key (before :stop) (after :stop))
  (declare (type list frames))
  (make-instance 'keyframe-sequence
                 :frames (make-array (length frames)
                                     :element-type 'keyframe
                                     :initial-contents frames
                                     :adjustable t)
                 :before before
                 :after after))

(defun create-simple-keyframe-sequence (points &optional (time-scale 1.0))
  (make-instance 'keyframe-sequence
                 :frames (make-array (length points)
                                     :element-type 'keyframe
                                     :initial-contents (loop for pt in points
                                                             for time = 0.0 then (1+ time)
                                                             collecting
                                                             (make-instance 'keyframe :value pt
                                                                            :start-time (* time-scale time)))
                                     :adjustable t)
                 :before :stop
                 :after :stop))
