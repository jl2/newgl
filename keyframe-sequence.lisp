;; keyframe-sequence.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl)

(defclass keyframe ()
  ((value :initarg :value :accessor keyframe-value)
   (start-time :initarg :start-time :accessor start-time)
   (interperolator :initarg :interperolator :initform #'vlerp)))

(defclass keyframe-sequence ()
  ((frames :initarg :frames)
   (before-behavior :initarg :before :initform :clamp)
   (after-behavior :initarg :after :initform :clamp)))

(defgeneric value-at (sequence time))

(defun compute-logical-time (real-time start-time start-behavior end-time end-behavior)
  (cond

    ;; In range
    ((and (>= real-time start-time) (<= real-time end-time))
     real-time)

    ;; Below and clamping
    ((and (<= real-time start-time) (eq start-behavior :clamp))
     start-time)

    ;; Above and clamping
    ((and (>= real-time end-time) (eq end-behavior :clamp))
     end-time)

    ;; Repeating on the end
    ((and (> real-time end-time) (eq end-behavior :repeat))
     (+ (mod (- real-time start-time) (- end-time start-time)) start-time))

    ((and (< real-time start-time) (eq start-behavior :repeat))
     (+ (mod (- real-time start-time) (- end-time start-time)) start-time))
    (t
     (error "Unhandled case in compute-logical-time ~a ~a ~a ~a ~a"
            real-time start-time end-time start-behavior end-behavior)
     )
  ))
(defmethod value-at ((sequence keyframe-sequence) time)
  (with-slots (frames before-behavior after-behavior) sequence
    (let ((last-idx (- (length frames) 1)))
      (cond
        ;; value-at of an empty sequence is an error
        ((= -1 last-idx)
         (error "Trying to get value-at of empty sequence.")
         nil)

        ;; Single value sequence is always that value
        ((= 0 last-idx)
         ;; (format t "Single value sequence.~%")
         (keyframe-value (aref frames 0)))

        ((and (<= time (slot-value (aref frames 0) 'start-time))
              (eq before-behavior :clamp) )
         ;; (format t "Before beginning of sequence.~%")
         (keyframe-value (aref frames 0)))
        ((and (>= time (slot-value (aref frames last-idx) 'start-time))
              (eq before-behavior :clamp))
         ;; (format t "After end of sequence.~%")
         (keyframe-value (aref frames last-idx)))

        (t
         ;; (format t "Computing using compute-logical-time~%")
         (let* ((logical-time (compute-logical-time
                                            time
                                            (slot-value (aref frames 0) 'start-time)
                                            before-behavior
                                            (slot-value (aref frames last-idx) 'start-time)
                                            after-behavior))
                (first-frame-idx (position logical-time
                                           frames :test #'>=
                                           :key #'start-time
                                           :from-end t))
                (second-frame-idx (1+ first-frame-idx))
                (first-frame (aref frames first-frame-idx))
                (second-frame (aref frames second-frame-idx)))
           (with-slots (interperolator) first-frame
             (funcall interperolator
                      (keyframe-value first-frame)
                      (keyframe-value second-frame)
                      (/ (- logical-time (start-time first-frame))
                         (- (start-time second-frame) (start-time first-frame)))))))))))

(defmethod value-at ((obj t) time)
  (declare (ignorable time))
  obj)

(defgeneric keyframe-count (sequence))

(defmethod keyframe-count ((sequence keyframe-sequence))
  (with-slots (frames) sequence
    (length frames)))

(defun create-keyframe (value time)
  (make-instance 'keyframe :value value :start-time time))

(defun create-keyframe-sequence (frames &key (before :clamp) (after :clamp))
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
                 :before :clamp
                 :after :clamp))
