;; fftanim.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:newgl.fftanim)

;; (declaim (optimize (speed 3) (safety 1) (size 1) (debug 1)))

(defclass mp3-fft-animation (newgl:geometry)
  ((mp3-filename :initform nil :initarg :mp3-file)
   (the-mp3 :initform nil)
   (mixer :initform nil)
   (current-stream :initform nil)
   (start-time :initform 0)
   (song-duration :initform 0)
   (window-size :initform 256 :initarg :window-size)
   (window-buffer :initform nil)
   (left-fft-data :initform nil)
   (right-fft-data :initform nil))
  (:documentation "An animation that uses FFT data computed from an MP3 file."))


(defmethod newgl:initialize ((object mp3-fft-animation))
  (call-next-method)
  ;; (gl:enable :line-smooth :polygon-smooth
  ;;            :depth-test :depth-clamp :alpha-test)
  (with-slots (current-stream the-mp3 mixer mp3-filename song-duration start-time window-size window-buffer left-fft-data right-fft-data) object
    (setf window-buffer (make-array window-size
                                              :element-type '(complex double-float)
                                              :adjustable nil
                                              :fill-pointer nil))
    (setf left-fft-data (make-array window-size
                                    :element-type '(complex double-float)
                                    :adjustable nil
                                    :fill-pointer nil))
    (setf right-fft-data (make-array window-size
                                     :element-type '(complex double-float)
                                     :adjustable nil
                                     :fill-pointer nil))
    (let* ((new-mp3-file (read-mp3-file mp3-filename))
           (sduration (mp3-file-duration-in-seconds new-mp3-file)))
      (when mixer
        (when current-stream
          (mixalot:mixer-remove-streamer mixer current-stream))
        (mixalot:destroy-mixer mixer))
      (setf mixer (mixalot:create-mixer))
      (setf current-stream (mixalot-mp3:make-mp3-streamer mp3-filename))
      (setf the-mp3 new-mp3-file)
      (setf song-duration sduration)
      (setf start-time (get-internal-real-time))
      (mixalot:mixer-add-streamer mixer current-stream)
      )))

(defmethod newgl:update ((object mp3-fft-animation) current-time)
  (call-next-method)
  (with-slots (start-time window-buffer song-duration window-size the-mp3 left-fft-data right-fft-data) object
    (let* ((location (/ (+ 1 (- (get-internal-real-time) start-time)) 1.0 internal-time-units-per-second))
           (win-center (ceiling (max 0
                                     (- (* 44100 location)
                                        (round (/ window-size 2)))))))

        (when (and the-mp3 (< (/ (- (get-internal-real-time) start-time) 1.0 internal-time-units-per-second) song-duration))

          (bordeaux-fft:windowed-fft! (mp3-file-left-channel the-mp3)
                                      window-buffer left-fft-data
                                      win-center window-size 'bordeaux-fft:triangle)
          (bordeaux-fft:windowed-fft! (mp3-file-right-channel the-mp3)
                                      window-buffer right-fft-data
                                      win-center window-size 'bordeaux-fft:triangle)))))

(defmethod newgl:cleanup ((object mp3-fft-animation))
  (with-slots (current-stream the-mp3 mixer mp3-filename song-duration start-time left-fft-data right-fft-data window-buffer) object
    (when current-stream
      (mixalot:mixer-remove-streamer mixer current-stream))
    (when mixer
      (mixalot:destroy-mixer mixer))
    (setf mixer nil)
    (setf current-stream nil)
    (setf the-mp3 nil)
    (setf window-buffer nil)
    (setf left-fft-data nil)
    (setf right-fft-data nil)
    ))
