(defpackage :spam
  (:use :cl)
  (:export
   :classify
   :train
   :memory-db
   :*spam-db*))

(in-package :spam)

(defclass memory-db ()
  ((features :initform (make-hash-table :test 'equal)
             :accessor features)
   (total-hams :initform 0
               :accessor total-hams)
   (total-spams :initform 0
                :accessor total-spams)))

(defvar *spam-db*)

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classify (text db)
  "Classify the text of a message as SPAM, HAM, or UNSURE."
  (classification (score (extract-features text db) db)))


(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))

(defun intern-feature (word db)
  (or (gethash word (features db))
      (setf (gethash word (features db))
            (make-instance 'word-feature :word word))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text db)
  (mapcar (lambda (word) (intern-feature word db)) (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

(defun train (text type db)
  (dolist (feature (extract-features text db))
    (increment-count feature type))
  (increment-total-count type db))

(defun increment-count (feature type)
  (ecase type
    (:ham (incf (ham-count feature)))
    (:spam (incf (spam-count feature)))))

(defun increment-total-count (type db)
  (ecase type
    (:ham (incf (total-hams db)))
    (:spam (incf (total-spams db)))))

(defun spam-probability (feature db)
  "Basic probability that a feature with the given relative
frequencies will appear in a spam assuming spams and hams are
otherwise equally probable. One of the two frequencies must be
non-zero."
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 (total-spams db))))
          (ham-frequency (/ ham-count (max 1 (total-hams db)))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))


(defun bayesian-spam-probability (feature db &optional
                                  (assumed-probability 1/2)
                                  (weight 1))
  "Bayesian adjustment of a given probability given the number of
data points that went into it, an assumed probability, and a
weight we give that assumed probability."
  (let ((basic-probability (spam-probability feature db))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun score (features db)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature db) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  "Probability that chi-square >= value with given degrees-of-freedom.
Based on Gary Robinson's Python implementation."
  (assert (evenp degrees-of-freedom))
  ;; Due to rounding errors in the multiplication and exponentiation
  ;; the sum computed in the loop may end up a shade above 1.0 which
  ;; we can't have since it's supposed to represent a probability.
  (min
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))

(defun classification (score)
  (values
   (cond
     ((<= score *max-ham-score*) :ham)
     ((>= score *min-spam-score*) :spam)
     (t :unsure))
   score))
