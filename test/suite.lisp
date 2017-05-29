(in-package :hu.dwim.zlib/test)

(defsuite (test :in root-suite))
(defsuite (test/random :in test))

(defun make-ub8-vector/random-content (length &optional (mode :semi))
  (let ((vector (cffi:make-shareable-byte-vector length)))
    ;; TODO when it's fulled up with random data it triggers errors with the :gzip container (probably because of overflowing)
    (ecase mode
      (:random
       (loop
         :for index :from 0 :below length
         :do (setf (aref vector index) (random 255))
         :finally (return t)))
      (:semi
       ;; initialize first half to random, second half to zero
       (loop
         :for index :from 0 :below (floor length 2)
         :do (setf (aref vector index) (random 255)))
       (loop
         :for index :from (floor length 2) :below length
         :do (setf (aref vector index) 0))
       t))
    vector))

(defun make-ub8-vector/random-content-and-length (length &optional (mode :semi))
  (make-ub8-vector/random-content (+ (/ length 2) (random length)) mode))

(defun vector-equal (v1 v2 &key (start1 0) (start2 0) (end1 (length v1)) (end2 (length v2)))
  (check-type start1 (integer 0))
  (check-type start2 (integer 0))
  (check-type end1 (integer 0))
  (check-type end2 (integer 0))
  (assert (<= end1 (length v1)))
  (assert (<= start1 end1))
  (assert (<= end2 (length v2)))
  (assert (<= start2 end2))
  (and (= (- end1 start1)
          (- end2 start2))
       (loop
         :for index1 :from start1 :below end1
         :for index2 :from start2 :below end2
         :do (unless (eql (aref v1 index1) (aref v2 index2))
               (return nil))
         :finally (return t))))
