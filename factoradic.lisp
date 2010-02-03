;;; cl-factoradic - factoradic.lisp 
;;; Copyright (C) 2009  Alexander Kahl <e-user@fsfe.org>
;;; This file is part of cl-factoradic.
;;; cl-factoradic is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.

;;; cl-factoradic is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-factoradic)

(declaim (optimize (speed 3) (debug 0) (safety 0) (space 0)))

(defun factorial (number)
  (labels ((fac (n acc)
  (declare (type fixnum n acc))
             (if (<= n 1)
                 acc
               (fac (1- n) (* n acc)))))
    (fac number 1)))

(defun create-factoradices-until (function)
  (collect-until #'factorial function 0 #'1+))

(defun factoradices (count)
  (declare (type fixnum count))
  (create-factoradices-until #'(lambda (seq n)
                                 (declare (ignore seq) (type fixnum n))
                                 (>= n count))))

(defun factoradices-for (number)
  (declare (type fixnum number))
  (if (= number 0)
      (list 0)
    (rest (create-factoradices-until #'(lambda (seq n)
                                         (declare (ignore n))
                                         (> (or (the (or fixnum null) (first seq)) 0) number))))))

(defun decimal-to-factoradic (number)
  (labels ((dtf (n radices acc)
             (if (single radices) (nconc acc (list 0))
               (multiple-value-bind (value rem) (mod-rest n (first radices))
                 (dtf rem (rest radices) (nconc acc (list value)))))))
  (dtf number (factoradices-for number) nil)))

(defun factoradic-permutation (sequence number)
  (labels ((permutate (factors seq acc)
             (if (null factors) acc
               (permutate (rest factors)
                          (remove-at seq (first factors))
                          (nconc acc (list (nth (first factors) seq)))))))
      (permutate (set-list-left (decimal-to-factoradic number)
                                (length sequence) :pad-element 0) sequence nil)))

(defun last-permutation (list)
  (1- (factorial (length list))))

(defun permutations (sequence &key (start 0) (end (last-permutation sequence)))
  (mapcan #'(lambda (n)
              (list (factoradic-permutation sequence n)))
          (number-sequence start end)))

(defun dump-string-permutations (string &optional (stream *standard-output*))
  (let ((sequence (coerce string 'list)))
    (do ((i 0 (1+ i)))
        ((> i (last-permutation sequence)) nil)
      (format stream "~{~a~}~%" (factoradic-permutation sequence i)))))

;; FACTORADIC> (time
;;              (let ((sequence (coerce "alexander" 'list)))
;;                (do ((i 0 (1+ i)))
;;                    ((> i (last-permutation sequence)) nil)
;;                  (factoradic-permutation sequence i))))
;; Evaluation took:
;;   9.071 seconds of real time
;;   9.026627 seconds of total run time (8.966636 user, 0.059991 system)
;;   [ Run times consist of 0.131 seconds GC time, and 8.896 seconds non-GC time. ]
;;   99.51% CPU
;;   14,477,223,156 processor cycles
;;   253,092,160 bytes consed
