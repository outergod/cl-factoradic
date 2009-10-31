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

(defun factorial (number)
  (labels ((fac (n acc)
                (cond ((zerop n) 0)
                      ((= n 1) acc)
                      (t (fac (1- n) (* n acc))))))
    (fac number 1)))

(defun create-factoradices-until (function)
  (collect-until #'factorial function 0 #'1+))

(defun factoradices (count)
  (create-factoradices-until #'(lambda (seq n)
                                 (declare (ignore seq))
                                 (>= n count))))

(defun factoradices-for (number)
  (rest (create-factoradices-until #'(lambda (seq n)
                                       (declare (ignore n))
                                       (> (or (first seq) 0) number)))))

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

  ;; (let ((vector (coerce sequence 'vector)))
  ;;   (loop for i in (set-list-left (decimal-to-factoradic number)
  ;;                                 (length vector) :pad-element 0)
  ;;         for elt = (elt vector i)
  ;;         do (delete-at vector i)
  ;;         collect elt)))


(defun last-permutation (list)
  (1- (factorial (length list))))

(defun permutations (list &key (start 0) (end (last-permutation list)))
  (mapcan #'(lambda (n)
              (list (factoradic-permutation list n)))
          (number-sequence start end)))

(defun dump-string-permutations (string)
  (dolist (permutation (permutations (coerce string 'list)))
    (format *standard-output* "~{~a~}~%" permutation)))

