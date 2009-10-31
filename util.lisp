;;; cl-factoradic - util.lisp 
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

(defun single (list)
  (and (consp list) (null (rest list))))

(defun collect-until (collector until start increment)
  (labels ((build (n acc)
                  (if (funcall until acc n) acc
                    (build (funcall increment n)
                           (cons (funcall collector n) acc)))))
    (build start nil)))

(defun mod-rest (number radix)
  (values (truncate (/ number radix))
          (rem number radix)))

(defun delete-at (array index)
  (delete (elt array index) array :count 1 :start index))

(defun remove-at (sequence index)
  (remove (nth index sequence) sequence :count 1 :start index))

(defun truncate-list-at (list index)
  (nthcdr (abs index) list))

(defun pad-list-left (list length &key (pad-element nil))
  (nconc (make-list (- length (length list))
                    :initial-element pad-element)
         list))

(defun set-list-left (list length &key (pad-element nil))
  (let ((diff-length (- length (length list))))
    (cond ((zerop diff-length) list)
          ((plusp diff-length) 
           (pad-list-left list (+ diff-length (length list))
                          :pad-element pad-element))
          (t (truncate-list-at list diff-length)))))

(defun number-sequence (from to &optional inc)
  (let ((inc (cond ((null inc) (signum (- to from)))
                   ((/= (signum inc) (signum (- to from))) (error "invalid increment"))
                   ((zerop inc) (error "inc cannot be zero"))
                   (t (the integer inc)))))
    (labels ((build (from to acc)
                   (if (= from to) acc
                     (build (+ from inc) to (cons from acc)))))
      (nreverse (cons to (build from to nil))))))

(defun make-thread (name function &rest args)
  "See SBCL documentation for SB-THREAD:MAKE-THREAD."
  (declare (ignorable name))
  #+:sb-thread
  (sb-thread:make-thread (lambda ()
                           (apply function args))
                         :name name)
  #-:sb-thread
  (apply function args))

(defun join-thread (thread &key (default nil))
  "See SBCL documentation for SB-THREAD:JOIN-THREAD."
  (declare (ignorable default))
  #+:sb-thread
  (sb-thread:join-thread thread :default default)
  #-:sb-thread
  thread)

;; (defun coop-mapcar (function seq threads)
;;   (make-thread 0 #'mapcar #'(lambda (n)
;;                               (factoradic-permutation n seq))
;;           (number-sequence start end)))
  

;; (defun permutations (seq &key (start 0) (end (last-permutation seq)))
;;   (mapcar #'(lambda (n)
;;               (factoradic-permutation n seq))
;;           (number-sequence start end)))
