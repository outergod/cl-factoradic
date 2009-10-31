;;; cl-factoradic - packages.lisp 
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

(in-package :cl-factoradic-system)

(defpackage :cl-factoradic
  (:nicknames :factoradic)
  (:use :cl :asdf)
  (:export :factorial
           :create-factorials-until
           :factoradices
           :factoradices-for
           :decimal-to-factoradic
           :factoradic-permutation
           :last-permutation
           :permutations
           :dump-string-permutations))
