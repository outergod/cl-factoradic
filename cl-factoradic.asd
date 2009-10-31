;;; cl-factoradic - cl-factoradic.asd
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

(in-package :cl-user)

(defpackage :cl-factoradic-system
  (:use :cl :asdf))

(in-package :cl-factoradic-system)

(asdf:defsystem :cl-factoradic
                :description "cl-factoradic: Factoradic number system library."
                :version "0.1"
                :author "Alexander Kahl <e-user@fsfe.org>"
                :license "GPLv3+"
                :components ((:file "packages")
                             (:file "util" :depends-on ("packages"))
                             (:file "factoradic" :depends-on ("util"))))
