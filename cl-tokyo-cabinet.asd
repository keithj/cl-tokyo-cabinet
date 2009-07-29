;;;
;;; Copyright (C) 2008-2009 Keith James. All rights reserved.
;;;
;;; This file is part of cl-tokyo-cabinet.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :deoxybyte-systems nil)
    (asdf:operate 'asdf:load-op :deoxybyte-systems)))

(defpackage :cl-tokyo-cabinet-system
  (:use :common-lisp :asdf :deoxybyte-systems)
  (:export :testsuite))

(in-package :cl-tokyo-cabinet-system)

(defsystem cl-tokyo-cabinet
    :name "Common Lisp Tokyo Cabinet"
    :version "0.1.0"
    :author "Keith James"
    :licence "GPL v3"
    :depends-on ((:version :cffi "0.10.3"))
    :in-order-to ((test-op (load-op :cl-tokyo-cabinet :cl-tokyo-cabinet-test)))
    :components ((:module :cl-tokyo-cabinet
                          :pathname "src/"
                          :components
                          ((:file "package")
                           (:file "tokyo-cabinet-ffi"
                                  :depends-on ("package"))
                           (:file "tokyo-cabinet"
                                  :depends-on ("package"
                                               "tokyo-cabinet-ffi"))
                           (:file "tokyo-cabinet-bdb"
                                  :depends-on ("package"
                                               "tokyo-cabinet-ffi"
                                               "tokyo-cabinet"))
                           (:file "tokyo-cabinet-hdb"
                                  :depends-on ("package"
                                               "tokyo-cabinet-ffi"
                                               "tokyo-cabinet"))))
                 (:lift-test-config :lift-tests
                                    :pathname "cl-tokyo-cabinet-test.config"
                                    :target-system :cl-tokyo-cabinet)))

