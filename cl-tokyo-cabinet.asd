;;;
;;; Copyright (C) 2008 Keith James. All rights reserved.
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

(defpackage #:cl-tokyo-cabinet-system
  (:use :common-lisp :asdf)
  (:export #:testsuite))

(in-package #:cl-tokyo-cabinet-system)

(defsystem cl-tokyo-cabinet
    :name "Common Lisp Tokyo Cabinet"
    :author "Keith James"
    :version "0.1.0"
    :licence "GPL"
    :depends-on (:cffi)
    :components ((:module :cl-tokyo-cabinet
                          :pathname "src/"
                          :components
                          ((:file "package")
                           (:file "tokyo-cabinet-cffi"
                                  :depends-on ("package"))
                           (:file "cl-tokyo-cabinet"
                                  :depends-on ("package"
                                               "tokyo-cabinet-cffi"))
                           (:file "tokyo-cabinet-bdb"
                                  :depends-on ("package"
                                               "tokyo-cabinet-cffi"
                                               "cl-tokyo-cabinet"))
                           (:file "tokyo-cabinet-hdb"
                                  :depends-on ("package"
                                               "tokyo-cabinet-cffi"
                                               "cl-tokyo-cabinet"))))))

(in-package #:asdf)

(defmethod perform ((op test-op) (c (eql (find-system
                                          :cl-tokyo-cabinet))))
  (operate 'load-op :cl-tokyo-cabinet-test)
  (let ((*default-pathname-defaults* (component-pathname c)))
    (funcall (intern (string :run!) (string :fiveam))
             'cl-tokyo-cabinet-system:testsuite)))

(defmethod operation-done-p ((op test-op) (c (eql (find-system
                                                   :cl-tokyo-cabinet))))
  nil)

(defmethod perform ((op cldoc-op) (c (eql (find-system
                                           :cl-tokyo-cabinet))))
  (unless (find-package :cl-tokyo-cabinet)
    (operate 'load-op :cl-tokyo-cabinet))
  (let ((*default-pathname-defaults* (component-pathname c))
        (fn-sym (intern (string :extract-documentation) (string :cldoc)))
        (op-sym (intern (string :html) (string :cldoc))))
    (funcall fn-sym op-sym "./doc/html" c)))