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

(defpackage #:cl-tcab-system
  (:use :common-lisp :asdf)
  (:export #:testsuite))

(in-package #:cl-tcab-system)

(defsystem cl-tcab
    :name "Common Lisp Tokyo Cabinet"
    :author "Keith James"
    :version "0.1.0"
    :licence "GPL"
    :depends-on (:cffi :cl-gp-utilities)
    :components ((:module :cl-tcab
                          :pathname "src/"
                          :components ((:file "package")
                                       (:file "tcab-cffi"
                                              :depends-on ("package"))
                                       (:file "cl-tcab"
                                              :depends-on ("package"
                                                           "tcab-cffi"))
                                       (:file "tcab-bdb"
                                              :depends-on ("package"
                                                           "tcab-cffi"
                                                           "cl-tcab"))
                                       (:file "tcab-hdb"
                                              :depends-on ("package"
                                                           "tcab-cffi"
                                                           "cl-tcab"))))))

(in-package #:asdf)

(defmethod perform ((op test-op) (c (eql (find-system
                                          :cl-tcab))))
  (operate 'load-op :cl-tcab-test)
  (let ((*default-pathname-defaults* (component-pathname c)))
    (funcall (intern (string :run!) (string :fiveam))
             'cl-tcab-system:testsuite)))

(defmethod operation-done-p ((op test-op) (c (eql (find-system
                                                   :cl-tcab))))
  nil)

(defmethod perform ((op cldoc-op) (c (eql (find-system
                                           :cl-tcab))))
  (unless (find-package :cl-tcab)
    (operate 'load-op :cl-tcab))
  (let ((*default-pathname-defaults* (component-pathname c))
        (fn-sym (intern (string :extract-documentation) (string :cldoc)))
        (op-sym (intern (string :html) (string :cldoc))))
    (funcall fn-sym op-sym "./doc/html" c)))
