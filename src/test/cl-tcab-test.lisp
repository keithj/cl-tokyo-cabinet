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

(in-package :cl-tcab-system)

(fiveam:def-suite testsuite
    :description "The test suite.")


(in-package :cl-tcab-test)

(def-fixture bdb-empty ()
  (let ((db (make-instance 'tcab-bdb))
        (bdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (dbm-open db bdb-filespec :write :create)
    (&body)
    (dbm-close db)
    (delete-file bdb-filespec)))

(def-fixture bdb-100 ()
  (let ((db (make-instance 'tcab-bdb))
        (bdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (dbm-open db bdb-filespec :write :create)
    (loop
       for i from 0 below 100
       do (dbm-put db (format nil "key-~a" i) (format nil "value-~a" i)))
    (&body)
    (dbm-close db)
    (delete-file bdb-filespec)))

(def-fixture hdb-empty ()
  (let ((db (make-instance 'tcab-hdb))
        (bdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "hdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (dbm-open db bdb-filespec :write :create)
    (&body)
    (dbm-close db)
    (delete-file bdb-filespec)))

(def-fixture hdb-100 ()
  (let ((db (make-instance 'tcab-hdb))
        (hdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "hdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (dbm-open db hdb-filespec :write :create)
    (loop
       for i from 0 below 100
       do (dbm-put db (format nil "key-~a" i) (format nil "value-~a" i)))
    (&body)
    (dbm-close db)
    (delete-file hdb-filespec)))
