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

(in-package :cl-tokyo-cabinet-test)

(deftestsuite cl-tokyo-cabinet-tests ()
  ())

(deftestsuite bdb-tests (cl-tokyo-cabinet-tests)
  ())

(deftestsuite hdb-tests (cl-tokyo-cabinet-tests)
  ())

(deftestsuite bdb-empty-tests (bdb-tests)
  ((db (make-instance 'tc-bdb))
   (bdb-filespec (namestring (dxi:make-tmp-pathname
                              :basename "bdb" :type "db"
                              :tmpdir (merge-pathnames "data")))))
  (:setup (dbm-open db bdb-filespec :write :create))
  (:teardown (dbm-close db)
             (delete-file bdb-filespec)))

(deftestsuite bdb-100-tests (bdb-tests)
  ((db (make-instance 'tc-bdb))
   (bdb-filespec (namestring (dxi:make-tmp-pathname
                              :basename "bdb" :type "db"
                              :tmpdir (merge-pathnames "data")))))
  (:setup (dbm-open db bdb-filespec :write :create)
          (loop
             for i from 0 below 100
             do (dbm-put db (format nil "key-~a" i) (format nil "value-~a" i))))
  (:teardown (dbm-close db)
             (delete-file bdb-filespec)))

(deftestsuite hdb-empty-tests (hdb-tests)
  ((db (make-instance 'tc-hdb))
   (hdb-filespec (namestring (dxi:make-tmp-pathname
                              :basename "hdb" :type "db"
                              :tmpdir (merge-pathnames "data")))))
  (:setup (dbm-open db hdb-filespec :write :create))
  (:teardown (dbm-close db)
             (delete-file hdb-filespec)))

(deftestsuite hdb-100-tests (hdb-tests)
  ((db (make-instance 'tc-hdb))
   (hdb-filespec (namestring (dxi:make-tmp-pathname
                              :basename "hdb" :type "db"
                              :tmpdir (merge-pathnames "data")))))
  (:setup (dbm-open db hdb-filespec :write :create)
          (loop
             for i from 0 below 100
             do (dbm-put db (format nil "key-~a" i) (format nil "value-~a" i))))
  (:teardown (dbm-close db)
             (delete-file hdb-filespec)))
