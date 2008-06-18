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

(in-package :cl-tcab-test)

(in-suite cl-tcab-system:testsuite)

(test new-hdb
  (let ((db (make-instance 'tcab-hdb)))
    (is-true (cffi:pointerp (cl-tcab::ptr-of db)))
    (dbm-delete db)))

(test dbm-open/hdb
  (let ((db (make-instance 'tcab-hdb))
        (hdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "hdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    ;; Can't create a new DB in read-only mode
    (signals dbm-error
      (dbm-open db hdb-filespec :write nil :create t))
    (dbm-open db hdb-filespec :write t :create t)
    (is-true (fad:file-exists-p hdb-filespec))
    (is-true (delete-file hdb-filespec))))

(test hbm-vanish/hdb
  (with-fixture hdb-100 ()
    (dbm-vanish db)
    (is (zerop (dbm-num-records db)))))

(test dbm-num-records/hdb
  (with-fixture hdb-100 ()
    (is (= 100 (dbm-num-records db)))
    (dbm-vanish db)
    (is (zerop (dbm-num-records db)))))

(test dbm-file-size/hdb
  (with-fixture hdb-100 ()
    (with-open-file (stream hdb-filespec
                     :direction :input)
        (= (dbm-file-size db)
           (file-length stream)))))
