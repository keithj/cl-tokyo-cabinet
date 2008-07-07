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

(in-package :cl-tokyo-cabinet-test)

(in-suite cl-tokyo-cabinet-system:testsuite)

(test new-hdb
  (let ((db (make-instance 'tc-hdb)))
    (is-true (cffi:pointerp (tc::ptr-of db)))
    (dbm-delete db)))

(test dbm-open/hdb
  (let ((db (make-instance 'tc-hdb))
        (hdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "hdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    ;; Can't create a new DB in read-only mode
    (signals dbm-error
      (dbm-open db hdb-filespec :read :create))
    (dbm-open db hdb-filespec :write :create)
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
    (with-open-file (stream hdb-filespec :direction :input)
        (= (dbm-file-size db)
           (file-length stream)))))

(test dbm-put/get/hdb/string/string
  (with-fixture hdb-100 ()
    (is-true (loop
                for i from 0 below 100
                for key = (format nil "key-~a" i)
                for value = (format nil "value-~a" i)
                always (string= (dbm-get db key) value)))))

(test dbm-get/hdb/string/octets
  (with-fixture hdb-100 ()
    (is-true (loop
                for i from 0 below 100
                for key = (format nil "key-~a" i)
                for value = (format nil "value-~a" i)
                always (string= (gpu:make-sb-string
                                 (dbm-get db key :octets)) value)))))

(test dbm-get/hdb/string/bad-type
  (with-fixture hdb-100 ()
    (signals error
      (dbm-get db "key-0" :bad-type))))

(test dbm-put/hdb/string/string
  (with-fixture hdb-empty ()
    ;; Add one
    (is-true (dbm-put db "key-one" "value-one"))
    (is (string= "value-one" (dbm-get db "key-one")))
    ;; Keep
    (signals dbm-error
        (dbm-put db "key-one" "VALUE-TWO" :mode :keep))
    (is (string= "value-one" (dbm-get db "key-one")))
    ;; Replace
    (is-true (dbm-put db "key-one" "VALUE-TWO" :mode :replace))
    (is (string= "VALUE-TWO" (dbm-get db "key-one")))
    ;; Concat
    (is-true (dbm-put db "key-one" "VALUE-THREE" :mode :concat))
    (is (string= "VALUE-TWOVALUE-THREE" (dbm-get db "key-one")))))

(test dbm-put/hdb/int32/string ()
 (with-fixture hdb-empty ()
    ;; Add one
    (is-true (dbm-put db 111 "value-one"))
    (is (string= "value-one" (dbm-get db 111)))
    ;; Keep
    (signals dbm-error
        (dbm-put db 111 "VALUE-TWO" :mode :keep))
    ;; Replace
    (is-true (dbm-put db 111 "VALUE-TWO" :mode :replace))
    (is (string= "VALUE-TWO" (dbm-get db 111)))
    ;; Concat
    (is-true (dbm-put db 111 "VALUE-THREE" :mode :concat))
    (is (string= "VALUE-TWOVALUE-THREE" (dbm-get db 111)))))

(test dbm-get/hdb/int32/string
 (with-fixture hdb-empty ()
    (loop
       for i from 0 below 100
       do (dbm-put db i (format nil "value-~a" i)))
    (is-true (loop
                for i from 0 below 100
                for value = (format nil "value-~a" i)
                always (string= (dbm-get db i) value)))))
