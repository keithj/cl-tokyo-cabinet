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

(test new-bdb
  (let ((db (make-instance 'tcab-bdb)))
    (is-true (cffi:pointerp (cl-tcab::ptr-of db)))
    (dbm-delete db)))

(test dbm-open/bdb
  (let ((db (make-instance 'tcab-bdb))
        (bdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    ;; Can't create a new DB in read-only mode
    (signals dbm-error
      (dbm-open db bdb-filespec :write nil :create t))
    (dbm-open db bdb-filespec :write t :create t)
    (is-true (fad:file-exists-p bdb-filespec))
    (is-true (delete-file bdb-filespec))))

(test dbm-vanish/bdb
  (with-fixture bdb-100 ()
    (dbm-vanish db)
    (is (zerop (dbm-num-records db)))))

(test dbm-num-records/bdb
  (with-fixture bdb-100 ()
    (is (= 100 (dbm-num-records db)))
    (dbm-vanish db)
    (is (zerop (dbm-num-records db)))))

(test dbm-file-size/bdb
  (with-fixture bdb-100 ()
    (with-open-file (stream bdb-filespec
                     :direction :input)
        (= (dbm-file-size db)
           (file-length stream)))))

(test dbm-get/bdb/string/string
  (with-fixture bdb-100 ()
    (is-true (loop
                for i from 0 below 100
                for key = (format nil "key-~a" i)
                for value = (format nil "value-~a" i)
                always (string= (dbm-get db key) value)))))

(test dbm-put/bdb/string/string
  (with-fixture bdb-empty ()
    ;; Add one
    (is-true (dbm-put db "key-one" "value-one"))
    (is (string= "value-one" (dbm-get db "key-one")))
    ;; Keep
    (signals dbm-error
        (dbm-put db "key-one" "VALUE-TWO" :mode :keep))
    ;; Replace
    (is-true (dbm-put db "key-one" "VALUE-TWO" :mode :replace))
    (is (string= "VALUE-TWO" (dbm-get db "key-one")))
    ;; Concat
    (is-true (dbm-put db "key-one" "VALUE-THREE" :mode :concat))
    (is (string= "VALUE-TWOVALUE-THREE" (dbm-get db "key-one")))))

(test dbm-put/bdb/int32/string ()
  (let ((db (make-instance 'tcab-bdb))
        (bdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (is-true (set-comparator db :int32))
    (dbm-open db bdb-filespec :write t :create t)
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
    (is (string= "VALUE-TWOVALUE-THREE" (dbm-get db 111)))
    (dbm-close db)
    (delete-file bdb-filespec)))

(test dbm-get/bdb/int32/string
  (let ((db (make-instance 'tcab-bdb))
        (bdb-filespec (namestring (iou:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (is-true (set-comparator db :int32))
    (dbm-open db bdb-filespec :write t :create t)
    (loop
       for i from 0 below 100
       do (dbm-put db i (format nil "value-~a" i)))
    (is-true (loop
                for i from 0 below 100
                for value = (format nil "value-~a" i)
                always (string= (dbm-get db i) value)))
    (dbm-close db)
    (delete-file bdb-filespec)))
