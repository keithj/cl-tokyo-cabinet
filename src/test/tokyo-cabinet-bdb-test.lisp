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

(addtest (bdb-tests) new-bdb/1
  (let ((db (make-instance 'tc-bdb)))
    (ensure (cffi:pointerp (tc::ptr-of db)))
    (dbm-delete db)))

(addtest (bdb-tests) dbm-open/bdb/1
  (let ((db (make-instance 'tc-bdb))
        (bdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    ;; Can't create a new DB in read-only mode
    (ensure-condition dbm-error
      (dbm-open db bdb-filespec :read :create))
    (ensure (dbm-open db bdb-filespec :write :create))
    (ensure (fad:file-exists-p bdb-filespec))
    (ensure (delete-file bdb-filespec))))

(addtest (bdb-100-tests) dbm-vanish/bdb/1
  (dbm-vanish db)
  (ensure (zerop (dbm-num-records db))))

(addtest (bdb-100-tests) dbm-num-records/bdb/1
  (ensure (= 100 (dbm-num-records db)))
  (dbm-vanish db)
  (ensure (zerop (dbm-num-records db))))

(addtest (bdb-100-tests) dbm-file-size/bdb/1
  (with-open-file (stream bdb-filespec :direction :input)
    (ensure (= (dbm-file-size db)
               (file-length stream)))))

(addtest (bdb-100-tests) dbm-get/bdb/string/string/1
  (ensure (loop
             for i from 0 below 100
             for key = (format nil "key-~a" i)
             for value = (format nil "value-~a" i)
             always (string= (dbm-get db key) value))))

(addtest (bdb-100-tests) dbm-get/bdb/string/octets/1
  (ensure (loop
             for i from 0 below 100
             for key = (format nil "key-~a" i)
             for value = (format nil "value-~a" i)
             always (string= (dxu:make-sb-string
                              (dbm-get db key :octets)) value))))

(addtest (bdb-100-tests) dbm-get/bdb/string/bad-type/1
  (ensure-error
    (dbm-get db "key-0" :bad-type)))

(addtest (bdb-empty-tests) dbm-put/bdb/string/string/1
  ;; Add one
  (ensure (dbm-put db "key-one" "value-one"))
  (ensure (string= "value-one" (dbm-get db "key-one")))
  ;; Keep
  (ensure-condition dbm-error
    (dbm-put db "key-one" "VALUE-TWO" :mode :keep))
  ;; Replace
  (ensure (dbm-put db "key-one" "VALUE-TWO" :mode :replace))
  (ensure (string= "VALUE-TWO" (dbm-get db "key-one")))
  ;; Concat
  (ensure (dbm-put db "key-one" "VALUE-THREE" :mode :concat))
  (ensure (string= "VALUE-TWOVALUE-THREE" (dbm-get db "key-one"))))

(addtest (bdb-empty-tests) dbm-put/bdb/string/octets/1
  (let ((octets (make-array 10 :element-type '(unsigned-byte 8)
                            :initial-contents (loop
                                                 for c across "abcdefghij"
                                                 collect (char-code c)))))
    (ensure (dbm-put db "key-one" octets))
    (ensure (equalp octets (dbm-get db "key-one" :octets)))))

(addtest (bdb-tests) dbm-put/bdb/int32/octets/1
  (let ((db (make-instance 'tc-bdb))
        (bdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data"))))
        (octets (make-array 10 :element-type '(unsigned-byte 8)
                            :initial-contents (loop
                                                 for c across "abcdefghij"
                                                 collect (char-code c)))))
    (ensure (set-comparator db :int32))
    (ensure (dbm-open db bdb-filespec :write :create))
    ;; Add one
    (ensure (dbm-put db 111 octets))
    (ensure (equalp octets (dbm-get db 111 :octets)))
    (dbm-close db)
    (delete-file bdb-filespec)))

(addtest (bdb-tests) dbm-put/bdb/int32/string/1
  (let ((db (make-instance 'tc-bdb))
        (bdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (ensure (set-comparator db :int32))
    (ensure (dbm-open db bdb-filespec :write :create))
    ;; Add one
    (ensure (dbm-put db 111 "value-one"))
    (ensure (string= "value-one" (dbm-get db 111)))
    ;; Keep
    (ensure-condition dbm-error
      (dbm-put db 111 "VALUE-TWO" :mode :keep))
    ;; Replace
    (ensure (dbm-put db 111 "VALUE-TWO" :mode :replace))
    (ensure (string= "VALUE-TWO" (dbm-get db 111)))
    ;; Concat
    (ensure (dbm-put db 111 "VALUE-THREE" :mode :concat))
    (ensure (string= "VALUE-TWOVALUE-THREE" (dbm-get db 111)))
    (dbm-close db)
    (delete-file bdb-filespec)))

(addtest (bdb-tests) dbm-get/bdb/int32/string/1
  (let ((db (make-instance 'tc-bdb))
        (bdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (ensure (set-comparator db :int32))
    (ensure (dbm-open db bdb-filespec :write :create))
    (loop
       for i from 0 below 100
       do (dbm-put db i (format nil "value-~a" i)))
    (ensure (loop
               for i from 0 below 100
               for value = (format nil "value-~a" i)
               always (string= (dbm-get db i) value)))
    (dbm-close db)
    (delete-file bdb-filespec)))

(addtest (bdb-tests) dbm-iter/bdb/int32/string/1
  (let ((db (make-instance 'tc-bdb))
        (bdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (ensure (set-comparator db :int32))
    (ensure (dbm-open db bdb-filespec :write :create))
    (loop
       for i from 0 below 100
       do (dbm-put db i (format nil "value-~a" i)))
    (let ((iter (iter-open db)))
      (iter-first iter)
      (ensure (loop
                 for i from 0 below 100
                 always (prog1
                            (and (= i (iter-key iter :integer))
                                 (string= (format nil "value-~a" i)
                                          (iter-get iter)))
                          (iter-next iter))))
      (iter-close iter))
    (dbm-close db)
    (delete-file bdb-filespec)))

(addtest (bdb-tests) with-database/bdb/1
  (let ((bdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (with-database (db bdb-filespec 'tc-bdb :write :create)
       (ensure (dbm-put db "key-one" "value-one"))
       (ensure (string= "value-one" (dbm-get db "key-one"))))
    (ensure (fad:file-exists-p bdb-filespec))
    (delete-file bdb-filespec)))

(addtest (bdb-tests) with-transaction/bdb/1
  (let ((bdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (with-database (db bdb-filespec 'tc-bdb :write :create)
      (ensure-error
        (with-transaction (db)
          (ensure (dbm-put db "key-one" "value-one"))
          (error "Test error.")))
      (ensure-null (dbm-get db "key-one"))) ; should rollback
    (delete-file bdb-filespec)))

(addtest (bdb-tests) with-iterator/bdb/1
  (let ((bdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "bdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    (with-database (db bdb-filespec 'tc-bdb :write :create)
      (let ((data (loop
                     for i from 0 below 10
                     collect (cons (format nil "key-~a" i)
                                   (format nil "value-~a" i)))))
        (loop for (key . value) in data
           do (dbm-put db key value))
        (with-iterator (iter db)
          (iter-first iter)
          (loop
             for (key . value) in (butlast data)
             do (progn
                  (ensure (string= key (iter-key iter)))
                  (ensure (iter-next iter)))
             finally (ensure-null (iter-next iter))))))
    (delete-file bdb-filespec)))
