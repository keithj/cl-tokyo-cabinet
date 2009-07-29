;;;
;;; Copyright (C) 2008-2009 Keith James. All rights reserved.
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

(addtest (hdb-tests) new-hdb/1
  (let ((db (make-instance 'tc-hdb)))
    (ensure (cffi:pointerp (tc::ptr-of db)))
    (dbm-delete db)))

(addtest (hdb-tests) dbm-open/hdb/1
  (let ((db (make-instance 'tc-hdb))
        (hdb-filespec (namestring (dxi:make-tmp-pathname
                                   :basename "hdb" :type "db"
                                   :tmpdir (merge-pathnames "data")))))
    ;; Can't create a new DB in read-only mode
    (ensure-condition dbm-error
      (dbm-open db hdb-filespec :read :create))
    (ensure (dbm-open db hdb-filespec :write :create))
    (ensure (fad:file-exists-p hdb-filespec))
    (ensure (delete-file hdb-filespec))))

(addtest (hdb-100-tests) hbm-vanish/hdb/1
  (dbm-vanish db)
  (ensure (zerop (dbm-num-records db))))

(addtest (hdb-100-tests) dbm-num-records/hdb/1
  (ensure (= 100 (dbm-num-records db)))
  (dbm-vanish db)
  (ensure (zerop (dbm-num-records db))))

;; This fails. I think it's a tc bug. I reported this, or similar, in
;; 2008, but the record of it seems to have been deleted from the tc
;; site.
(addtest (hdb-100-tests) dbm-file-size/hdb/1
  (with-open-file (stream hdb-filespec :direction :input)
    (ensure (= (dbm-file-size db)
               (file-length stream))
            :report "expected file size ~a but found ~a"
            :arguments ((dbm-file-size db) (file-length stream)))))

(addtest (hdb-100-tests) dbm-put/get/hdb/string/string/1
 (ensure (loop
            for i from 0 below 100
            for key = (format nil "key-~a" i)
            for value = (format nil "value-~a" i)
            always (string= (dbm-get db key) value))))

(addtest (hdb-100-tests) dbm-get/hdb/string/octets/1
  (ensure (loop
             for i from 0 below 100
             for key = (format nil "key-~a" i)
             for value = (format nil "value-~a" i)
             always (string= (dxu:make-sb-string
                              (dbm-get db key :octets)) value))))

(addtest (hdb-100-tests) dbm-get/hdb/string/bad-type/1
 (ensure-error
   (dbm-get db "key-0" :bad-type)))

(addtest (hdb-empty-tests) dbm-put/hdb/string/string/1
  ;; Add one
  (ensure (dbm-put db "key-one" "value-one"))
  (ensure (string= "value-one" (dbm-get db "key-one")))
  ;; Keep
  (ensure-condition dbm-error
    (dbm-put db "key-one" "VALUE-TWO" :mode :keep))
  (ensure (string= "value-one" (dbm-get db "key-one")))
  ;; Replace
  (ensure (dbm-put db "key-one" "VALUE-TWO" :mode :replace))
  (ensure (string= "VALUE-TWO" (dbm-get db "key-one")))
  ;; Concat
  (ensure (dbm-put db "key-one" "VALUE-THREE" :mode :concat))
  (ensure (string= "VALUE-TWOVALUE-THREE" (dbm-get db "key-one"))))

(addtest (hdb-empty-tests) dbm-put/hdb/int32/string/1
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
  (ensure (string= "VALUE-TWOVALUE-THREE" (dbm-get db 111))))

(addtest (hdb-empty-tests) dbm-get/hdb/int32/string/1
  (loop
     for i from 0 below 100
     do (dbm-put db i (format nil "value-~a" i)))
  (ensure (loop
             for i from 0 below 100
             for value = (format nil "value-~a" i)
             always (string= (dbm-get db i) value))))
