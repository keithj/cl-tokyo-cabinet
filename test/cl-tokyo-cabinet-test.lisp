;;;
;;; Copyright (c) 2008-2010, Keith James.
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;     * Redistributions of source code must retain the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials
;;;       provided with the distribution.
;;;
;;;     * Neither the names of the copyright holders nor the names of
;;;       its contributors may be used to endorse or promote products
;;;       derived from this software without specific prior written
;;;       permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;

(in-package :cl-tokyo-cabinet-test)

(deftestsuite cl-tokyo-cabinet-tests ()
  ()
  (:setup (ensure-directories-exist (merge-pathnames "data/"))))

(deftestsuite bdb-tests (cl-tokyo-cabinet-tests)
  ())

(deftestsuite hdb-tests (cl-tokyo-cabinet-tests)
  ())

(deftestsuite bdb-empty-tests (bdb-tests)
  ((db (make-instance 'tc-bdb))
   (bdb-filespec (namestring (dxi:tmp-pathname
                              :basename "bdb" :type "db"
                              :tmpdir (merge-pathnames "data")))))
  (:setup (dbm-open db bdb-filespec :write :create))
  (:teardown (dbm-close db)
             (delete-file bdb-filespec)))

(deftestsuite bdb-100-tests (bdb-tests)
  ((db (make-instance 'tc-bdb))
   (bdb-filespec (namestring (dxi:tmp-pathname
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
   (hdb-filespec (namestring (dxi:tmp-pathname
                              :basename "hdb" :type "db"
                              :tmpdir (merge-pathnames "data")))))
  (:setup (dbm-open db hdb-filespec :write :create))
  (:teardown (dbm-close db)
             (delete-file hdb-filespec)))

(deftestsuite hdb-100-tests (hdb-tests)
  ((db (make-instance 'tc-hdb))
   (hdb-filespec (namestring (dxi:tmp-pathname
                              :basename "hdb" :type "db"
                              :tmpdir (merge-pathnames "data")))))
  (:setup (dbm-open db hdb-filespec :write :create)
          (loop
             for i from 0 below 100
             do (dbm-put db (format nil "key-~a" i) (format nil "value-~a" i))))
  (:teardown (dbm-close db)
             (delete-file hdb-filespec)))

(defun string-as-octets (str)
  (map-into (make-array (length str) :element-type '(unsigned-byte 8))
            #'char-code str))

;;; Tests shared between DB classes
(defun test-new-db (class)
  (let ((db (make-instance class)))
    (ensure (cffi:pointerp (tc::ptr-of db)))
    (dbm-delete db)))

(defun test-raise-error (class)
  (let ((db (make-instance class)))
    (ensure-condition dbm-error
      (tc::raise-error db))
    (ensure-condition dbm-error
      (tc::raise-error db "no args message"))
    (ensure-condition dbm-error
      (tc::raise-error db "message with ~a" ""))))

(defun test-dbm-open (class filespec)
  (let ((db (make-instance class)))
    ;; Can't create a new DB in read-only mode
    (ensure-condition dbm-error
      (dbm-open db filespec :read :create))
    (ensure (dbm-open db filespec :write :create))
    (ensure (fad:file-exists-p filespec))
    (ensure (delete-file filespec))))

(defun test-dbm-vanish (db)
  (dbm-vanish db)
  (ensure (zerop (dbm-num-records db))))

(defun test-dbm-num-records (db n)
  (ensure (= n (dbm-num-records db)))
  (dbm-vanish db)
  (ensure (zerop (dbm-num-records db))))

(defun test-dbm-file-size (db filespec)
  (with-open-file (stream filespec :direction :input
                          :element-type '(unsigned-byte 8))
    (ensure (= (dbm-file-size db)
               (file-length stream))
            :report "expected file size ~a but found ~a"
            :arguments ((dbm-file-size db) (file-length stream)))))

(defun test-dbm-get-string/string (db)
  (ensure (loop
             for i from 0 below 100
             for key = (format nil "key-~a" i)
             for value = (format nil "value-~a" i)
             always (string= (dbm-get db key) value))))

(defun test-dbm-get-string/octets (db)
  (ensure (loop
             for i from 0 below 100
             for key = (format nil "key-~a" i)
             for value = (format nil "value-~a" i)
             always (string= (dxu:make-sb-string
                              (dbm-get db key :octets)) value))))

(defun test-dbm-get-octets/octets (db)
  (ensure (loop
             for i from 0 below 100
             for key = (string-as-octets (format nil "key-~a" i))
             for value = (string-as-octets (format nil "value-~a" i))
             always (equalp (dbm-get db key :octets) value))))

(defun test-dbm-get-string/bad-type (db)
  (ensure-error
    (dbm-get db "key-0" :bad-type)))

(defun test-dbm-put-string/string (db)
  ;; Add one
  (ensure (dbm-put db "key-one" "value-one"))
  (ensure (equal "value-one" (dbm-get db "key-one")))
  ;; Keep
  (ensure-condition dbm-error
    (dbm-put db "key-one" "VALUE-TWO" :mode :keep))
  (ensure (equal "value-one" (dbm-get db "key-one")))
  ;; Replace
  (ensure (dbm-put db "key-one" "VALUE-TWO" :mode :replace))
  (ensure (equal "VALUE-TWO" (dbm-get db "key-one")))
  ;; Concat
  (ensure (dbm-put db "key-one" "VALUE-THREE" :mode :concat))
  (ensure (equal "VALUE-TWOVALUE-THREE" (dbm-get db "key-one"))))

(defun test-dbm-put-string/octets (db)
  ;; Add one
  (ensure (dbm-put db "key-one" (string-as-octets "value-one")))
  (ensure (equalp (string-as-octets "value-one")
                  (dbm-get db "key-one" :octets)))
  ;; Keep
  (ensure-condition dbm-error
    (dbm-put db "key-one" (string-as-octets "VALUE-TWO") :mode :keep))
  (ensure (equalp (string-as-octets "value-one")
                  (dbm-get db "key-one" :octets)))
  ;; Replace
  (ensure (dbm-put db "key-one" (string-as-octets "VALUE-TWO") :mode :replace))
  (ensure (equalp (string-as-octets "VALUE-TWO")
                  (dbm-get db "key-one" :octets)))
  ;; Concat
  (ensure (dbm-put db "key-one" (string-as-octets "VALUE-THREE") :mode :concat))
  (ensure (equalp (string-as-octets "VALUE-TWOVALUE-THREE")
                  (dbm-get db "key-one" :octets))))

(defun test-dbm-put-octets/string (db)
  (let ((key (string-as-octets "key-one")))
    ;; Add one
    (ensure (dbm-put db key "value-one"))
    (ensure (equal "value-one"  (dbm-get db key)))
    ;; Keep
    (ensure-condition dbm-error
      (dbm-put db key "VALUE-TWO" :mode :keep))
    (ensure (equal "value-one" (dbm-get db key)))
    ;; Replace
    (ensure (dbm-put db key "VALUE-TWO" :mode :replace))
    (ensure (equal "VALUE-TWO" (dbm-get db key)))
    ;; Concat
    (ensure (dbm-put db key "VALUE-THREE" :mode :concat))
    (ensure (equal "VALUE-TWOVALUE-THREE" (dbm-get db key)))))

(defun test-with-database (class filespec)
 (with-database (db filespec class :write :create)
   (ensure (dbm-put db "key-one" "value-one"))
   (ensure (string= "value-one" (dbm-get db "key-one"))))
 (ensure (fad:file-exists-p filespec))
 (delete-file filespec))

(defun test-with-transaction (class filespec)
  (with-database (db filespec class :write :create)
    (with-transaction (db)
      (ensure (dbm-put db "key-one" "value-one")))
    (ensure (string= "value-one" (dbm-get db "key-one"))))
  (delete-file filespec))

(defun test-with-transaction-rollback (class filespec)
  (with-database (db filespec class :write :create)
    (ensure-error
      (with-transaction (db)
        (ensure (dbm-put db "key-one" "value-one"))
        (error "Test error.")))
    (ensure-null (dbm-get db "key-one")))
  (delete-file filespec))
