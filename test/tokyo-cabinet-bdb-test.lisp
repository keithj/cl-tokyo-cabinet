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

(defun bdb-test-file (&optional (basename "bdb") (dir "data"))
  (namestring (dxi:tmp-pathname :basename basename :type "db"
                                :tmpdir (merge-pathnames dir))))

(addtest (bdb-tests) new-bdb/1
  (let ((db (make-instance 'tc-bdb)))
    (ensure (cffi:pointerp (tc::ptr-of db)))
    (dbm-delete db)))

(addtest (bdb-tests) raise-error/bdb/1
  (let ((db (make-instance 'tc-bdb)))
    (ensure-condition dbm-error
      (tc::raise-error db))
    (ensure-condition dbm-error
      (tc::raise-error db "no args message"))
    (ensure-condition dbm-error
      (tc::raise-error db "message with ~a" ""))))

(addtest (bdb-tests) dbm-open/bdb/1
  (let ((db (make-instance 'tc-bdb))
        (bdb-filespec (bdb-test-file)))
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

(addtest (bdb-empty-tests) dbm-put/bdb/octets/string/1
  (let ((octets (make-array 7 :element-type '(unsigned-byte 8)
                            :initial-contents (loop
                                                 for c across "key-one"
                                                 collect (char-code c)))))
    (ensure (dbm-put db octets "value-one"))
    (ensure (equal "value-one" (dbm-get db octets :string)))))

(addtest (bdb-tests) dbm-put/bdb/int32/octets/1
  (let ((db (make-instance 'tc-bdb))
        (bdb-filespec (bdb-test-file))
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
        (bdb-filespec (bdb-test-file)))
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
        (bdb-filespec (bdb-test-file)))
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
        (bdb-filespec (bdb-test-file)))
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
  (let ((bdb-filespec (bdb-test-file)))
    (with-database (db bdb-filespec 'tc-bdb :write :create)
       (ensure (dbm-put db "key-one" "value-one"))
       (ensure (string= "value-one" (dbm-get db "key-one"))))
    (ensure (fad:file-exists-p bdb-filespec))
    (delete-file bdb-filespec)))

(addtest (bdb-tests) with-transaction/bdb/1
  (let ((bdb-filespec (bdb-test-file)))
    (with-database (db bdb-filespec 'tc-bdb :write :create)
      (ensure-error
        (with-transaction (db)
          (ensure (dbm-put db "key-one" "value-one"))
          (error "Test error.")))
      (ensure-null (dbm-get db "key-one"))) ; should rollback
    (delete-file bdb-filespec)))

(addtest (bdb-tests) with-iterator/bdb/1
  (let ((bdb-filespec (bdb-test-file)))
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
