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

(defun hdb-test-file (&optional (basename "hdb") (dir "data"))
  (namestring (dxi:tmp-pathname :basename basename :type "db"
                                :tmpdir (merge-pathnames dir))))

(addtest (hdb-tests) new-hdb/1
  (test-new-db 'tc-hdb))

(addtest (hdb-tests) raise-error/hdb/1
  (test-raise-error 'tc-hdb))

(addtest (hdb-tests) dbm-open/hdb/1
  (test-dbm-open 'tc-hdb (hdb-test-file)))

(addtest (hdb-100-tests) hbm-vanish/hdb/1
  (test-dbm-vanish db))

(addtest (hdb-100-tests) dbm-num-records/hdb/1
  (test-dbm-num-records db 100))

;; This fails. I think it's a tc bug. I reported this, or similar, in
;; 2008, but the record of it seems to have been deleted from the tc
;; site.
(addtest (hdb-100-tests) dbm-file-size/hdb/1
  (test-dbm-file-size db hdb-filespec))

(addtest (hdb-100-tests) dbm-get/hdb/string/string/1
  (test-dbm-get-string/string db))

(addtest (hdb-100-tests) dbm-get/hdb/string/octets/1
  (test-dbm-get-string/octets db))

(addtest (hdb-100-tests) dbm-get/bdb/octets/octets/1
  (test-dbm-get-octets/octets db))

(addtest (hdb-100-tests) dbm-get/hdb/string/bad-type/1
  (test-dbm-get-string/bad-type db))

(addtest (hdb-empty-tests) dbm-put/hdb/string/string/1
  (test-dbm-put-string/string db))

(addtest (hdb-empty-tests) dbm-put/hdb/string/octets/1
  (test-dbm-put-string/octets db))

(addtest (hdb-empty-tests) dbm-put/hdb/octets/string/1
  (test-dbm-put-octets/string db))

(addtest (hdb-empty-tests) dbm-put/hdb/int32/octets/1
  (let ((octets (string-as-octets "abcdefghij")))
    ;; Add one
    (ensure (dbm-put db 111 octets))
    (ensure (equalp octets (dbm-get db 111 :octets)))))

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

(addtest (hdb-tests) with-database/hdb/1
  (test-with-database 'tc-hdb (hdb-test-file)))

(addtest (hdb-tests) with-transaction/hdb/1
  (test-with-transaction 'tc-hdb (hdb-test-file)))

(addtest (hdb-tests) with-transaction/hdb/2
  (test-with-transaction-rollback 'tc-hdb (hdb-test-file)))
