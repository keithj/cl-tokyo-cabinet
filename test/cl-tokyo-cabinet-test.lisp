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
