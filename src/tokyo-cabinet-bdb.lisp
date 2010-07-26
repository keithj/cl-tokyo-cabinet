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

(in-package :tokyo-cabinet)

(defmethod initialize-instance :after ((db tc-bdb) &key)
  (with-slots (ptr)
      db
    (setf ptr (tcbdbnew))))

(defmethod set-comparator ((db tc-bdb) (comparator symbol))
  (tcbdbsetcmpfunc (ptr-of db) (or (%builtin-comparator comparator)
                                   comparator) (null-pointer)))

(defmethod raise-error ((db tc-bdb) &optional (message "")
                        &rest message-arguments)
  (let* ((code (tcbdbecode (ptr-of db)))
         (msg (tcbdberrmsg code)))
    (error 'dbm-error :error-code code :error-msg msg
           :text (apply #'format nil message message-arguments))))

(defmethod maybe-raise-error ((db tc-bdb) &optional message
                              &rest message-arguments)
  (let ((ecode (tcbdbecode (ptr-of db))))
    (cond ((= +tcesuccess+ ecode)
           t)
          ((= +tcenorec+ ecode)
           nil)
          (t
           (apply #'raise-error db message message-arguments)))))

(defmethod dbm-open ((db tc-bdb) filespec &rest mode)
  (let ((db-ptr (ptr-of db)))
    (check-open-mode mode)
    (unless (tcbdbopen db-ptr filespec mode) ; opens db by side-effect
      (let* ((code (tcbdbecode db-ptr))
             (msg (tcbdberrmsg code)))
        (tcbdbdel db-ptr) ; clean up on error
        (error 'dbm-error :error-code code :error-msg msg))))
  db)

(defmethod dbm-close ((db tc-bdb))
  (tcbdbclose (ptr-of db)))

(defmethod dbm-delete ((db tc-bdb))
  (tcbdbdel (ptr-of db)))

(defmethod dbm-vanish ((db tc-bdb))
  (tcbdbvanish (ptr-of db)))

(defmethod dbm-begin ((db tc-bdb))
  (unless (tcbdbtranbegin (ptr-of db))
    (raise-error db)))

(defmethod dbm-commit ((db tc-bdb))
  (unless (tcbdbtrancommit (ptr-of db))
    (raise-error db)))

(defmethod dbm-abort ((db tc-bdb))
  (unless (tcbdbtranabort (ptr-of db))
    (raise-error db)))

(defmethod dbm-get ((db tc-bdb) (key string) &optional (type :string))
  (ecase type
    (:string (get-string->string db key #'tcbdbget2))
    (:octets (get-string->octets db key #'tcbdbget))))

(defmethod dbm-get ((db tc-bdb) (key integer) &optional (type :string))
  (ecase type
    (:string (get-int32->string db key #'tcbdbget))
    (:octets (get-int32->octets db key #'tcbdbget))))

;; KTR
(defmethod dbm-get ((db tc-bdb) (key vector) &optional (type :string))
  (ecase type
    (:string (get-octets->string db key #'tcbdbget))
    (:octets (get-octets->octets db key #'tcbdbget))))
;; KTR

(defmethod dbm-put ((db tc-bdb) (key vector) (value vector)
                    &key (mode :replace))
  (put-octets->octets db key value (%bdb-put-fn mode)))

;; KTR
(defmethod dbm-put ((db tc-bdb) (key vector) (value string)
                    &key (mode :replace))
  (put-octets->string db key value (%bdb-put-fn mode)))
;; KTR

(defmethod dbm-put ((db tc-bdb) (key string) (value string) 
                    &key (mode :replace))
  (put-string->string db key value (%bdb-str-put-fn mode)))

(defmethod dbm-put ((db tc-bdb) (key string) (value vector) 
                    &key (mode :replace))
  (put-string->octets db key value (%bdb-put-fn mode)))

(defmethod dbm-put ((db tc-bdb) (key integer) (value string)
                    &key (mode :replace))
  (put-int32->string db key value (%bdb-put-fn mode)))

(defmethod dbm-put ((db tc-bdb) (key integer) (value vector)
                    &key (mode :replace))
  (put-int32->octets db key value (%bdb-put-fn mode)))

(defmethod dbm-rem ((db tc-bdb) (key string) &key remove-dups)
  (if remove-dups
      (rem-string->duplicates db key #'tcbdbout3)
      (rem-string->value db key #'tcbdbout2)))

(defmethod dbm-rem ((db tc-bdb) (key integer) &key remove-dups)
  (if remove-dups
      (rem-int32->value db key #'tcbdbout3)
      (rem-int32->value db key #'tcbdbout)))

;; KTR
(defmethod dbm-rem ((db tc-bdb) (key vector) &key remove-dups)
  (if remove-dups
      (rem-octets->value db key #'tcbdbout3)
      (rem-octets->value db key #'tcbdbout)))
;; KTR

(defmethod iter-open ((db tc-bdb))
  (make-instance 'bdb-iterator :ptr (tcbdbcurnew (ptr-of db))))

(defmethod iter-close ((iter bdb-iterator))
  (tcbdbcurdel (ptr-of iter)))

(defmethod iter-first ((iter bdb-iterator))
  (tcbdbcurfirst (ptr-of iter)))

(defmethod iter-last ((iter bdb-iterator))
  (tcbdbcurlast (ptr-of iter)))

(defmethod iter-prev ((iter bdb-iterator))
  (tcbdbcurprev (ptr-of iter)))

(defmethod iter-next ((iter bdb-iterator))
  (tcbdbcurnext (ptr-of iter)))
  
(defmethod iter-jump ((iter bdb-iterator) (key string))
  (tcbdbcurjump2 (ptr-of iter) key))

(defmethod iter-jump ((iter bdb-iterator) (key integer))
  (declare (type int32 key))
  (with-foreign-object (key-ptr :int32)
      (setf (mem-ref key-ptr :int32) key)
      (tcbdbcurjump (ptr-of iter) key-ptr (foreign-type-size :int32))))

;; KTR
(defmethod iter-jump ((iter bdb-iterator) (key vector))
  (declare (type (vector octet) key))
  (let ((key-len (length key)))
    (with-foreign-object (key-ptr :unsigned-char key-len)
      (loop
	 for i from 0 below key-len
	 do (setf (mem-aref key-ptr :unsigned-char i) (aref key i)))
      (tcbdbcurjump (ptr-of iter) key-ptr key-len))))
;; KTR

(defmethod iter-get ((iter bdb-iterator) &optional (type :string))
  (let ((value-ptr nil))
    (unwind-protect
         (with-foreign-object (size-ptr :int)
           (setf value-ptr (tcbdbcurval (ptr-of iter) size-ptr))
           (unless (null-pointer-p value-ptr)
             (ecase type
               (:string (foreign-string-to-lisp value-ptr :count
                                                (mem-ref size-ptr :int)))
               (:integer (mem-ref value-ptr :int32))
               (:octets (copy-foreign-value value-ptr size-ptr)))))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-free value-ptr)))))

(defmethod iter-put ((iter bdb-iterator) (value string)
                     &key (mode :current))
  (tcbdbcurput2 (ptr-of iter) value (%bdb-iter-mode mode)))

(defmethod iter-put ((iter bdb-iterator) (value vector)
                     &key (mode :current))
  (declare (type (vector octet) value))
  (let ((value-len (length value)))
    (with-foreign-object (value-ptr :unsigned-char value-len)
      (tcbdbcurput (ptr-of iter) value-ptr value-len (%bdb-iter-mode mode)))))

(defmethod iter-rem ((iter bdb-iterator))
  (tcbdbcurout (ptr-of iter)))

(defmethod iter-key ((iter bdb-iterator) &optional (type :string))
  (let ((key-ptr nil))
    (unwind-protect
         (with-foreign-object (size-ptr :int)
           (setf key-ptr (tcbdbcurkey (ptr-of iter) size-ptr))
           (unless (null-pointer-p key-ptr)
             (ecase type
               (:string (foreign-string-to-lisp key-ptr :count
                                                (mem-ref size-ptr :int)))
               (:integer (mem-ref key-ptr :int32))
               (:octets (copy-foreign-value key-ptr size-ptr)))))
      (when (and key-ptr (not (null-pointer-p key-ptr)))
        (foreign-free key-ptr)))))

(defmethod dbm-num-records ((db tc-bdb))
  (tcbdbrnum (ptr-of db)))

(defmethod dbm-file-namestring ((db tc-bdb))
  (tcbdbpath (ptr-of db)))

(defmethod dbm-file-size ((db tc-bdb))
  (tcbdbfsiz (ptr-of db)))

(defmethod dbm-optimize ((db tc-bdb) &key (leaf 0) (non-leaf 0)
                         (bucket-size 0) (rec-align -1) (free-pool -1)
                         (opts '(:defaults)))
  (tcbdboptimize (ptr-of db) leaf non-leaf bucket-size rec-align
                 free-pool opts))

(defmethod dbm-cache ((db tc-bdb) &key (leaf 1024) (non-leaf 512))
  (tcbdbsetcache (ptr-of db) leaf non-leaf))

(defmethod dbm-xmsize ((db tc-bdb) (size integer))
  (tcbdbsetxmsiz (ptr-of db) size))

(declaim (inline %bdb-put-fn))
(defun %bdb-put-fn (mode)
  (ecase mode
    (:replace #'tcbdbput)
    (:keep #'tcbdbputkeep)
    (:concat #'tcbdbputcat)
    (:duplicate #'tcbdbputdup)))

(declaim (inline %bdb-str-put-fn))
(defun %bdb-str-put-fn (mode)
  (ecase mode
    (:replace #'tcbdbput2)
    (:keep #'tcbdbputkeep2)
    (:concat #'tcbdbputcat2)
    (:duplicate #'tcbdbputdup2)))

(declaim (inline %builtin-comparator))
(defun %builtin-comparator (type)
  (foreign-symbol-pointer (case type
                            (:lexical "tccmpintlexical")
                            (:decimal "tccmpintdecimal")
                            (:int32 "tccmpint32")
                            (:int64 "tccmpint64"))))

(declaim (inline %bdb-iter-mode))
(defun %bdb-iter-mode (mode)
  (ecase mode
    (:current +bdbcpcurrent+)
    (:prev +bdbcpbefore+)
    (:after +bdbcpafter+)))
