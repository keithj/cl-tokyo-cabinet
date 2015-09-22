;;;
;;; Copyright (c) 2015 Andrey Kotlarski. All rights reserved.
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

(defmethod initialize-instance :after ((db tc-fdb) &key)
  (with-slots (ptr)
      db
    (setf ptr (tcfdbnew))))

(defmethod raise-error ((db tc-fdb) &optional (message "")
                        &rest message-arguments)
  (let* ((code (tcfdbecode (ptr-of db)))
         (msg (tcfdberrmsg code)))
    (error 'dbm-error :error-code code :error-msg msg
           :text (apply #'format nil message message-arguments))))

(defmethod maybe-raise-error ((db tc-fdb) &optional message
                              &rest message-arguments)
  (let ((ecode (tcfdbecode (ptr-of db))))
    (cond ((= +tcesuccess+ ecode)
           t)
          ((= +tcenorec+ ecode)
           nil)
          (t
           (apply #'raise-error db message message-arguments)))))

(defmethod dbm-open ((db tc-fdb) filename &rest mode)
  (let ((db-ptr (ptr-of db)))
    (check-open-mode mode)
    (unless (tcfdbopen db-ptr filename mode) ; opens db by side-effect
      (let* ((code (tcfdbecode db-ptr))
             (msg (tcfdberrmsg code)))
        (tcfdbdel db-ptr) ; clean up on error
        (error 'dbm-error :error-code code :error-msg msg))))
  db)

(defmethod dbm-close ((db tc-fdb))
  (tcfdbclose (ptr-of db)))

(defmethod dbm-delete ((db tc-fdb))
  (tcfdbdel (ptr-of db)))

(defmethod dbm-vanish ((db tc-fdb))
  (tcfdbvanish (ptr-of db)))

(defmethod dbm-begin ((db tc-fdb))
  (unless (tcfdbtranbegin (ptr-of db))
    (raise-error db)))

(defmethod dbm-commit ((db tc-fdb))
  (unless (tcfdbtrancommit (ptr-of db))
    (raise-error db)))

(defmethod dbm-abort ((db tc-fdb))
  (unless (tcfdbtranabort (ptr-of db))
    (raise-error db)))

(defmethod dbm-get ((db tc-fdb) (key string) &optional (type :string))
  (ecase type
    (:string (get-string->string db key #'tcfdbget3))
    (:octets (get-string->octets db key #'tcfdbget2))))

(defmethod dbm-get ((db tc-fdb) (key integer) &optional (type :string))
  (ecase type
    (:string (get-int32->string db key #'tcfdbget2))
    (:octets (get-int32->octets db key #'tcfdbget2))))

(defmethod dbm-get ((db tc-fdb) (key vector) &optional (type :string))
  (ecase type
    (:string (get-octets->string db key #'tcfdbget2))
    (:octets (get-octets->octets db key #'tcfdbget2))))

(defmethod dbm-put ((db tc-fdb) (key vector) (value vector)
                    &key (mode :replace))
  (put-octets->octets db key value (%fdb-put-fn mode)))

(defmethod dbm-put ((db tc-fdb) (key vector) (value string)
                    &key (mode :replace))
  (put-octets->string db key value (%fdb-put-fn mode)))

(defmethod dbm-put ((db tc-fdb) (key string) (value string)
                    &key (mode :replace))
  (put-string->string db key value (%fdb-str-put-fn mode)))

(defmethod dbm-put ((db tc-fdb) (key string) (value vector) 
                    &key (mode :replace))
  (put-string->octets db key value (%fdb-put-fn mode)))

(defmethod dbm-put ((db tc-fdb) (key integer) (value string)
                    &key (mode :replace))
  (put-int32->string db key value (%fdb-put-fn mode)))

(defmethod dbm-put ((db tc-fdb) (key integer) (value vector)
                    &key (mode :replace))
  (put-int32->octets db key value (%fdb-put-fn mode)))

(defmethod dbm-rem ((db tc-fdb) (key string) &key remove-dups)
  (declare (ignore remove-dups))
  (rem-string->value db key #'tcfdbout3))

(defmethod dbm-rem ((db tc-fdb) (key integer) &key remove-dups)
  (declare (ignore remove-dups))
  (rem-int32->value db key #'tcfdbout2))

(defmethod iter-open ((db tc-fdb))
  (if (tcfdbiterinit (ptr-of db))
      (make-instance 'fdb-iterator :ptr (ptr-of db))
      (raise-error db)))

(defmethod iter-close ((iter fdb-iterator))
  t)

(defmethod iter-first ((iter fdb-iterator))
  (iter-next iter))

(defmethod iter-next ((iter fdb-iterator))
  (let ((next-id (tcfdbiternext (ptr-of iter))))
    (setf (current-id-of iter) next-id)
    (not (zerop next-id))))

(defmethod iter-key ((iter fdb-iterator) &optional (type :string))
  (declare (ignore type))
  (current-id-of iter))

(defmethod iter-get ((iter fdb-iterator) &optional (type :string))
  (let ((value-ptr nil))
    (unwind-protect
         (with-foreign-object (size-ptr :int)
           (setf value-ptr (tcfdbget (ptr-of iter) (current-id-of iter)
                                     size-ptr))
           (unless (null-pointer-p value-ptr)
             (ecase type
               (:string (foreign-string-to-lisp value-ptr :count
                                                (mem-ref size-ptr :int)))
               (:integer (mem-ref value-ptr :int32))
               (:octets (copy-foreign-value value-ptr size-ptr)))))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-free value-ptr)))))

(defmethod dbm-num-records ((db tc-fdb))
  (tcfdbrnum (ptr-of db)))

(defmethod dbm-file-namestring ((db tc-fdb))
  (tcfdbpath (ptr-of db)))

(defmethod dbm-file-size ((db tc-fdb))
  (tcfdbfsiz (ptr-of db)))

(defmethod dbm-optimize ((db tc-fdb) &key (width 0) (limsiz 0)
                         &allow-other-keys)
  (tcfdboptimize (ptr-of db) width limsiz))

(declaim (inline %fdb-put-fn))
(defun %fdb-put-fn (mode)
  (ecase mode
    (:replace #'tcfdbput2)
    (:keep #'tcfdbputkeep2)
    (:concat #'tcfdbputcat2)))

(declaim (inline %fdb-str-put-fn))
(defun %fdb-str-put-fn (mode)
  (ecase mode
    (:replace #'tcfdbput3)
    (:keep #'tcfdbputkeep3)
    (:concat #'tcfdbputcat3)))
