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

(defmethod initialize-instance :after ((db tc-hdb) &key)
  (with-slots (ptr)
      db
    (setf ptr (tchdbnew))))

(defmethod raise-error ((db tc-hdb) &optional (message "")
                        &rest message-arguments)
  (let* ((code (tchdbecode (ptr-of db)))
         (msg (tchdberrmsg code)))
    (error 'dbm-error :error-code code :error-msg msg
           :text (apply #'format nil message message-arguments))))

(defmethod maybe-raise-error ((db tc-hdb) &optional message
                              &rest message-arguments)
  (let ((ecode (tchdbecode (ptr-of db))))
    (cond ((= +tcesuccess+ ecode)
           t)
          ((= +tcenorec+ ecode)
           nil)
          (t
           (apply #'raise-error db message message-arguments)))))

(defmethod dbm-open ((db tc-hdb) filename &rest mode)
  (let ((db-ptr (ptr-of db)))
    (check-open-mode mode)
    (unless (tchdbopen db-ptr filename mode) ; opens db by side-effect
      (let* ((code (tchdbecode db-ptr))
             (msg (tchdberrmsg code)))
        (tchdbdel db-ptr) ; clean up on error
        (error 'dbm-error :error-code code :error-msg msg))))
  db)

(defmethod dbm-close ((db tc-hdb))
  (tchdbclose (ptr-of db)))

(defmethod dbm-delete ((db tc-hdb))
  (tchdbdel (ptr-of db)))

(defmethod dbm-vanish ((db tc-hdb))
  (tchdbvanish (ptr-of db)))

(defmethod dbm-begin ((db tc-hdb))
  (unless (tchdbtranbegin (ptr-of db))
    (raise-error db)))

(defmethod dbm-commit ((db tc-hdb))
  (unless (tchdbtrancommit (ptr-of db))
    (raise-error db)))

(defmethod dbm-abort ((db tc-hdb))
  (unless (tchdbtranabort (ptr-of db))
    (raise-error db)))

(defmethod dbm-get ((db tc-hdb) (key string) &optional (type :string))
  (ecase type
    (:string (get-string->string db key #'tchdbget2))
    (:octets (get-string->octets db key #'tchdbget))))

(defmethod dbm-get ((db tc-hdb) (key integer) &optional (type :string))
  (ecase type
    (:string (get-int32->string db key #'tchdbget))
    (:octets (get-int32->octets db key #'tchdbget))))

(defmethod dbm-get ((db tc-hdb) (key vector) &optional (type :string))
  (ecase type
    (:string (get-octets->string db key #'tchdbget))
    (:octets (get-octets->octets db key #'tchdbget))))

(defmethod dbm-put ((db tc-hdb) (key vector) (value vector)
                    &key (mode :replace))
  (put-octets->octets db key value (%hdb-put-fn mode)))

(defmethod dbm-put ((db tc-hdb) (key vector) (value string)
                    &key (mode :replace))
  (put-octets->string db key value (%hdb-put-fn mode)))

(defmethod dbm-put ((db tc-hdb) (key string) (value string)
                    &key (mode :replace))
  (put-string->string db key value (%hdb-str-put-fn mode)))

(defmethod dbm-put ((db tc-hdb) (key string) (value vector) 
                    &key (mode :replace))
  (put-string->octets db key value (%hdb-put-fn mode)))

(defmethod dbm-put ((db tc-hdb) (key integer) (value string)
                    &key (mode :replace))
  (put-int32->string db key value (%hdb-put-fn mode)))

(defmethod dbm-put ((db tc-hdb) (key integer) (value vector)
                    &key (mode :replace))
  (put-int32->octets db key value (%hdb-put-fn mode)))

(defmethod dbm-rem ((db tc-hdb) (key string) &key remove-dups)
  (declare (ignore remove-dups))
  (rem-string->value db key #'tchdbout2))

(defmethod dbm-rem ((db tc-hdb) (key integer) &key remove-dups)
  (declare (ignore remove-dups))
  (rem-int32->value db key #'tchdbout))

(defmethod iter-open ((db tc-hdb))
  (if (tchdbiterinit (ptr-of db))
      (make-instance 'hdb-iterator :ptr (ptr-of db))
      (raise-error db)))

(defmethod iter-close ((iter hdb-iterator))
  t)

(defmethod iter-next ((iter hdb-iterator))
  (with-foreign-object (size-ptr :int)
    (let ((key-ptr (tchdbiternext (ptr-of iter) size-ptr)))
      (setf (next-key-of iter) key-ptr
            (key-size-of iter) size-ptr)
      (null-pointer-p key-ptr))))

(defmethod iter-key ((iter hdb-iterator) &optional (type :string))
  (let ((key-ptr (next-key-of iter))
        (size-ptr (key-size-of iter)))
    (unless (null-pointer-p key-ptr)
      (ecase type
        (:string (foreign-string-to-lisp key-ptr :count
                                         (mem-ref size-ptr :int)))
        (:integer (mem-ref key-ptr :int32))
        (:octets (copy-foreign-value key-ptr size-ptr))))))

(defmethod dbm-num-records ((db tc-hdb))
  (tchdbrnum (ptr-of db)))

(defmethod dbm-file-namestring ((db tc-hdb))
  (tchdbpath (ptr-of db)))

(defmethod dbm-file-size ((db tc-hdb))
  (tchdbfsiz (ptr-of db)))

(defmethod dbm-optimize ((db tc-hdb) &key (bucket-size 0)
                         (rec-align -1) (free-pool -1)
                         (options '(:defaults))
                         &allow-other-keys)
  (tchdboptimize (ptr-of db) bucket-size rec-align free-pool options))

(defmethod dbm-cache ((db tc-hdb) &key (records 0))
  (tchdbsetcache (ptr-of db) records))

(defmethod dbm-xmsize ((db tc-hdb) (size integer))
  (tchdbsetxmsiz (ptr-of db) size))

(declaim (inline %hdb-put-fn))
(defun %hdb-put-fn (mode)
  (ecase mode
    (:replace #'tchdbput)
    (:keep #'tchdbputkeep)
    (:concat #'tchdbputcat)
    (:async #'tchdbputasync)))

(declaim (inline %hdb-str-put-fn))
(defun %hdb-str-put-fn (mode)
  (ecase mode
    (:replace #'tchdbput2)
    (:keep #'tchdbputkeep2)
    (:concat #'tchdbputcat2)
    (:async #'tchdbputasync2)))
