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

(in-package :cl-tokyo-cabinet)

(defmethod initialize-instance :after ((db tc-hdb) &key)
  (with-slots (ptr) db
    (setf ptr (tchdbnew))))

(defmethod raise-error ((db tc-hdb) &optional text)
  (let* ((code (tchdbecode (ptr-of db)))
         (msg (tchdberrmsg code)))
    (error 'dbm-error :error-code code :error-msg msg :text text)))

(defmethod maybe-raise-error ((db tc-hdb) &optional text)
  (let ((ecode (tchdbecode (ptr-of db))))
    (cond ((= +tcesuccess+ ecode)
           t)
          ((= +tcenorec+ ecode)
           nil)
          (t
           (raise-error db text)))))

(defmethod dbm-open ((db tc-hdb) filename &rest mode)
  (let ((db-ptr (ptr-of db)))
    (validate-open-mode mode)
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

(defmethod dbm-get ((db tc-hdb) (key string) &optional (type :string))
  (ecase type
    (:string (get-string->string db key #'tchdbget2))
    (:octets (get-string->octets db key #'tchdbget))))

(defmethod dbm-get ((db tc-hdb) (key integer) &optional (type :string))
  (ecase type
    (:string (get-int32->string db key #'tchdbget))
    (:octets (get-int32->octets db key #'tchdbget))))

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

(defun %hdb-put-fn (mode)
  (ecase mode
    (:replace #'tchdbput)
    (:keep #'tchdbputkeep)
    (:concat #'tchdbputcat)
    (:async #'tchdbputasync)))

(defun %hdb-str-put-fn (mode)
  (ecase mode
    (:replace #'tchdbput2)
    (:keep #'tchdbputkeep2)
    (:concat #'tchdbputcat2)
    (:async #'tchdbputasync2)))
