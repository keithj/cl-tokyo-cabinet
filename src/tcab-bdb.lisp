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

(in-package :cl-tcab)

(defmethod initialize-instance :after ((db tcab-bdb) &key)
  (with-slots (ptr) db
    (setf ptr (tcbdbnew))))

(defmethod set-comparator ((db tcab-bdb) (comparator symbol))
  (tcbdbsetcmpfunc (ptr-of db) (or (%builtin-comparator comparator)
                                   comparator) (null-pointer)))

(defmethod raise-error ((db tcab-bdb) &optional text)
  (let* ((code (tcbdbecode (ptr-of db)))
         (msg (tcbdberrmsg code)))
    (error 'dbm-error :error-code code :error-msg msg :text text)))

(defmethod maybe-raise-error ((db tcab-bdb) &optional text)
  (let ((ecode (tcbdbecode (ptr-of db))))
    (cond ((= +tcesuccess+ ecode)
           t)
          ((= +tcenorec+ ecode)
           nil)
          (t
           (raise-error db text)))))

(defmethod dbm-open ((db tcab-bdb) filename &key write create truncate
                     (lock t) (blocking nil))
  (check-mode write create truncate lock blocking)
  (let ((mode-flags (combine-mode-flags
                     write create truncate lock blocking
                     +bdboreader+ +bdbowriter+ +bdbocreat+
                     +bdbotrunc+ +bdbonolck+ +bdbolcknb+))
        (db-ptr (ptr-of db)))
    (unless (tcbdbopen db-ptr filename mode-flags) ; opens db by side-effect
      (let* ((code (tcbdbecode db-ptr))
             (msg (tcbdberrmsg code)))
        (tcbdbdel db-ptr) ; clean up on error
        (error 'dbm-error :error-code code :error-msg msg))))
  db)

(defmethod dbm-close ((db tcab-bdb))
  (tcbdbclose (ptr-of db)))

(defmethod dbm-delete ((db tcab-bdb))
  (tcbdbdel (ptr-of db)))

(defmethod dbm-vanish ((db tcab-bdb))
  (tcbdbvanish (ptr-of db)))

(defmethod dbm-begin ((db tcab-bdb))
  (unless (tcbdbtranbegin (ptr-of db))
    (raise-error db)))

(defmethod dbm-commit ((db tcab-bdb))
  (unless (tcbdbtrancommit (ptr-of db))
    (raise-error db)))

(defmethod dbm-abort ((db tcab-bdb))
  (unless (tcbdbtranabort (ptr-of db))
    (raise-error db)))

(defmethod dbm-get ((db tcab-bdb) (key string) &optional (type :string))
  (ecase type
    (:string (get-string->string db key #'tcbdbget2))
    (:octets (get-string->octets db key #'tcbdbget))))

(defmethod dbm-get ((db tcab-bdb) (key integer) &optional (type :string))
  (ecase type
    (:string (get-int32->string db key #'tcbdbget))
    (:octets (get-int32->octets db key #'tcbdbget))))

(defmethod dbm-put ((db tcab-bdb) (key string) (value string) 
                    &key (mode :replace))
  (or (funcall (%bdb-str-put-fn mode) (ptr-of db) key value)
      (maybe-raise-error db (format nil "(key ~a) (value ~a)" key value))))

(defmethod dbm-put ((db tcab-bdb) (key integer) (value string)
                    &key (mode :replace))
  (declare (type int32 key))
  (let ((key-len (foreign-type-size :int32))
        (value-len (length value)))
    (with-foreign-object (key-ptr :int32)
      (setf (mem-ref key-ptr :int32) key)
      (with-foreign-string (value-ptr value)
        (or (funcall (%bdb-put-fn mode) (ptr-of db)
                     key-ptr key-len value-ptr value-len)
            (maybe-raise-error db(format nil "(key ~a) (value ~a)"
                                           key value)))))))

(defmethod dbm-put ((db tcab-bdb) (key integer) (value vector)
                    &key (mode :replace))
  (declare (type int32 key)
           (type (simple-array (unsigned-byte 8) (*)) value))
  (let ((key-len (foreign-type-size :int32))
        (value-len (length value)))
    (with-foreign-objects ((key-ptr :int32)
                           (value-ptr :string value-len))
      (setf (mem-ref key-ptr :int32) key)
      (loop
         for i from 0 below (length value)
         do (setf (mem-aref value-ptr :unsigned-char i) (aref value i)))
      (or (funcall (%bdb-put-fn mode) (ptr-of db)
                       key-ptr key-len value-ptr value-len)
        (maybe-raise-error db (format nil "(key ~a) (value ~a)"
                                      key value))))))

(defmethod dbm-num-records ((db tcab-bdb))
  (tcbdbrnum (ptr-of db)))

(defmethod dbm-file-size ((db tcab-bdb))
  (tcbdbfsiz (ptr-of db)))

(defmethod dbm-optimize ((db tcab-bdb) &rest args)
  (apply #'tcbdboptimize (ptr-of db) args))

(defun %bdb-put-fn (mode)
  (ecase mode
    (:replace #'tcbdbput)
    (:keep #'tcbdbputkeep)
    (:concat #'tcbdbputcat)
    (:duplicate #'tcbdbputdup)))

(defun %bdb-str-put-fn (mode)
  (ecase mode
    (:replace #'tcbdbput2)
    (:keep #'tcbdbputkeep2)
    (:concat #'tcbdbputcat2)
    (:duplicate #'tcbdbputdup2)))

(defun %builtin-comparator (type)
  (foreign-symbol-pointer (case type
                            (:lexical "tcbdbcmplexical")
                            (:decimal "tcbdbcmpdecimal")
                            (:int32 "tcbdbcmpint32")
                            (:int64 "tcbdbcmpint64"))))
