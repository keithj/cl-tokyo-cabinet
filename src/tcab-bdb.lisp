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

(defmacro %bdb-key-typed-put (db key-type key value)
  `(with-foreign-object (key-ptr ,key-type)
     (setf (mem-ref key-ptr ,key-type) key)
     (with-foreign-string (value-ptr value)
       (unless (tcbdbput (ptr-of ,db) key-ptr (foreign-type-size ,key-type)
                         value-ptr (length ,value))
         (raise-error ,db (format nil "key: ~a value: ~a" ,key ,value))))))

(defmacro %bdb-key-typed-get (db key-type key)
  `(let ((value-ptr))
     (unwind-protect
          (with-foreign-objects ((key-ptr ,key-type)
                                 (size-ptr :int))
            (setf (mem-ref key-ptr ,key-type) key)
            (setf value-ptr (tcbdbget (ptr-of ,db) key-ptr
                                      (foreign-type-size ,key-type)
                                      size-ptr))
            (if (null-pointer-p value-ptr)
                (maybe-raise-error ,db (format nil "key: ~a" ,key))
              (foreign-string-to-lisp value-ptr :count
                                      (mem-ref size-ptr :int))))
       (when (and value-ptr (not (null-pointer-p value-ptr)))
         (foreign-string-free value-ptr)))))

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
  (if (= +tcesuccess+ (tcbdbecode (ptr-of db)))
      nil
    (raise-error db text)))

(defmethod dbm-open ((db tcab-bdb) filename &key write create truncate
                     (lock t) (blocking nil))
  (validate-mode write create truncate lock blocking)
  (let ((mode-flags (combine-mode-flags
                     write create truncate lock blocking
                     +bdboreader+ +bdbowriter+ +bdbocreat+
                     +bdbotrunc+ +bdbonolck+ +bdbolcknb+))
        (db-ptr (ptr-of db)))
      (unless (tcbdbopen db-ptr ; opens db by side-effect
                         filename
                         (reduce #'(lambda (x y)
                                     (boole boole-ior x y))
                                 mode-flags))
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

(defmethod dbm-put ((db tcab-bdb) (key string) (value string) &key mode)
  (declare (ignore mode))
  (unless (tcbdbput2 (ptr-of db) key value)
    (raise-error db (format nil "key: ~a value: ~a" key value))))

(defmethod dbm-put ((db tcab-bdb) (key integer) (value string) &key mode)
  (declare (ignore mode))
  (declare (type int32 key))
  (%bdb-key-typed-put db :int32 key value))

(defmethod dbm-get ((db tcab-bdb) (key string))
  (let ((value-ptr))
    (unwind-protect
         (progn
           (setf value-ptr (tcbdbget2 (ptr-of db) key))
           (if (null-pointer-p value-ptr)
               (maybe-raise-error db)
             (foreign-string-to-lisp value-ptr)))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-string-free value-ptr)))))

(defmethod dbm-get ((db tcab-bdb) (key integer))
  (declare (type int32 key))
  (%bdb-key-typed-get db :int32 key))

(defmethod dbm-num-records ((db tcab-bdb))
  (tcbdbrnum (ptr-of db)))

(defmethod dbm-file-size ((db tcab-bdb))
  (tcbdbfsiz (ptr-of db)))

(defun %builtin-comparator (type)
  (foreign-symbol-pointer (case type
                            (:lexical "tcbdbcmplexical")
                            (:decimal "tcbdbcmpdecimal")
                            (:int32 "tcbdbcmpint32")
                            (:int64 "tcbdbcmpint64"))))
