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

(defmethod initialize-instance :after ((db tcab-hdb) &key)
  (with-slots (ptr) db
    (setf ptr (tchdbnew))))

(defmethod raise-error ((db tcab-hdb) &optional text)
  (let* ((code (tchdbecode (ptr-of db)))
         (msg (tchdberrmsg code)))
    (error 'dbm-error :error-code code :error-msg msg :text text)))

(defmethod maybe-raise-error ((db tcab-bdb) &optional text)
  (if (= +tcesuccess+ (tcbdbecode (ptr-of db)))
      nil
    (raise-error db text)))

(defmethod dbm-open ((db tcab-hdb) filename &key write create truncate
                     (lock t) (blocking nil))
  (validate-mode write create truncate lock blocking)
  (let ((mode-flags (combine-mode-flags
                     write create truncate lock blocking
                     +hdboreader+ +hdbowriter+ +hdbocreat+
                     +hdbotrunc+ +hdbonolck+ +hdbolcknb+))
        (db-ptr (ptr-of db)))
      (unless (tchdbopen db-ptr ; opens db by side-effect
                         filename
                         (reduce #'(lambda (x y)
                                     (boole boole-ior x y))
                                 mode-flags))
        (let* ((code (tchdbecode db-ptr))
               (msg (tchdberrmsg code)))
          (tchdbdel db-ptr) ; clean up on error
          (error 'dbm-error :error-code code :error-msg msg))))
  db)

(defmethod dbm-close ((db tcab-hdb))
  (tchdbclose (ptr-of db)))

(defmethod dbm-delete ((db tcab-hdb))
  (tchdbdel (ptr-of db)))

(defmethod dbm-vanish ((db tcab-hdb))
  (tcbdbvanish (ptr-of db)))

(defmethod dbm-put ((db tcab-hdb) (key string) (value string) &key mode)
  (declare (ignore mode))
  (unless (tchdbput2 (ptr-of db) key value)
    (raise-error db (format nil "key: ~a value: ~a" key value))))

(defmethod dbm-get ((db tcab-hdb) (key string))
  (let ((value-ptr))
    (unwind-protect
         (progn
           (setf value-ptr (tchdbget2 (ptr-of db) key))
           (if (null-pointer-p value-ptr)
               (maybe-raise-error db)
             (foreign-string-to-lisp value-ptr)))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-string-free value-ptr)))))

(defmethod dbm-num-records ((db tcab-hdb))
  (tchdbrnum (ptr-of db)))

(defmethod dbm-file-size ((db tcab-hdb))
  (tchdbfsiz (ptr-of db)))

(defmethod dbm-optimize ((db tcab-hdb) &rest args)
  (apply #'tchdboptimize (ptr-of db) args))
