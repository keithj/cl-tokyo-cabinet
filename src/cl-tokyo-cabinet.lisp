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

;; (declaim (optimize (debug 3) (safety 3)))

(deftype int32 ()
  "The 32bit built-in DBM key type."
  '(signed-byte 32))

(deftype int64 ()
  "The 64bit built-in DBM key type."
  '(signed-byte 64))

(cffi:define-foreign-library libtc
  (t (:default "libtokyocabinet")))

(cffi:use-foreign-library libtc)

(define-condition dbm-error (error)
  ((error-code :initform nil
               :initarg :error-code
               :reader error-code-of)
   (error-msg :initform nil
              :initarg :error-msg
              :reader error-msg-of)
   (text :initform nil
         :initarg :text
         :reader text))
  (:report (lambda (condition stream)
             (format stream "DBM error (~a) ~a~@[: ~a~]."
                     (error-code-of condition)
                     (error-msg-of condition)
                     (text condition)))))

(defclass tc-dbm ()
  ((ptr :initarg :ptr
        :accessor ptr-of
        :documentation "A pointer to a TC native database object."))
  (:documentation "A TC database."))

(defclass tc-bdb (tc-dbm)
  ()
  (:documentation "A TC B+ tree database."))

(defclass tc-hdb (tc-dbm)
  ()
  (:documentation "A TC hash database."))

(defclass tc-iterator ()
  ((ptr :initarg :ptr
        :accessor ptr-of
        :documentation "A pointer to a native TC iterator or
cursor."))
  (:documentation "A TC database iterator."))

(defclass bdb-iterator (tc-iterator)
  ()
  (:documentation "A B+ tree database cursor."))

(defclass hdb-iterator (tc-iterator)
  ()
  (:documentation "A hash database iterator."))

(defgeneric dbm-open (db filespec &rest mode)
  (:documentation "Opens a new, or existing TC database.

Arguments:
- db (object): A TC dbm object.
- filespec (string): A pathname designator for the database file.

Rest:
- mode (list symbol): A list of mode keywords used when opening the
file. The modes are :read :write :create :truncate :noblock :nolock
which correspond to those described in the TC specification.

Returns:
The TC dbm object, now open."))

(defgeneric dbm-close (db)
  (:documentation "Closes an open TC database.

Arguments:
- db (object): A TC dbm object.

Returns:
T on success, or NIL otherwise."))

(defgeneric dbm-delete (db)
  (:documentation "Deletes a TC database. If open, implicitly closes
it first.

Arguments:
- db (object): A TC dbm object.

Returns: 
NIL ."))

(defgeneric dbm-vanish (db))

(defgeneric dbm-begin (db))

(defgeneric dbm-commit (db))

(defgeneric dbm-abort (db))

(defgeneric dbm-put (db key value &key mode))

(defgeneric dbm-get (db key &optional type))

(defgeneric dbm-rem (db key &key remove-dups))

(defgeneric iter-open (db))

(defgeneric iter-close (iter))

(defgeneric iter-first (iter))

(defgeneric iter-last (iter))

(defgeneric iter-prev (iter))

(defgeneric iter-next (iter))

(defgeneric iter-jump (iter key))

(defgeneric iter-get (iter &optional :type))

(defgeneric iter-put (iter value &key mode))

(defgeneric iter-rem (iter))

(defgeneric iter-key (iter &optional :type))

(defgeneric dbm-num-records (db))

(defgeneric dbm-file-namestring (db))

(defgeneric dbm-file-size (db))

(defgeneric dbm-optimize (db &rest args))

(defgeneric dbm-cache (db &rest args))

(defgeneric set-comparator (db fn))

(defgeneric raise-error (db &optional text))

(defgeneric maybe-raise-error (db &optional text))

(defun validate-open-mode (mode)
  (cond ((and (member :create mode)
              (not (member :write mode)))
         (error 'dbm-error
           "The :CREATE argument may not be used in :READ mode"))
        ((and (member :truncate mode)
              (not (member :write mode)))
         (error 'dbm-error
                "The :TRUNCATE argument may not be used in :READ mode"))
        (t t)))

(defun get-string->string (db key fn)
  (let ((value-ptr nil))
    (unwind-protect
         (progn
           (setf value-ptr (funcall fn (ptr-of db) key))
           (if (null-pointer-p value-ptr)
               (maybe-raise-error db (format nil "(key ~a)" key))
             (foreign-string-to-lisp value-ptr)))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-string-free value-ptr)))))

(defun get-string->octets (db key fn)
  "Note that for the key we allocate a foreign string that is not
null-terminated."
  (let ((value-ptr nil))
    (unwind-protect
         (with-foreign-string ((key-ptr key-len) key
                               :null-terminated-p nil)
           (with-foreign-object (size-ptr :int)
             (setf value-ptr (funcall fn (ptr-of db) key-ptr key-len size-ptr))
             (if (null-pointer-p value-ptr)
                 (maybe-raise-error db (format nil "(key ~a)" key))
               (copy-foreign-value value-ptr size-ptr))))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-string-free value-ptr)))))

(defun get-int32->string (db key fn)
  (declare (type int32 key))
  (let ((key-len (foreign-type-size :int32))
        (value-ptr nil))
    (unwind-protect
         (with-foreign-objects ((key-ptr :int32)
                                (size-ptr :int))
           (setf (mem-ref key-ptr :int32) key
                 value-ptr (funcall fn (ptr-of db) key-ptr key-len size-ptr))
           (if (null-pointer-p value-ptr)
               (maybe-raise-error db (format nil "(key ~a)" key))
             (foreign-string-to-lisp value-ptr
                                     :count (mem-ref size-ptr :int))))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-string-free value-ptr)))))

(defun get-int32->octets (db key fn)
  (declare (type int32 key))
  (let ((key-len (foreign-type-size :int32))
        (value-ptr nil))
    (unwind-protect
         (with-foreign-objects ((key-ptr :int32)
                                (size-ptr :int))
           (setf (mem-ref key-ptr :int32) key
                 value-ptr (funcall fn (ptr-of db) key-ptr key-len size-ptr))
           (if (null-pointer-p value-ptr)
               (maybe-raise-error db (format nil "(key ~a)" key))
             (copy-foreign-value value-ptr size-ptr)))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-string-free value-ptr)))))

(defun put-string->string (db key value fn)
  (or (funcall fn (ptr-of db) key value)
      (maybe-raise-error db (format nil "(key ~a) (value ~a)" key value))))

(defun put-string->octets (db key value fn)
  "Note that for the key we allocate a foreign string that is not
null-terminated."
  (declare (type (vector (unsigned-byte 8)) value))
  (let ((value-len (length value)))
    (with-foreign-string ((key-ptr key-len) key :null-terminated-p nil)
      (with-foreign-object (value-ptr :unsigned-char value-len)
        (loop
           for i from 0 below value-len
           do (setf (mem-aref value-ptr :unsigned-char i) (aref value i)))
        (or (funcall fn (ptr-of db) key-ptr key-len value-ptr value-len)
            (maybe-raise-error db (format nil "(key ~a) (value ~a)"
                                          key value)))))))

(defun put-int32->string (db key value fn)
  (declare (type int32 key))
  (let ((key-len (foreign-type-size :int32))
        (value-len (length value)))
    (with-foreign-object (key-ptr :int32)
      (setf (mem-ref key-ptr :int32) key)
      (with-foreign-string (value-ptr value)
        (or (funcall fn (ptr-of db) key-ptr key-len value-ptr value-len)
            (maybe-raise-error db (format nil "(key ~a) (value ~a)"
                                          key value)))))))

(defun put-int32->octets (db key value fn)
  (declare (type int32 key)
           (type (vector (unsigned-byte 8)) value))
  (let ((key-len (foreign-type-size :int32))
        (value-len (length value)))
    (with-foreign-objects ((key-ptr :int32)
                           (value-ptr :unsigned-char value-len))
      (setf (mem-ref key-ptr :int32) key)
      (loop
         for i from 0 below value-len
         do (setf (mem-aref value-ptr :unsigned-char i) (aref value i)))
      (or (funcall fn (ptr-of db) key-ptr key-len value-ptr value-len)
        (maybe-raise-error db (format nil "(key ~a) (value ~a)"
                                      key value))))))

(defun rem-string->value (db key fn)
  (or (funcall fn (ptr-of db) key)
      (maybe-raise-error db (format nil "(key ~a)" key))))

(defun rem-string->duplicates (db key fn)
  (with-foreign-string ((key-ptr key-len) key :null-terminated-p nil)
    (or (funcall fn (ptr-of db) key key-len)
        (maybe-raise-error db (format nil "(key ~a)" key)))))

(defun rem-int32->value (db key fn)
  (declare (type int32 key))
  (with-foreign-object (key-ptr :int32)
    (setf (mem-ref key-ptr :int32) key)
    (or (funcall fn (ptr-of db) key-ptr (foreign-type-size :int32))
        (maybe-raise-error db (format nil "(key ~a)" key)))))

(defun copy-foreign-value (value-ptr size-ptr)
  (let ((size (mem-ref size-ptr :int)))
    (loop
       with value = (make-array size :element-type '(unsigned-byte 8))
       for i from 0 below size
       do (setf (aref value i) (mem-aref value-ptr :unsigned-char i))
       finally (return value))))

                    
;; (defun your-wrapper-around-the-foreign-function (...)
;;   (let ((ptr (your-foreign-function ...)))
;;     (unwind-protect
;;         (foreign-string-to-lisp ptr)
;;       (foreign-funcall "free" :pointer ptr))))

;; (defctype my-string :pointer)

;; (define-type-translator my-string :from-c (value)
;;   "Converts a foreign string to lisp, and frees it."
;;   (once-only (value)
;;     `(unwind-protect (foreign-string-to-lisp ,value)
;;        (foreign-funcall "free" :pointer ptr))))

;; (defcfun your-foreign-function my-string ...)
