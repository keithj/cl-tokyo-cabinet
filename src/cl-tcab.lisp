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

(deftype int32 ()
  "The 32bit built-in DBM key type."
  '(signed-byte 32))

(deftype int64 ()
  "The 64bit built-in DBM key type."
  '(signed-byte 64))

(cffi:define-foreign-library libtcab
  (t (:default "libtokyocabinet")))

(cffi:use-foreign-library libtcab)

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
             (format stream "DBM error (~a): ~a~@[: ~a~]."
                     (error-code-of condition)
                     (error-msg-of condition)
                     (text condition)))))

(defclass tcab-dbm ()
  ((ptr :initarg :ptr
        :accessor ptr-of)))

(defclass tcab-bdb (tcab-dbm)
  ())

(defclass tcab-hdb (tcab-dbm)
  ())

(defgeneric dbm-open (name db-type
                      &key write create truncate lock blocking))

(defgeneric dbm-close (db))

(defgeneric dbm-delete (db))

(defgeneric dbm-vanish (db))

(defgeneric dbm-begin (db))

(defgeneric dbm-commit (db))

(defgeneric dbm-abort (db))

(defgeneric dbm-put (db key value &key mode))

(defgeneric dbm-get (db key))

(defgeneric dbm-iterator (db))

(defgeneric dbm-num-records (db))

(defgeneric dbm-file-size (db))

(defgeneric dbm-optimize (db &rest args))

(defgeneric raise-error (db &optional text))

(defgeneric maybe-raise-error (db &optional text))

(defun validate-mode (write create truncate lock blocking)
  (when (and create (not write))
    (error 'dbm-error
           "The CREATE argument may not be used in READ mode"))
  (when (and truncate (not write))
    (error 'dbm-error
           "The TRUNCATE argument may not be used in READ mode"))
  (when (and blocking (not lock))
    (error 'dbm-error
           "The BLOCKING argument may not be used without the LOCK argument")))

(defun combine-mode-flags (write create truncate lock blocking
                           read-flag write-flag create-flag
                           truncate-flag nolock-flag noblock-flag)
  (let ((mode-flags (if write
                        (list write-flag)
                      (list read-flag))))
    (when create (push create-flag mode-flags))
    (when truncate (push truncate-flag mode-flags))
    (when (not lock) (push nolock-flag mode-flags))
    (when (not blocking) (push noblock-flag mode-flags))))




;; (defclass qdbm-iterator ()
;;   ((qdbm-handle :initarg :handle
;;                 :reader handle-of)
;;    (next-key-ptr :initarg :next-key
;;                  :accessor next-key-of)))

;; (defmethod qdbm-iterator ((handle qdbm-depot-handle))
;;   (if (= 1 (dpiterinit (depot-of handle)))
;;       (make-instance 'qdbm-iterator :handle handle)
;;     (error 'qdbm-error :text (last-qdbm-error-msg))))

;; (defmethod qdbm-iterator-next ((iterator qdbm-iterator))
;;   )

;; (defmethod qdbm-iterator-next ((handle qdbm-depot-handle))
;;   (let ((key-size-ptr (foreign-alloc :int)))
;;     (unwind-protect
;;          (let ((key-ptr (dpiternext (depot-of handle) key-size-ptr)))
;;            (cond ((null-pointer-p key-size-ptr)
;;                   nil)
;;                  ((null-pointer-p key-ptr)
;;                   nil)
;;                  (t
;;                   (let ((key-size (mem-ref key-ptr :int))))))))))
                    
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