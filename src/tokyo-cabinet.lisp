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

(deftype int32 ()
  "The 32bit built-in DBM key type."
  '(signed-byte 32))

(deftype int64 ()
  "The 64bit built-in DBM key type."
  '(signed-byte 64))

(defparameter *in-transaction-p* nil
  "Bound when in a transaction.")

(define-condition dbm-error (error)
  ((error-code :initform nil
               :initarg :error-code
               :reader error-code-of
               :documentation "The error code provided by TC.")
   (error-msg :initform nil
              :initarg :error-msg
              :reader error-msg-of
              :documentation "The error message provided by TC.")
   (text :initform nil
         :initarg :text
         :reader text
         :documentation "Any additional message provided by the CL API."))
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
        :documentation "A TC pointer."))
  (:documentation "A TC database iterator, the superclass of both B+
tree cursors and hash iterators."))

(defclass bdb-iterator (tc-iterator)
  ()
  (:documentation "A B+ tree database cursor."))

(defclass hdb-iterator (tc-iterator)
  ((next-key :accessor next-key-of)
   (key-size :accessor key-size-of))
  (:documentation "A hash database iterator."))

(defgeneric dbm-open (db filespec &rest mode)
  (:documentation "Opens a new, or existing TC database.

Arguments:

- db (object): A TC dbm object.
- filespec (string): A pathname designator for the database file.

Rest:

- mode (list symbol): A list of mode keywords used when opening the
file. The modes are :READ :WRITE :CREATE :TRUNCATE :NOBLOCK :NOLOCK
and :TSYNC which correspond to those described in the TC
specification.

Returns:

- The TC dbm object, now open."))

(defgeneric dbm-close (db)
  (:documentation "Closes an open TC database.

Arguments:

- db (object): A TC dbm object.

Returns:

- T on success, or NIL otherwise."))

(defgeneric dbm-delete (db)
  (:documentation "Deletes a TC database. If open, implicitly closes
it first.

Arguments:

- db (object): A TC dbm object.

Returns: 

- NIL ."))

(defgeneric dbm-vanish (db)
  (:documentation "Removes all records from DB."))

(defgeneric dbm-begin (db)
  (:documentation "Begins a transaction with DB."))

(defgeneric dbm-commit (db)
  (:documentation "Commits a transaction with DB."))

(defgeneric dbm-abort (db)
  (:documentation "Aborts a transaction with DB."))

(defgeneric dbm-put (db key value &key mode)
  (:documentation "Inserts KEY and VALUE into DB. MODE varies with DB
class.

Arguments:

- db (object): A TC database object.
- key (object): A key under which to insert.
- value (object): A value to insert under key.

Key:

- :mode (symbol): A symbol designating one of the TC insertion modes:
replace, keep, concat, duplicate, async etc.

Valid modes for B+ tree databases are:

- :REPLACE : If a record with the same key exists in the database, it
is overwritten.
- :KEEP : If a record with the same key exists in the database, this
function has no effect.
- :CONCAT : Concatenates a value at the end of the existing record in
the database. If there is no corresponding record, a new record is
created.
- :DUPLICATE : Adds a record to the database, allowing duplication of
keys. If a record with the same key exists in the database, the new
record is placed after the existing one.

Valid modes for hash databases are:

:REPLACE , :KEEP and :CONCAT , as above. Additionally:

- :ASYNC : Collects inserts into a buffer and then inserts them
asynchronously, in one operation."))

(defgeneric dbm-get (db key &optional type)
  (:documentation "Returns the value under KEY in DB. Type may be one
of :STRING or :OCTETS , depending on how the value is to be
treated. :STRING indicates that the value should be converted to a
Lisp string, while :OCTETS indicates that the byte vector should be
returned."))

(defgeneric dbm-rem (db key &key remove-dups)
  (:documentation "Removes the value under KEY in DB. If REMOVE-DUPS
is T, duplicate values will be removed from a B+ tree database."))

(defgeneric iter-open (db)
  (:documentation "Opens an iterator on DB.

Arguments:

- db (object): A TC dbm object.

Returns:
 - A TC iterator object."))

(defgeneric iter-close (iterator)
  (:documentation "Closes ITERATOR. Only effective for B+ tree
databases."))

(defgeneric iter-first (iterator)
  (:documentation "Moves ITERATOR to the first record and returns T,
or NIL if the database is empty. Only effective for B+ tree
databases."))

(defgeneric iter-last (iterator)
  (:documentation "Moves ITERATOR to the last record and returns T, or
NIL if the database is empty. Only effective for B+ tree databases."))

(defgeneric iter-prev (iterator)
  (:documentation "Moves ITERATOR to the previous record and returns
T, or NIL if already at the first record. Only effective for B+ tree
databases."))

(defgeneric iter-next (iterator)
  (:documentation "Moves ITERATOR to the next record and returns T, or
NIL if already at the last record."))

(defgeneric iter-jump (iterator key)
  (:documentation "Moves ITERATOR to the record at KEY. Only effective
for B+ tree databases."))

(defgeneric iter-get (iterator &optional type)
  (:documentation "Returns the current value at ITERATOR. Type may be
one of :STRING or :OCTETS , depending on how the value is to be
treated. :STRING indicates that the value should be converted to a
Lisp string, while :OCTETS indicates that the byte vector should be
returned."))

(defgeneric iter-put (iterator value &key mode)
  (:documentation "Inserts VALUE around ITERATOR. Mode may be one
of :CURRENT , :BEFORE or :AFTER . Only effective for B+ tree
databases."))

(defgeneric iter-rem (iterator)
  (:documentation "Removed the record at the ITERATOR position and
advances ITERATOR, if possible. Only effective for B+ tree
databases."))

(defgeneric iter-key (iterator &optional type)
  (:documentation "Returns current key at the ITERATOR position. Type
may be one of :STRING or :OCTETS , depending on how the value is to be
treated. :STRING indicates that the value should be converted to a
Lisp string, while :OCTETS indicates that the byte vector should be
returned."))

(defgeneric dbm-num-records (db)
  (:documentation "Returns the number of records in DB."))

(defgeneric dbm-file-namestring (db)
  (:documentation "Returns the name of the DB file."))

(defgeneric dbm-file-size (db)
  (:documentation "Returns the size of the DB file in bytes."))

(defgeneric dbm-optimize (db &rest args)
  (:documentation "Sets the DB optimization parameters on an open
database. These are described in the TC documentation.

The keyword arguments for B+ tree databases are:

- :LEAF (fixnum) : Sets the number of leaf nodes.
- :NON-LEAF (fixnum) : Sets the number of non-leaf nodes.
- :BUCKET-SIZE (fixnum) : Sets the bucket
- :REC-ALIGN (fixnum) : A power of 2 indicating record alignment.
- :FREE-POOL (fixnum): A power of 2 indicating the size of the free
record pool.

- :OPTS (list symbol): A list of keywords indicating optional database
parameters.

The keyword arguments for hash databases are:

:BUCKET-SIZE :REC-ALIGN :FREE-POOL and :OPTS , as above.

In both cases the :OPTS value is a list of one or more of

- :LARGE : Use a 64-bit bucket array to allow datbases > 2Gb.
- :DEFLATE : Use deflate compression.
- :BZIP : Use bzip compression.
- :TCBS : Use tcbs compression.
- :DEFAULTS : Use the current settings.

For example:

;;; (dbm-optimize db :leaf 512 :non-leaf 256 :bucket-size 100000000
;;;                  :rec-align 4 :free-pool 10 :opts '(:large :deflate))"))

(defgeneric dbm-cache (db &rest args)
  (:documentation "Sets the caching parameters of DB. These are
described in the TC documentation.

The keyword arguments for B+ tree databases are:

- :LEAF : The number of leaf nodes to cache. Defaults to 1024.
- :NON-LEAF : The number of non-leaf nodes to cache. Defaults to 512.

The keyword arguments for hash databases are:

- :RECORDS : The number of records to cache. Defaults to 0."))

(defgeneric dbm-xmsize (db size)
  (:documentation "Sets the DB extra mapped memory to SIZE bytes."))

(defgeneric set-comparator (db fn)
  (:documentation "Sets the DB comparator function to that given by
symbol FN."))

(defgeneric raise-error (db &optional message &rest message-arguments)
  (:documentation "Raises a {define-condition dbm-error} with
MESSAGE. MESSAGE may be a format template, in which case the rest of
the arguments are taken to be format arguments for that template. The
error code and TC error message are automatically obtained from the DB
handle."))

(defgeneric maybe-raise-error (db &optional message &rest message-arguments)
  (:documentation "Checks the DB handle for any error reported by TC
and raises a {define-condition dbm-error} if one has occurred by
calling {defun raise-error} ."))

(defmacro with-database ((var filespec type &rest mode) &body body)
  "Evaluates BODY with VAR bound to an open database.

Arguments:

- var (symbol): The binding for the new database object.
- filespec (filespec): The database file.
- type (symbol): A symbol that names a TC database class.

Rest:

- mode (symbols): :READ :WRITE :CREATE :TRUNCATE
                  :NOLOCK :NOBLOCK and :TSYNC

See the TC documentation for the meaning of the mode arguments."
  `(let ((,var (make-instance ,type)))
     (unwind-protect
          (progn
            (dbm-open ,var ,filespec ,@mode)
            ,@body)
       (when ,var
         (dbm-close ,var)))))

(defmacro with-transaction ((db) &body body)
  "Evaluates BODY in the context of a transaction on DB. If no
transaction is in progress, a new one is started. If a transaction is
already in progress, BODY is evaluated in its context. If an error
occurs, the transaction will rollback, otherwise it will commit."
  (let ((success (gensym)))
    `(let ((,success nil))
       (flet ((atomic-op ()
                ,@body))
         (if *in-transaction-p*
             (atomic-op)
           (unwind-protect
                (let ((*in-transaction-p* t))
                  (prog2
                      (dbm-begin ,db)
                      (atomic-op)
                    (setf ,success t)))
             (if ,success
                 (dbm-commit ,db)
               (dbm-abort ,db))))))))

(defmacro with-iterator ((var db) &body body)
  "Evaluates BODY on with VAR bound to a new, open iterator on DB."
  `(let ((,var (iter-open ,db)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var
         (iter-close ,var)))))

(defmacro with-string-value ((value-ptr initform) &body body)
  "Helper macro for managing string values."
  `(let ((,value-ptr ,initform))
     (unwind-protect
          (progn
            ,@body)
       (when (and value-ptr (not (null-pointer-p ,value-ptr)))
         (foreign-string-free ,value-ptr)))))

(defun check-open-mode (mode)
  "Checks the list MODE for valid and consistent database opening
arguments."
  (cond ((and (member :create mode)
              (not (member :write mode)))
         (error 'dbm-error
                :text "The :CREATE argument may not be used in :READ mode"))
        ((and (member :truncate mode)
              (not (member :write mode)))
         (error 'dbm-error
                :text "The :TRUNCATE argument may not be used in :READ mode"))
        (t t)))

(defun get-string->string (db key fn)
  "Returns a value from DB under KEY using FN where the key and value
are strings."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (with-string-value (value-ptr (funcall fn (ptr-of db) key))
    (if (null-pointer-p value-ptr)
        (maybe-raise-error db "(key ~a)" key)
      (foreign-string-to-lisp value-ptr))))

(defun get-string->octets (db key fn)
  "Returns a value from DB under KEY using FN where the key is a
string and the value an octet vector. Note that for the key we
allocate a foreign string that is not null-terminated."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (with-foreign-string ((key-ptr key-len) key :null-terminated-p nil)
    (with-foreign-object (size-ptr :int)
      (with-string-value (value-ptr (funcall fn (ptr-of db)
                                             key-ptr key-len size-ptr))
        (if (null-pointer-p value-ptr)
            (maybe-raise-error db "(key ~a)" key)
          (copy-foreign-value value-ptr size-ptr))))))

;; KTR
(defun get-octets->octets (db key fn)
  "Returns a value from DB under KEY using FN where the key and value
are octet vectors."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (unsigned-byte 8)) key)
           (type function fn))
  (let ((key-len (length key)))
    (with-foreign-object (key-ptr :unsigned-char key-len)
      (loop
	 for i from 0 below key-len
	 do (setf (mem-aref key-ptr :unsigned-char i) (aref key i)))
      (with-foreign-object (size-ptr :int)
	(with-string-value (value-ptr (funcall fn (ptr-of db)
					       key-ptr key-len size-ptr))
	  (if (null-pointer-p value-ptr)
	      (maybe-raise-error db "(key ~a)" key)
	      (copy-foreign-value value-ptr size-ptr)))))))

(defun get-octets->string (db key fn)
  "Returns a value from DB under KEY using FN where the key is a
vector of octets and value is a string."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (unsigned-byte 8)) key)
           (type function fn))
  (let ((key-len (length key)))
    (with-foreign-object (key-ptr :unsigned-char key-len)
      (loop
	 for i from 0 below key-len
	 do (setf (mem-aref key-ptr :unsigned-char i) (aref key i)))
      (with-foreign-object (size-ptr :int)
	(with-string-value (value-ptr (funcall fn (ptr-of db)
					       key-ptr key-len size-ptr))
	  (if (null-pointer-p value-ptr)
	      (maybe-raise-error db "(key ~a)" key)
	      (foreign-string-to-lisp value-ptr
				      :count (mem-ref size-ptr :int))))))))
;; KTR

(defun get-int32->string (db key fn)
  "Returns a value from DB under KEY using FN where the key is a
32-but integer and the value a string."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (let ((key-len (foreign-type-size :int32)))
    (with-foreign-objects ((key-ptr :int32)
                           (size-ptr :int))
      (setf (mem-ref key-ptr :int32) key)
      (with-string-value (value-ptr (funcall fn (ptr-of db)
                                             key-ptr key-len size-ptr))
        (if (null-pointer-p value-ptr)
            (maybe-raise-error db "(key ~a)" key)
          (foreign-string-to-lisp value-ptr
                                  :count (mem-ref size-ptr :int)))))))

(defun get-int32->octets (db key fn)
  "Returns a value from DB under KEY using FN where the key is a
32-bit integer and the value an octet vector."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (let ((key-len (foreign-type-size :int32))
        (value-ptr nil))
    (unwind-protect
         (with-foreign-objects ((key-ptr :int32)
                                (size-ptr :int))
           (setf (mem-ref key-ptr :int32) key
                 value-ptr (funcall fn (ptr-of db) key-ptr key-len size-ptr))
           (if (null-pointer-p value-ptr)
               (maybe-raise-error db "(key ~a)" key)
             (copy-foreign-value value-ptr size-ptr)))
      (when (and value-ptr (not (null-pointer-p value-ptr)))
        (foreign-free value-ptr)))))

(defun put-string->string (db key value fn)
  "Inserts VALUE into DB under KEY using FN where the key and value
are strings."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (or (funcall fn (ptr-of db) key value)
      (maybe-raise-error db "(key ~a) (value ~a)" key value)))

(defun put-string->octets (db key value fn)
  "Inserts VALUE into DB under KEY using FN where the key is a string
and the value an octet vector. Note that for the key we allocate a
foreign string that is not null-terminated."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (unsigned-byte 8)) value)
           (type function fn))
  (let ((value-len (length value)))
    (with-foreign-string ((key-ptr key-len) key :null-terminated-p nil)
      (with-foreign-object (value-ptr :unsigned-char value-len)
        (loop
           for i from 0 below value-len
           do (setf (mem-aref value-ptr :unsigned-char i) (aref value i)))
        (or (funcall fn (ptr-of db) key-ptr key-len value-ptr value-len)
            (maybe-raise-error db "(key ~a) (value ~a)" key value))))))

;; KTR
(defun put-octets->string (db key value fn)
  "Inserts VALUE into DB under KEY using FN where the key and value
are octet vectors."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (unsigned-byte 8)) key)
           (type function fn))
  (let ((key-len (length key)))
    (with-foreign-object (key-ptr :unsigned-char key-len)
      (with-foreign-string ((value-ptr value-len) value :null-terminated-p nil)
	(loop
	   for i from 0 below key-len
	   do (setf (mem-aref key-ptr :unsigned-char i) (aref key i)))
	(or (funcall fn (ptr-of db) key-ptr key-len value-ptr value-len)
	    (maybe-raise-error db "(key ~a) (value ~a)" key value))))))
;; KTR
    
(defun put-octets->octets (db key value fn)
  "Inserts VALUE into DB under KEY using FN where the key and value
are octet vectors."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (unsigned-byte 8)) key value)
           (type function fn))
  (let ((key-len (length key))
        (value-len (length value)))
    (with-foreign-object (key-ptr :unsigned-char key-len)
      (with-foreign-object (value-ptr :unsigned-char value-len)
        (loop
           for i from 0 below key-len
           do (setf (mem-aref key-ptr :unsigned-char i) (aref key i)))
        (loop
           for i from 0 below value-len
           do (setf (mem-aref value-ptr :unsigned-char i) (aref value i)))
        (or (funcall fn (ptr-of db) key-ptr key-len value-ptr value-len)
            (maybe-raise-error db "(key ~a) (value ~a)" key value))))))

(defun put-int32->string (db key value fn)
  "Inserts VALUE into DB under KEY using FN where the key is a 32-bit
integer and the value is a string."
  (declare (optimize (speed 3)))
  (declare (type simple-string value)
           (type function fn))
  (let ((key-len (foreign-type-size :int32))
        (value-len (length value)))
    (with-foreign-object (key-ptr :int32)
      (setf (mem-ref key-ptr :int32) key)
      (with-foreign-string (value-ptr value)
        (or (funcall fn (ptr-of db) key-ptr key-len value-ptr value-len)
            (maybe-raise-error db "(key ~a) (value ~a)" key value))))))

(defun put-int32->octets (db key value fn)
  "Inserts VALUE into DB under KEY using FN where the key is a 32-bit
integer and the value is an octet vector."
  (declare (optimize (speed 3)))
  (declare (type (simple-array (unsigned-byte 8)) value)
           (type function fn))
  (let ((key-len (foreign-type-size :int32))
        (value-len (length value)))
    (with-foreign-objects ((key-ptr :int32)
                           (value-ptr :unsigned-char value-len))
      (setf (mem-ref key-ptr :int32) key)
      (loop
         for i from 0 below value-len
         do (setf (mem-aref value-ptr :unsigned-char i) (aref value i)))
      (or (funcall fn (ptr-of db) key-ptr key-len value-ptr value-len)
        (maybe-raise-error db "(key ~a) (value ~a)" key value)))))

(defun rem-string->value (db key fn)
  "Removes value from DB under KEY using FN where the key is a
string."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (or (funcall fn (ptr-of db) key)
      (maybe-raise-error db "(key ~a)" key)))

(defun rem-string->duplicates (db key fn)
  "Removes all values from DB under KEY using FN where the key is a
string."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (with-foreign-string ((key-ptr key-len) key :null-terminated-p nil)
    (or (funcall fn (ptr-of db) key key-len)
        (maybe-raise-error db "(key ~a)" key))))

(defun rem-int32->value (db key fn)
  "Removes value from DB under KEY using FN where the key is a 32-bit
integer."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (with-foreign-object (key-ptr :int32)
    (setf (mem-ref key-ptr :int32) key)
    (or (funcall fn (ptr-of db) key-ptr (foreign-type-size :int32))
        (maybe-raise-error db "(key ~a)" key))))

;; KTR
(defun rem-octets->value (db key fn)
  "Removes value from DB under KEY using FN where the key is a vector
of octets."
  (declare (optimize (speed 3)))
  (declare (type function fn))
  (declare (type (vector (unsigned-byte 8)) key))
  (let ((key-len (length key)))
    (with-foreign-object (key-ptr :unsigned-char key-len)
      (loop
	 for i from 0 below key-len
	 do (setf (mem-aref key-ptr :unsigned-char i) (aref key i)))
      (or (funcall fn (ptr-of db) key-ptr key-len)
	  (maybe-raise-error db "(key ~a)" key)))))
;; KTR

(declaim (inline copy-foreign-value))
(defun copy-foreign-value (value-ptr size-ptr)
  (declare (optimize (speed 3)))
  (let ((size (mem-ref size-ptr :int)))
    (loop
       with value = (make-array size :element-type '(unsigned-byte 8))
       for i from 0 below size
       do (setf (aref value i) (mem-aref value-ptr :unsigned-char i))
       finally (return value))))
