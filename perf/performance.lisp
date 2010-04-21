;;;
;;; Copyright (c) 2010, Keith James.
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

(in-package :cl-user)

(defpackage :cl-tokyo-cabinet-perf
  (:use #:common-lisp #:tokyo-cabinet))

(in-package :cl-tokyo-cabinet-perf)

(defun random-string (length &optional (alphabet "abcdefghijklmnopqrstuvwxyz"))
  "Return a pseudo-random string of LENGTH characters."
  (let ((string (make-array length :element-type 'base-char))
        (n (length alphabet)))
    (loop
       for i from 0 below length
       do (setf (char string i) (char alphabet (random n)))
       finally (return string))))

(defun write-random-strings (number length filespec)
  "Write a NUMBER of pseudo-random strings of LENGTH characters to
file denoted by FILESPEC."
  (with-open-file (stream filespec :direction :output :element-type 'base-char
                          :external-format :ascii :if-exists :supersede)
    (dotimes (i number)
      (write-line (random-string length) stream))))

(defmacro with-timing (&body body)
  "Execute BODY, collecting real- and run-time. Returns multiple
values with the last two values being the real-time and run-time
respectively."
  (let ((begin-real (gensym))
        (begin-run (gensym))
        (end-real (gensym))
        (end-run (gensym)))
    `(let ((,begin-real (get-internal-real-time))
           (,begin-run (get-internal-run-time)))
       (let* ((values (reverse (multiple-value-list
                                (progn
                                  ,@body))))
              (,end-real (/ (- (get-internal-real-time) ,begin-real)
                            internal-time-units-per-second))
              (,end-run  (/ (- (get-internal-run-time) ,begin-run)
                            internal-time-units-per-second)))
         (apply #'values 
                (nreverse (cons ,end-run (cons ,end-real values))))))))

(defun bench-insert (db-filename data-filename result-filename
                     &key (size 100000) (count 10) (db-class 'tc-bdb)
                     (key-length 32) (element-type 'base-char)
                     (optimize '(:leaf 512 :non-leaf 256 :bucket-size 10000000
                                 :rec-align 4 :free-pool 10 :opts (:large)))
                     (mode :replace) xmsize)
  "Simple database insert benchmark. Performs SIZE inserts, COUNT
times for a total of SIZE * COUNT inserts. The default is to insert
a pseudo-random 32 byte string as both key and value.

Arguments:

- db-filename (filespec): The database file (will be deleted after
  each run).
- data-filename (filespec): The data file containing key strings (also
  used as values).
- result-filename (filespec): The result file to which timings will be
  written.

Key:

- size (fixnum): The number of records to write in each batch. Each
  batch is wrapped in a transaction.
- count (fixnum): The number of batches to write. Timings are recorded
  for each successive batch.
- db-class (symbol): Symbol naming a TC database class.
- key-length (fixnum): The size of key (and value). Defaults to 32
  bytes.
- element-type (type): The element type of Lisp vectors used. Should
  be 'base-char or '(unsigned-byte 8).
- optimize (list): TC database optimization arguments.
- mode (symbol): TC database insertion mode.
- xmsize (fixnum): TC database extra mapped memory argument.

Returns:

- list of timings."
  (let ((db (make-instance db-class))
        (records (list 0))
        (real-times (list 0))
        (run-times (list 0))
        (buffer (make-array key-length :element-type element-type)))
    (when xmsize
      (dbm-xmsize db xmsize))
    (dbm-open db db-filename :write :create)
    (when optimize
      (apply #'dbm-optimize db optimize))
    (unwind-protect
         (with-open-file (stream data-filename :element-type element-type)
           (flet ((insert-n-records (n)
                    (with-transaction (db)
                      (loop
                         for i from 0 below n
                         for k = (read-sequence buffer stream)
                         while (plusp k)
                         do (dbm-put db buffer buffer :mode mode)
                         finally (return i)))))
             (let ((total 0))
               (dotimes (i count)
                 (multiple-value-bind (n real run)
                     (with-timing (insert-n-records size))
                   (incf total n)
                   (format t "Inserted ~d records, total ~d~%" n total)
                   (push total records)
                   (push real real-times)
                   (push run run-times))))))
      (dbm-close db)
      (delete-file db-filename))
    (with-open-file (results result-filename :direction :output)
      (let ((*print-pretty* nil))
        (format results
                "# ~a, element-type: ~a, optimize? ~a mode: ~a xmsize: ~a~%"
                db-class element-type optimize mode xmsize))
      (mapcar (lambda (x)
                (apply #'format results "~d ~,4f ~,4f~%" x)
                x)
              (nreverse (mapcar (lambda (rec real run)
                                  (list rec real run))
                                records real-times run-times))))))
