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

(in-package :cl-user)

(asdf:load-system :deoxybyte-systems)

(in-package :uk.co.deoxybyte-systems)

(defsystem cl-tokyo-cabinet
    :name "Common Lisp Tokyo Cabinet"
    :version "0.2.1"
    :author "Keith James"
    :licence "New BSD"
    :depends-on ((:version :cffi "0.10.3"))
    :in-order-to ((test-op (load-op :cl-tokyo-cabinet :cl-tokyo-cabinet-test)))
    :components ((:module :cl-tokyo-cabinet
                          :pathname "src/"
                          :components
                          ((:file "package")
                           (:file "tokyo-cabinet-ffi"
                                  :depends-on ("package"))
                           (:file "tokyo-cabinet"
                                  :depends-on ("package"
                                               "tokyo-cabinet-ffi"))
                           (:file "tokyo-cabinet-bdb"
                                  :depends-on ("package"
                                               "tokyo-cabinet-ffi"
                                               "tokyo-cabinet"))
                           (:file "tokyo-cabinet-hdb"
                                  :depends-on ("package"
                                               "tokyo-cabinet-ffi"
                                               "tokyo-cabinet"))))
                 (:lift-test-config :lift-tests
                                    :pathname "cl-tokyo-cabinet-test"
                                    :target-system :cl-tokyo-cabinet)))
