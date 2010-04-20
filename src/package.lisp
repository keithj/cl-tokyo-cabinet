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

(defpackage :tokyo-cabinet-ffi
  (:use #:common-lisp #:cffi)
  (:export
   ;; Constants
   #:+tcesuccess+
   #:+tcethread+
   #:+tceinvalid+
   #:+tcenofile+
   #:+tcenoperm+
   #:+tcemeta+
   #:+tcerhead+
   #:+tceopen+
   #:+tceclose+
   #:+tcetrunc+
   #:+tcesync+
   #:+tcestat+
   #:+tceseek+
   #:+tceread+
   #:+tcewrite+
   #:+tcemmap+
   #:+tcelock+
   #:+tceunlink+
   #:+tcerename+
   #:+tcemkdir+
   #:+tcermdir+
   #:+tcekeep+
   #:+tcenorec+
   #:+tcemisc+
   #:+bdbcpcurrent+
   #:+bdbcpbefore+
   #:+bdbcpafter+

   #:bdb-open-flags
   #:hdb-open-flags
   #:bdb-options
   #:hdb-options
   ;; Variables

   ;; Functions
   #:tcbdberrmsg
   #:tcbdbnew
   #:tcbdbdel
   #:tcbdbecode
   #:tcbdbsetcmpfunc
   #:tcbdbtune
   #:tcbdbsetcache
   #:tcbdbsetxmsiz
   #:tcbdbopen
   #:tcbdbclose
   #:tcbdbput
   #:tcbdbput2
   #:tcbdbputkeep
   #:tcbdbputkeep2
   #:tcbdbputcat
   #:tcbdbputcat2
   #:tcbdbputdup
   #:tcbdbputdup2
   #:tcbdbputdup3
   #:tcbdbout
   #:tcbdbout2
   #:tcbdbout3
   #:tcbdbget
   #:tcbdbget2

   #:tcbdbvnum
   #:tcbdbvnum2
   #:tcbdbvsiz
   #:tcbdbvsiz2

   #:tcbdbsync
   #:tcbdboptimize
   #:tcbdbvanish
   #:tcbdbcopy
   #:tcbdbtranbegin
   #:tcbdbtrancommit
   #:tcbdbtranabort
   #:tcbdbpath   
   #:tcbdbrnum
   #:tcbdbfsiz
   #:tcbdbcurnew
   #:tcbdbcurdel
   #:tcbdbcurfirst
   #:tcbdbcurlast
   #:tcbdbcurjump
   #:tcbdbcurjump2
   #:tcbdbcurprev
   #:tcbdbcurnext
   #:tcbdbcurput
   #:tcbdbcurput2
   #:tcbdbcurout
   #:tcbdbcurkey
   #:tcbdbcurkey2
   #:tcbdbcurkey3
   #:tcbdbcurval
   #:tcbdbcurval2
   #:tcbdbcurval3
   #:tcbdbcurrec
   
   #:tchdberrmsg
   #:tchdbnew
   #:tchdbdel
   #:tchdbecode

   #:tchdbtune
   #:tchdbsetcache
   #:tchdbsetxmsiz
   #:tchdbopen
   #:tchdbclose
   #:tchdbput
   #:tchdbput2
   #:tchdbputkeep
   #:tchdbputkeep2
   #:tchdbputcat
   #:tchdbputcat2
   #:tchdbputasync
   #:tchdbputasync2
   #:tchdbout
   #:tchdbout2
   #:tchdbget
   #:tchdbget2

   #:tchdbvsiz
   #:tchdbvsiz2
   #:tchdbiterinit
   #:tchdbiternext
   #:tchdbiternext2
   #:tchdbiternext3

   #:tchdbsync
   #:tchdboptimize
   #:tchdbvanish
   #:tchdbcopy
   #:tchdbtranbegin
   #:tchdbtrancommit
   #:tchdbtranabort

   #:tchdbpath
   #:tchdbrnum
   #:tchdbfsiz))

(defpackage :tokyo-cabinet
  (:use #:common-lisp #:cffi #:tokyo-cabinet-ffi)
  (:nicknames #:tc)
  (:export
   ;; Constants

   ;; Variables

   ;; Classes
   #:tc-bdb
   #:tc-hdb

   ;; Conditions
   #:dbm-error

   ;; Generics
   #:dbm-open
   #:dbm-close
   #:dbm-begin
   #:dbm-commit
   #:dbm-delete
   #:dbm-vanish
   #:dbm-put
   #:dbm-get
   #:dbm-rem
   #:iter-open
   #:iter-first
   #:iter-last
   #:iter-prev
   #:iter-next
   #:iter-jump
   #:iter-put
   #:iter-rem
   #:iter-key
   #:iter-get
   #:iter-close
   
   #:dbm-num-records
   #:dbm-file-size
   #:dbm-optimize
   #:dbm-cache
   #:dbm-xmsize
   #:set-comparator
   
   ;; Functions

   ;; Macros
   #:with-database
   #:with-transaction
   #:with-iterator))
