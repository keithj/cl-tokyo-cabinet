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

(in-package :tokyo-cabinet-ffi)

(define-foreign-library libtc
  (t (:default "libtokyocabinet")))

(use-foreign-library libtc)

(defconstant +tcesuccess+ 0)
(defconstant +tcethread+ 1)
(defconstant +tceinvalid+ 2)
(defconstant +tcenofile+ 3)
(defconstant +tcenoperm+ 4)
(defconstant +tcemeta+ 5)
(defconstant +tcerhead+ 6)
(defconstant +tceopen+ 7)
(defconstant +tceclose+ 8)
(defconstant +tcetrunc+ 9)
(defconstant +tcesync+ 10)
(defconstant +tcestat+ 11)
(defconstant +tceseek+ 12)
(defconstant +tceread+ 13)
(defconstant +tcewrite+ 14)
(defconstant +tcemmap+ 15)
(defconstant +tcelock+ 16)
(defconstant +tceunlink+ 17)
(defconstant +tcerename+ 18)
(defconstant +tcemkdir+ 19)
(defconstant +tcermdir+ 20)
(defconstant +tcekeep+ 21)
(defconstant +tcenorec+ 22)
(defconstant +tcemisc+ 9999)

(defconstant +bdbcpcurrent+ 0)
(defconstant +bdbcpbefore+ 1)
(defconstant +bdbcpafter+ 2)

(defbitfield bdb-open-flags
  :read
  :write
  :create
  :truncate
  :nolock
  :noblock
  :tsync)

(defbitfield hdb-open-flags
  :read
  :write
  :create
  :truncate
  :nolock
  :noblock
  :tsync)

(defbitfield bdb-options
  :large
  :deflate
  :bzip
  :tcbs
  (:defaults #xff))

(defbitfield hdb-options
  :large
  :deflate
  :bzip
  :tcbs
  (:defaults #xff))

(defcfun ("tcbdberrmsg" tcbdberrmsg) :string
  (ecode :int))

(defcfun ("tcbdbnew" tcbdbnew) :pointer)

(defcfun ("tcbdbdel" tcbdbdel) :void
  (bdb :pointer))

(defcfun ("tcbdbecode" tcbdbecode) :int
  (bdb :pointer))

;; (defcfun ("tcbdbsetmutex" tcbdbsetmutex) :pointer
;;   (bdb :pointer))

(defcfun ("tcbdbsetcmpfunc" tcbdbsetcmpfunc) :boolean
  (bdb :pointer)
  (cmp :pointer)
  (cmpop :pointer))

(defcfun ("tcbdbtune" tcbdbtune) :boolean
  (bdb :pointer)
  (lmemb :int32)
  (nmemb :int32)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts bdb-options))

(defcfun ("tcbdbsetcache" tcbdbsetcache) :pointer
  (bdb :pointer)
  (lcnum :int32)
  (ncnum :int32))

(defcfun ("tcbdbsetxmsiz" tcbdbsetxmsiz) :boolean
  (bdb :pointer)
  (xmsiz :int64))

(defcfun ("tcbdbopen" tcbdbopen) :boolean
  (bdb :pointer)
  (path :string)
  (mode bdb-open-flags))

(defcfun ("tcbdbclose" tcbdbclose) :boolean
  (bdb :pointer))

(defcfun ("tcbdbput" tcbdbput) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(defcfun ("tcbdbput2" tcbdbput2) :boolean
  (bdb :pointer)
  (kstr :string)
  (vstr :string))

(defcfun ("tcbdbputkeep" tcbdbputkeep) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(defcfun ("tcbdbputkeep2" tcbdbputkeep2) :boolean
  (bdb :pointer)
  (kstr :string)
  (vstr :string))

(defcfun ("tcbdbputcat" tcbdbputcat) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(defcfun ("tcbdbputcat2" tcbdbputcat2) :boolean
  (bdb :pointer)
  (kstr :string)
  (vstr :string))

(defcfun ("tcbdbputdup" tcbdbputdup) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(defcfun ("tcbdbputdup2" tcbdbputdup2) :boolean
  (bdb :pointer)
  (kstr :string)
  (vstr :string))

(defcfun ("tcbdbputdup3" tcbdbputdup3) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vals :pointer))

(defcfun ("tcbdbout" tcbdbout) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(defcfun ("tcbdbout2" tcbdbout2) :boolean
  (bdb :pointer)
  (kstr :string))

(defcfun ("tcbdbout3" tcbdbout3) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(defcfun ("tcbdbget" tcbdbget) :pointer
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (sp :pointer))

(defcfun ("tcbdbget2" tcbdbget2) :pointer
  (bdb :pointer)
  (kstr :string))

;; (defcfun ("tcbdbget3" tcbdbget3) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (sp :pointer))

;; (defcfun ("tcbdbget4" tcbdbget4) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

(defcfun ("tcbdbvnum" tcbdbvnum) :int
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(defcfun ("tcbdbvnum2" tcbdbvnum2) :int
  (bdb :pointer)
  (kstr :string))

(defcfun ("tcbdbvsiz" tcbdbvsiz) :int
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(defcfun ("tcbdbvsiz2" tcbdbvsiz2) :int
  (bdb :pointer)
  (kstr :string))

;; (defcfun ("tcbdbrange" tcbdbrange) :pointer
;;   (bdb :pointer)
;;   (bkbuf :pointer)
;;   (bksiz :int)
;;   (binc :pointer)
;;   (ekbuf :pointer)
;;   (eksiz :int)
;;   (einc :pointer)
;;   (max :int))

;; (defcfun ("tcbdbrange2" tcbdbrange2) :pointer
;;   (bdb :pointer)
;;   (bkstr :string)
;;   (binc :pointer)
;;   (ekstr :string)
;;   (einc :pointer)
;;   (max :int))

;; (defcfun ("tcbdbfwmkeys" tcbdbfwmkeys) :pointer
;;   (bdb :pointer)
;;   (pbuf :pointer)
;;   (psiz :int)
;;   (max :int))

;; (defcfun ("tcbdbfwmkeys2" tcbdbfwmkeys2) :pointer
;;   (bdb :pointer)
;;   (pstr :string)
;;   (max :int))

(defcfun ("tcbdbsync" tcbdbsync) :boolean
  (bdb :pointer))

(defcfun ("tcbdboptimize" tcbdboptimize) :boolean
  (bdb :pointer)
  (lmemb :int32)
  (nmemb :int32)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts bdb-options))

(defcfun ("tcbdbvanish" tcbdbvanish) :boolean
  (bdb :pointer))

(defcfun ("tcbdbcopy" tcbdbcopy) :pointer
  (bdb :pointer)
  (path :string))

(defcfun ("tcbdbtranbegin" tcbdbtranbegin) :boolean
  (bdb :pointer))

(defcfun ("tcbdbtrancommit" tcbdbtrancommit) :boolean
  (bdb :pointer))

(defcfun ("tcbdbtranabort" tcbdbtranabort) :boolean
  (bdb :pointer))

(defcfun ("tcbdbpath" tcbdbpath) :string
  (bdb :pointer))

(defcfun ("tcbdbrnum" tcbdbrnum) :uint64
  (bdb :pointer))

(defcfun ("tcbdbfsiz" tcbdbfsiz) :uint64
  (bdb :pointer))

(defcfun ("tcbdbcurnew" tcbdbcurnew) :pointer
  (bdb :pointer))

(defcfun ("tcbdbcurdel" tcbdbcurdel) :void
  (cur :pointer))

(defcfun ("tcbdbcurfirst" tcbdbcurfirst) :boolean
  (cur :pointer))

(defcfun ("tcbdbcurlast" tcbdbcurlast) :boolean
  (cur :pointer))

(defcfun ("tcbdbcurjump" tcbdbcurjump) :boolean
  (cur :pointer)
  (kbuf :pointer)
  (ksiz :int))

(defcfun ("tcbdbcurjump2" tcbdbcurjump2) :boolean
  (cur :pointer)
  (kstr :string))

(defcfun ("tcbdbcurprev" tcbdbcurprev) :boolean
  (cur :pointer))

(defcfun ("tcbdbcurnext" tcbdbcurnext) :boolean
  (cur :pointer))

(defcfun ("tcbdbcurput" tcbdbcurput) :boolean
  (cur :pointer)
  (vbuf :pointer)
  (vsiz :int)
  (cpmode :int))

(defcfun ("tcbdbcurput2" tcbdbcurput2) :boolean
  (cur :pointer)
  (vstr :string)
  (cpmode :int))

(defcfun ("tcbdbcurout" tcbdbcurout) :pointer
  (cur :pointer))

(defcfun ("tcbdbcurkey" tcbdbcurkey) :pointer
  (cur :pointer)
  (sp :pointer))

(defcfun ("tcbdbcurkey2" tcbdbcurkey2) :pointer
  (cur :pointer))

(defcfun ("tcbdbcurkey3" tcbdbcurkey3) :pointer
  (cur :pointer)
  (sp :pointer))

(defcfun ("tcbdbcurval" tcbdbcurval) :pointer
  (cur :pointer)
  (sp :pointer))

(defcfun ("tcbdbcurval2" tcbdbcurval2) :pointer
  (cur :pointer))

(defcfun ("tcbdbcurval3" tcbdbcurval3) :pointer
  (cur :pointer)
  (sp :pointer))

(defcfun ("tcbdbcurrec" tcbdbcurrec) :pointer
  (cur :pointer)
  (kxstr :pointer)
  (vxstr :pointer))


;; (defcfun ("tclistnew" tclistnew) :pointer)

;; (defcfun ("tclistnew2" tclistnew2) :pointer
;;   (anum :int))

;; (defcfun ("tclistdup" tclistdup) :pointer
;;   (list :pointer))

;; (defcfun ("tclistdel" tclistdel) :void
;;   (list :pointer))

;; (defcfun ("tclistnum" tclistnum) :int
;;   (list :pointer))

;; (defcfun ("tclistval" tclistval) :pointer
;;   (list :pointer)
;;   (index :int)
;;   (sp :pointer))

;; (defcfun ("tclistval2" tclistval2) :string
;;   (list :pointer)
;;   (index :int))

;; (defcfun ("tclistpush" tclistpush) :void
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (defcfun ("tclistpush2" tclistpush2) :void
;;   (list :pointer)
;;   (str :string))

;; (defcfun ("tclistpushmalloc" tclistpushmalloc) :void
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (defcfun ("tclistpop" tclistpop) :pointer
;;   (list :pointer)
;;   (sp :pointer))

;; (defcfun ("tclistpop2" tclistpop2) :string
;;   (list :pointer))

;; (defcfun ("tclistunshift" tclistunshift) :void
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (defcfun ("tclistunshift2" tclistunshift2) :void
;;   (list :pointer)
;;   (str :string))

;; (defcfun ("tclistshift" tclistshift) :pointer
;;   (list :pointer)
;;   (sp :pointer))

;; (defcfun ("tclistshift2" tclistshift2) :string
;;   (list :pointer))

;; (defcfun ("tclistinsert" tclistinsert) :void
;;   (list :pointer)
;;   (index :int)
;;   (ptr :pointer)
;;   (size :int))

;; (defcfun ("tclistinsert2" tclistinsert2) :void
;;   (list :pointer)
;;   (index :int)
;;   (str :string))

;; (defcfun ("tclistremove" tclistremove) :pointer
;;   (list :pointer)
;;   (index :int)
;;   (sp :pointer))

;; (defcfun ("tclistremove2" tclistremove2) :string
;;   (list :pointer)
;;   (index :int))

;; (defcfun ("tclistover" tclistover) :void
;;   (list :pointer)
;;   (index :int)
;;   (ptr :pointer)
;;   (size :int))

;; (defcfun ("tclistover2" tclistover2) :void
;;   (list :pointer)
;;   (index :int)
;;   (str :string))

;; (defcfun ("tclistsort" tclistsort) :void
;;   (list :pointer))

;; (defcfun ("tclistsortci" tclistsortci) :void
;;   (list :pointer))

;; (defcfun ("tclistsortex" tclistsortex) :void
;;   (list :pointer)
;;   (cmp :pointer))

;; (defcfun ("tclistlsearch" tclistlsearch) :int
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (defcfun ("tclistbsearch" tclistbsearch) :int
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (defcfun ("tclistclear" tclistclear) :void
;;   (list :pointer))

;; (defcfun ("tclistdump" tclistdump) :pointer
;;   (list :pointer)
;;   (sp :pointer))

;; (defcfun ("tclistload" tclistload) :pointer
;;   (ptr :pointer)
;;   (size :int))


(defcfun ("tchdberrmsg" tchdberrmsg) :string
  (ecode :int))

(defcfun ("tchdbnew" tchdbnew) :pointer)

(defcfun ("tchdbdel" tchdbdel) :void
  (hdb :pointer))

(defcfun ("tchdbecode" tchdbecode) :int
  (hdb :pointer))

;; (defcfun ("tchdbsetmutex" tchdbsetmutex) :pointer
;;   (hdb :pointer))

(defcfun ("tchdbtune" tchdbtune) :boolean
  (hdb :pointer)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts hdb-options))

(defcfun ("tchdbsetcache" tchdbsetcache) :boolean
  (hdb :pointer)
  (rcnum :int32))

(defcfun ("tchdbsetxmsiz" tchdbsetxmsiz) :boolean
  (hdb :pointer)
  (xmsiz :int64))

(defcfun ("tchdbopen" tchdbopen) :boolean
  (hdb :pointer)
  (path :string)
  (mode hdb-open-flags))

(defcfun ("tchdbclose" tchdbclose) :boolean
  (hdb :pointer))

(defcfun ("tchdbput" tchdbput) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(defcfun ("tchdbput2" tchdbput2) :boolean
  (hdb :pointer)
  (kstr :string)
  (vstr :string))

(defcfun ("tchdbputkeep" tchdbputkeep) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(defcfun ("tchdbputkeep2" tchdbputkeep2) :boolean
  (hdb :pointer)
  (kstr :string)
  (vstr :string))

(defcfun ("tchdbputcat" tchdbputcat) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(defcfun ("tchdbputcat2" tchdbputcat2) :boolean
  (hdb :pointer)
  (kstr :string)
  (vstr :string))

(defcfun ("tchdbputasync" tchdbputasync) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(defcfun ("tchdbputasync2" tchdbputasync2) :boolean
  (hdb :pointer)
  (kstr :string)
  (vstr :string))

(defcfun ("tchdbout" tchdbout) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(defcfun ("tchdbout2" tchdbout2) :boolean
  (hdb :pointer)
  (kstr :string))

(defcfun ("tchdbget" tchdbget) :pointer
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (sp :pointer))

(defcfun ("tchdbget2" tchdbget2) :pointer
  (hdb :pointer)
  (kstr :string))

;; (defcfun ("tchdbget3" tchdbget3) :int
;;   (hdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (max :int))

(defcfun ("tchdbvsiz" tchdbvsiz) :int
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(defcfun ("tchdbvsiz2" tchdbvsiz2) :int
  (hdb :pointer)
  (kstr :string))

(defcfun ("tchdbiterinit" tchdbiterinit) :boolean
  (hdb :pointer))

(defcfun ("tchdbiternext" tchdbiternext) :pointer
  (hdb :pointer)
  (sp :pointer))

(defcfun ("tchdbiternext2" tchdbiternext2) :string
  (hdb :pointer))

(defcfun ("tchdbiternext3" tchdbiternext3) :boolean
  (hdb :pointer)
  (kxstr :pointer)
  (vxstr :pointer))

;; (defcfun ("tchdbfwmkeys" tchdbfwmkeys) :pointer
;;   (hdb :pointer)
;;   (pbuf :pointer)
;;   (psiz :int)
;;   (max :int))

;; (defcfun ("tchdbfwmkeys2" tchdbfwmkeys2) :pointer
;;   (hdb :pointer)
;;   (pstr :string)
;;   (max :int))

(defcfun ("tchdbsync" tchdbsync) :boolean
  (hdb :pointer))

(defcfun ("tchdboptimize" tchdboptimize) :boolean
  (hdb :pointer)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts hdb-options))

(defcfun ("tchdbvanish" tchdbvanish) :boolean
  (hdb :pointer))

(defcfun ("tchdbcopy" tchdbcopy) :pointer
  (hdb :pointer)
  (path :string))

(defcfun ("tchdbtranbegin" tchdbtranbegin) :boolean
  (hdb :pointer))

(defcfun ("tchdbtrancommit" tchdbtrancommit) :boolean
  (hdb :pointer))

(defcfun ("tchdbtranabort" tchdbtranabort) :boolean
  (hdb :pointer))

(defcfun ("tchdbpath" tchdbpath) :string
  (hdb :pointer))

(defcfun ("tchdbrnum" tchdbrnum) :uint64
  (hdb :pointer))

(defcfun ("tchdbfsiz" tchdbfsiz) :uint64
  (hdb :pointer))
