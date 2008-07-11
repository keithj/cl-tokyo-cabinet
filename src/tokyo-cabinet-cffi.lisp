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

(in-package :tokyo-cabinet-cffi)

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
  :noblock
  :nolock)

(defbitfield hdb-open-flags
  :read
  :write
  :create
  :truncate
  :noblock
  :nolock)

(defbitfield bdb-options
  :large
  :deflate
  :tcbs
  (:defaults #xff))

(defbitfield hdb-options
  :large
  :deflate
  :tcbs
  (:defaults #xff))

(cffi:defcfun ("tcbdberrmsg" tcbdberrmsg) :string
  (ecode :int))

(cffi:defcfun ("tcbdbnew" tcbdbnew) :pointer)

(cffi:defcfun ("tcbdbdel" tcbdbdel) :void
  (bdb :pointer))

(cffi:defcfun ("tcbdbecode" tcbdbecode) :int
  (bdb :pointer))

;; (cffi:defcfun ("tcbdbsetmutex" tcbdbsetmutex) :pointer
;;   (bdb :pointer))

(cffi:defcfun ("tcbdbsetcmpfunc" tcbdbsetcmpfunc) :boolean
  (bdb :pointer)
  (cmp :pointer)
  (cmpop :pointer))

(cffi:defcfun ("tcbdbtune" tcbdbtune) :boolean
  (bdb :pointer)
  (lmemb :int32)
  (nmemb :int32)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts bdb-options))

(cffi:defcfun ("tcbdbsetcache" tcbdbsetcache) :pointer
  (bdb :pointer)
  (lcnum :pointer)
  (ncnum :pointer))

(cffi:defcfun ("tcbdbopen" tcbdbopen) :boolean
  (bdb :pointer)
  (path :string)
  (mode bdb-open-flags))

(cffi:defcfun ("tcbdbclose" tcbdbclose) :boolean
  (bdb :pointer))

(cffi:defcfun ("tcbdbput" tcbdbput) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(cffi:defcfun ("tcbdbput2" tcbdbput2) :boolean
  (bdb :pointer)
  (kstr :string)
  (vstr :string))

(cffi:defcfun ("tcbdbputkeep" tcbdbputkeep) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(cffi:defcfun ("tcbdbputkeep2" tcbdbputkeep2) :boolean
  (bdb :pointer)
  (kstr :string)
  (vstr :string))

(cffi:defcfun ("tcbdbputcat" tcbdbputcat) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(cffi:defcfun ("tcbdbputcat2" tcbdbputcat2) :boolean
  (bdb :pointer)
  (kstr :string)
  (vstr :string))

(cffi:defcfun ("tcbdbputdup" tcbdbputdup) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(cffi:defcfun ("tcbdbputdup2" tcbdbputdup2) :boolean
  (bdb :pointer)
  (kstr :string)
  (vstr :string))

(cffi:defcfun ("tcbdbputdup3" tcbdbputdup3) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vals :pointer))

(cffi:defcfun ("tcbdbout" tcbdbout) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(cffi:defcfun ("tcbdbout2" tcbdbout2) :boolean
  (bdb :pointer)
  (kstr :string))

(cffi:defcfun ("tcbdbout3" tcbdbout3) :boolean
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(cffi:defcfun ("tcbdbget" tcbdbget) :pointer
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (sp :pointer))

(cffi:defcfun ("tcbdbget2" tcbdbget2) :pointer
  (bdb :pointer)
  (kstr :string))

;; (cffi:defcfun ("tcbdbget3" tcbdbget3) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcbdbget4" tcbdbget4) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

(cffi:defcfun ("tcbdbvnum" tcbdbvnum) :int
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(cffi:defcfun ("tcbdbvnum2" tcbdbvnum2) :int
  (bdb :pointer)
  (kstr :string))

(cffi:defcfun ("tcbdbvsiz" tcbdbvsiz) :int
  (bdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(cffi:defcfun ("tcbdbvsiz2" tcbdbvsiz2) :int
  (bdb :pointer)
  (kstr :string))

;; (cffi:defcfun ("tcbdbrange" tcbdbrange) :pointer
;;   (bdb :pointer)
;;   (bkbuf :pointer)
;;   (bksiz :int)
;;   (binc :pointer)
;;   (ekbuf :pointer)
;;   (eksiz :int)
;;   (einc :pointer)
;;   (max :int))

;; (cffi:defcfun ("tcbdbrange2" tcbdbrange2) :pointer
;;   (bdb :pointer)
;;   (bkstr :string)
;;   (binc :pointer)
;;   (ekstr :string)
;;   (einc :pointer)
;;   (max :int))

;; (cffi:defcfun ("tcbdbfwmkeys" tcbdbfwmkeys) :pointer
;;   (bdb :pointer)
;;   (pbuf :pointer)
;;   (psiz :int)
;;   (max :int))

;; (cffi:defcfun ("tcbdbfwmkeys2" tcbdbfwmkeys2) :pointer
;;   (bdb :pointer)
;;   (pstr :string)
;;   (max :int))

(cffi:defcfun ("tcbdbsync" tcbdbsync) :boolean
  (bdb :pointer))

(cffi:defcfun ("tcbdboptimize" tcbdboptimize) :boolean
  (bdb :pointer)
  (lmemb :int32)
  (nmemb :int32)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts bdb-options))

(cffi:defcfun ("tcbdbvanish" tcbdbvanish) :boolean
  (bdb :pointer))

(cffi:defcfun ("tcbdbcopy" tcbdbcopy) :pointer
  (bdb :pointer)
  (path :string))

(cffi:defcfun ("tcbdbtranbegin" tcbdbtranbegin) :boolean
  (bdb :pointer))

(cffi:defcfun ("tcbdbtrancommit" tcbdbtrancommit) :boolean
  (bdb :pointer))

(cffi:defcfun ("tcbdbtranabort" tcbdbtranabort) :boolean
  (bdb :pointer))

(cffi:defcfun ("tcbdbpath" tcbdbpath) :string
  (bdb :pointer))

(cffi:defcfun ("tcbdbrnum" tcbdbrnum) :uint64
  (bdb :pointer))

(cffi:defcfun ("tcbdbfsiz" tcbdbfsiz) :uint64
  (bdb :pointer))

(cffi:defcfun ("tcbdbcurnew" tcbdbcurnew) :pointer
  (bdb :pointer))

(cffi:defcfun ("tcbdbcurdel" tcbdbcurdel) :void
  (cur :pointer))

(cffi:defcfun ("tcbdbcurfirst" tcbdbcurfirst) :boolean
  (cur :pointer))

(cffi:defcfun ("tcbdbcurlast" tcbdbcurlast) :boolean
  (cur :pointer))

(cffi:defcfun ("tcbdbcurjump" tcbdbcurjump) :boolean
  (cur :pointer)
  (kbuf :pointer)
  (ksiz :int))

(cffi:defcfun ("tcbdbcurjump2" tcbdbcurjump2) :boolean
  (cur :pointer)
  (kstr :string))

(cffi:defcfun ("tcbdbcurprev" tcbdbcurprev) :boolean
  (cur :pointer))

(cffi:defcfun ("tcbdbcurnext" tcbdbcurnext) :boolean
  (cur :pointer))

(cffi:defcfun ("tcbdbcurput" tcbdbcurput) :boolean
  (cur :pointer)
  (vbuf :pointer)
  (vsiz :int)
  (cpmode :int))

(cffi:defcfun ("tcbdbcurput2" tcbdbcurput2) :boolean
  (cur :pointer)
  (vstr :string)
  (cpmode :int))

(cffi:defcfun ("tcbdbcurout" tcbdbcurout) :pointer
  (cur :pointer))

(cffi:defcfun ("tcbdbcurkey" tcbdbcurkey) :pointer
  (cur :pointer)
  (sp :pointer))

(cffi:defcfun ("tcbdbcurkey2" tcbdbcurkey2) :pointer
  (cur :pointer))

(cffi:defcfun ("tcbdbcurkey3" tcbdbcurkey3) :pointer
  (cur :pointer)
  (sp :pointer))

(cffi:defcfun ("tcbdbcurval" tcbdbcurval) :pointer
  (cur :pointer)
  (sp :pointer))

(cffi:defcfun ("tcbdbcurval2" tcbdbcurval2) :pointer
  (cur :pointer))

(cffi:defcfun ("tcbdbcurval3" tcbdbcurval3) :pointer
  (cur :pointer)
  (sp :pointer))

(cffi:defcfun ("tcbdbcurrec" tcbdbcurrec) :pointer
  (cur :pointer)
  (kxstr :pointer)
  (vxstr :pointer))


;; (cffi:defcfun ("tclistnew" tclistnew) :pointer)

;; (cffi:defcfun ("tclistnew2" tclistnew2) :pointer
;;   (anum :int))

;; (cffi:defcfun ("tclistdup" tclistdup) :pointer
;;   (list :pointer))

;; (cffi:defcfun ("tclistdel" tclistdel) :void
;;   (list :pointer))

;; (cffi:defcfun ("tclistnum" tclistnum) :int
;;   (list :pointer))

;; (cffi:defcfun ("tclistval" tclistval) :pointer
;;   (list :pointer)
;;   (index :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tclistval2" tclistval2) :string
;;   (list :pointer)
;;   (index :int))

;; (cffi:defcfun ("tclistpush" tclistpush) :void
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tclistpush2" tclistpush2) :void
;;   (list :pointer)
;;   (str :string))

;; (cffi:defcfun ("tclistpushmalloc" tclistpushmalloc) :void
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tclistpop" tclistpop) :pointer
;;   (list :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tclistpop2" tclistpop2) :string
;;   (list :pointer))

;; (cffi:defcfun ("tclistunshift" tclistunshift) :void
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tclistunshift2" tclistunshift2) :void
;;   (list :pointer)
;;   (str :string))

;; (cffi:defcfun ("tclistshift" tclistshift) :pointer
;;   (list :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tclistshift2" tclistshift2) :string
;;   (list :pointer))

;; (cffi:defcfun ("tclistinsert" tclistinsert) :void
;;   (list :pointer)
;;   (index :int)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tclistinsert2" tclistinsert2) :void
;;   (list :pointer)
;;   (index :int)
;;   (str :string))

;; (cffi:defcfun ("tclistremove" tclistremove) :pointer
;;   (list :pointer)
;;   (index :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tclistremove2" tclistremove2) :string
;;   (list :pointer)
;;   (index :int))

;; (cffi:defcfun ("tclistover" tclistover) :void
;;   (list :pointer)
;;   (index :int)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tclistover2" tclistover2) :void
;;   (list :pointer)
;;   (index :int)
;;   (str :string))

;; (cffi:defcfun ("tclistsort" tclistsort) :void
;;   (list :pointer))

;; (cffi:defcfun ("tclistsortci" tclistsortci) :void
;;   (list :pointer))

;; (cffi:defcfun ("tclistsortex" tclistsortex) :void
;;   (list :pointer)
;;   (cmp :pointer))

;; (cffi:defcfun ("tclistlsearch" tclistlsearch) :int
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tclistbsearch" tclistbsearch) :int
;;   (list :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tclistclear" tclistclear) :void
;;   (list :pointer))

;; (cffi:defcfun ("tclistdump" tclistdump) :pointer
;;   (list :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tclistload" tclistload) :pointer
;;   (ptr :pointer)
;;   (size :int))


(cffi:defcfun ("tchdberrmsg" tchdberrmsg) :string
  (ecode :int))

(cffi:defcfun ("tchdbnew" tchdbnew) :pointer)

(cffi:defcfun ("tchdbdel" tchdbdel) :void
  (hdb :pointer))

(cffi:defcfun ("tchdbecode" tchdbecode) :int
  (hdb :pointer))

;; (cffi:defcfun ("tchdbsetmutex" tchdbsetmutex) :pointer
;;   (hdb :pointer))

(cffi:defcfun ("tchdbtune" tchdbtune) :boolean
  (hdb :pointer)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts hdb-options))

(cffi:defcfun ("tchdbsetcache" tchdbsetcache) :boolean
  (hdb :pointer)
  (rcnum :int32))

(cffi:defcfun ("tchdbopen" tchdbopen) :boolean
  (hdb :pointer)
  (path :string)
  (mode hdb-open-flags))

(cffi:defcfun ("tchdbclose" tchdbclose) :boolean
  (hdb :pointer))

(cffi:defcfun ("tchdbput" tchdbput) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(cffi:defcfun ("tchdbput2" tchdbput2) :boolean
  (hdb :pointer)
  (kstr :string)
  (vstr :string))

(cffi:defcfun ("tchdbputkeep" tchdbputkeep) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(cffi:defcfun ("tchdbputkeep2" tchdbputkeep2) :boolean
  (hdb :pointer)
  (kstr :string)
  (vstr :string))

(cffi:defcfun ("tchdbputcat" tchdbputcat) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(cffi:defcfun ("tchdbputcat2" tchdbputcat2) :boolean
  (hdb :pointer)
  (kstr :string)
  (vstr :string))

(cffi:defcfun ("tchdbputasync" tchdbputasync) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (vbuf :pointer)
  (vsiz :int))

(cffi:defcfun ("tchdbputasync2" tchdbputasync2) :boolean
  (hdb :pointer)
  (kstr :string)
  (vstr :string))

(cffi:defcfun ("tchdbout" tchdbout) :boolean
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(cffi:defcfun ("tchdbout2" tchdbout2) :boolean
  (hdb :pointer)
  (kstr :string))

(cffi:defcfun ("tchdbget" tchdbget) :pointer
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (sp :pointer))

(cffi:defcfun ("tchdbget2" tchdbget2) :pointer
  (hdb :pointer)
  (kstr :string))

;; (cffi:defcfun ("tchdbget3" tchdbget3) :int
;;   (hdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (max :int))

(cffi:defcfun ("tchdbvsiz" tchdbvsiz) :int
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int))

(cffi:defcfun ("tchdbvsiz2" tchdbvsiz2) :int
  (hdb :pointer)
  (kstr :string))

(cffi:defcfun ("tchdbiterinit" tchdbiterinit) :boolean
  (hdb :pointer))

(cffi:defcfun ("tchdbiternext" tchdbiternext) :pointer
  (hdb :pointer)
  (sp :pointer))

(cffi:defcfun ("tchdbiternext2" tchdbiternext2) :string
  (hdb :pointer))

(cffi:defcfun ("tchdbiternext3" tchdbiternext3) :boolean
  (hdb :pointer)
  (kxstr :pointer)
  (vxstr :pointer))

;; (cffi:defcfun ("tchdbfwmkeys" tchdbfwmkeys) :pointer
;;   (hdb :pointer)
;;   (pbuf :pointer)
;;   (psiz :int)
;;   (max :int))

;; (cffi:defcfun ("tchdbfwmkeys2" tchdbfwmkeys2) :pointer
;;   (hdb :pointer)
;;   (pstr :string)
;;   (max :int))

(cffi:defcfun ("tchdbsync" tchdbsync) :boolean
  (hdb :pointer))

(cffi:defcfun ("tchdboptimize" tchdboptimize) :boolean
  (hdb :pointer)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts hdb-options))

(cffi:defcfun ("tchdbvanish" tchdbvanish) :boolean
  (hdb :pointer))

;; (cffi:defcfun ("tchdbcopy" tchdbcopy) :pointer
;;   (hdb :pointer)
;;   (path :string))

(cffi:defcfun ("tchdbpath" tchdbpath) :string
  (hdb :pointer))

(cffi:defcfun ("tchdbrnum" tchdbrnum) :uint64
  (hdb :pointer))

(cffi:defcfun ("tchdbfsiz" tchdbfsiz) :uint64
  (hdb :pointer))
