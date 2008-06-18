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

(in-package :tcab-cffi)

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

(defconstant +bdboreader+ 1)
(defconstant +bdbowriter+ 2)
(defconstant +bdbocreat+ 4)
(defconstant +bdbotrunc+ 8)
(defconstant +bdbonolck+ 16)
(defconstant +bdbolcknb+ 32)
(defconstant +bdbtlarge+ 1)
(defconstant +bdbtdeflate+ 2)
(defconstant +bdbttcbs+ 4)
(defconstant +bdbcpcurrent+ 0)
(defconstant +bdbcpbefore+ 1)
(defconstant +bdbcpafter+ 2)

(defconstant +hdboreader+ 1)
(defconstant +hdbowriter+ 2)
(defconstant +hdbocreat+ 4)
(defconstant +hdbotrunc+ 8)
(defconstant +hdbonolck+ 16)
(defconstant +hdbolcknb+ 32)
(defconstant +hdbtlarge+ 1)
(defconstant +hdbtdeflate+ 2)
(defconstant +hdbttcbs+ 4)

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

;; (cffi:defcfun ("tcbdbtune" tcbdbtune) :pointer
;;   (bdb :pointer)
;;   (lmemb :pointer)
;;   (nmemb :pointer)
;;   (bnum :pointer)
;;   (apow :pointer)
;;   (fpow :pointer)
;;   (opts :pointer))

;; (cffi:defcfun ("tcbdbsetcache" tcbdbsetcache) :pointer
;;   (bdb :pointer)
;;   (lcnum :pointer)
;;   (ncnum :pointer))

(cffi:defcfun ("tcbdbopen" tcbdbopen) :boolean
  (bdb :pointer)
  (path :string)
  (omode :int))

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

;; (cffi:defcfun ("tcbdbputkeep" tcbdbputkeep) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcbdbputkeep2" tcbdbputkeep2) :pointer
;;   (bdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcbdbputcat" tcbdbputcat) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcbdbputcat2" tcbdbputcat2) :pointer
;;   (bdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcbdbputdup" tcbdbputdup) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcbdbputdup2" tcbdbputdup2) :pointer
;;   (bdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcbdbputdup3" tcbdbputdup3) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vals :pointer))

;; (cffi:defcfun ("tcbdbout" tcbdbout) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tcbdbout2" tcbdbout2) :pointer
;;   (bdb :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcbdbout3" tcbdbout3) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

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

;; (cffi:defcfun ("tcbdbvnum" tcbdbvnum) :int
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tcbdbvnum2" tcbdbvnum2) :int
;;   (bdb :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcbdbvsiz" tcbdbvsiz) :int
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tcbdbvsiz2" tcbdbvsiz2) :int
;;   (bdb :pointer)
;;   (kstr :string))

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

;; (cffi:defcfun ("tcbdbsync" tcbdbsync) :pointer
;;   (bdb :pointer))

(cffi:defcfun ("tcbdboptimize" tcbdboptimize) :pointer
  (bdb :pointer)
  (lmemb :int32)
  (nmemb :int32)
  (bnum :int64)
  (apow :int8)
  (fpow :int8)
  (opts :uint8))

(cffi:defcfun ("tcbdbvanish" tcbdbvanish) :boolean
  (bdb :pointer))

;; (cffi:defcfun ("tcbdbcopy" tcbdbcopy) :pointer
;;   (bdb :pointer)
;;   (path :string))

(cffi:defcfun ("tcbdbtranbegin" tcbdbtranbegin) :boolean
  (bdb :pointer))

(cffi:defcfun ("tcbdbtrancommit" tcbdbtrancommit) :boolean
  (bdb :pointer))

(cffi:defcfun ("tcbdbtranabort" tcbdbtranabort) :boolean
  (bdb :pointer))

;; (cffi:defcfun ("tcbdbpath" tcbdbpath) :string
;;   (bdb :pointer))

(cffi:defcfun ("tcbdbrnum" tcbdbrnum) :uint64
  (bdb :pointer))

(cffi:defcfun ("tcbdbfsiz" tcbdbfsiz) :uint64
  (bdb :pointer))

;; (cffi:defcfun ("tcbdbcurnew" tcbdbcurnew) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbcurdel" tcbdbcurdel) :void
;;   (cur :pointer))

;; (cffi:defcfun ("tcbdbcurfirst" tcbdbcurfirst) :pointer
;;   (cur :pointer))

;; (cffi:defcfun ("tcbdbcurlast" tcbdbcurlast) :pointer
;;   (cur :pointer))

;; (cffi:defcfun ("tcbdbcurjump" tcbdbcurjump) :pointer
;;   (cur :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tcbdbcurjump2" tcbdbcurjump2) :pointer
;;   (cur :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcbdbcurprev" tcbdbcurprev) :pointer
;;   (cur :pointer))

;; (cffi:defcfun ("tcbdbcurnext" tcbdbcurnext) :pointer
;;   (cur :pointer))

;; (cffi:defcfun ("tcbdbcurput" tcbdbcurput) :pointer
;;   (cur :pointer)
;;   (vbuf :pointer)
;;   (vsiz :int)
;;   (cpmode :int))

;; (cffi:defcfun ("tcbdbcurput2" tcbdbcurput2) :pointer
;;   (cur :pointer)
;;   (vstr :string)
;;   (cpmode :int))

;; (cffi:defcfun ("tcbdbcurout" tcbdbcurout) :pointer
;;   (cur :pointer))

;; (cffi:defcfun ("tcbdbcurkey" tcbdbcurkey) :string
;;   (cur :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tcbdbcurkey2" tcbdbcurkey2) :string
;;   (cur :pointer))

;; (cffi:defcfun ("tcbdbcurkey3" tcbdbcurkey3) :string
;;   (cur :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tcbdbcurval" tcbdbcurval) :string
;;   (cur :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tcbdbcurval2" tcbdbcurval2) :string
;;   (cur :pointer))

;; (cffi:defcfun ("tcbdbcurval3" tcbdbcurval3) :string
;;   (cur :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tcbdbcurrec" tcbdbcurrec) :pointer
;;   (cur :pointer)
;;   (kxstr :pointer)
;;   (vxstr :pointer))


;; (cffi:defcvar ("tcversion" tcversion)
;;  :string)

;; (cffi:defcvar ("tcfatalfunc" tcfatalfunc)
;;  :pointer)

;; (cffi:defcfun ("tcmalloc" tcmalloc) :pointer
;;   (size :pointer))

;; (cffi:defcfun ("tccalloc" tccalloc) :pointer
;;   (nmemb :pointer)
;;   (size :pointer))

;; (cffi:defcfun ("tcrealloc" tcrealloc) :pointer
;;   (ptr :pointer)
;;   (size :pointer))

;; (cffi:defcfun ("tcmemdup" tcmemdup) :pointer
;;   (ptr :pointer)
;;   (size :pointer))

;; (cffi:defcfun ("tcstrdup" tcstrdup) :string
;;   (str :pointer))

;; (cffi:defcfun ("tcfree" tcfree) :void
;;   (ptr :pointer))

;; (cffi:defcstruct TCXSTR
;; 	(ptr :string)
;; 	(size :int)
;; 	(asize :int))

;; (cffi:defcfun ("tcxstrnew" tcxstrnew) :pointer)

;; (cffi:defcfun ("tcxstrnew2" tcxstrnew2) :pointer
;;   (str :string))

;; (cffi:defcfun ("tcxstrnew3" tcxstrnew3) :pointer
;;   (asiz :int))

;; (cffi:defcfun ("tcxstrdup" tcxstrdup) :pointer
;;   (xstr :pointer))

;; (cffi:defcfun ("tcxstrdel" tcxstrdel) :void
;;   (xstr :pointer))

;; (cffi:defcfun ("tcxstrcat" tcxstrcat) :void
;;   (xstr :pointer)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tcxstrcat2" tcxstrcat2) :void
;;   (xstr :pointer)
;;   (str :string))

;; (cffi:defcfun ("tcxstrptr" tcxstrptr) :pointer
;;   (xstr :pointer))

;; (cffi:defcfun ("tcxstrsize" tcxstrsize) :int
;;   (xstr :pointer))

;; (cffi:defcfun ("tcxstrclear" tcxstrclear) :void
;;   (xstr :pointer))

;; (cffi:defcfun ("tcxstrtomalloc" tcxstrtomalloc) :pointer
;;   (xstr :pointer))

;; (cffi:defcfun ("tcxstrfrommalloc" tcxstrfrommalloc) :pointer
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tcxstrprintf" tcxstrprintf) :void
;;   (xstr :pointer)
;;   (format :string)
;;   &rest)

;; (cffi:defcfun ("tcsprintf" tcsprintf) :string
;;   (format :string)
;;   &rest)

;; (cffi:defcstruct TCLISTDATUM
;; 	(ptr :string)
;; 	(size :int))

;; (cffi:defcstruct TCLIST
;; 	(array :pointer)
;; 	(anum :int)
;; 	(start :int)
;; 	(num :int))

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

;; (cffi:defcstruct TCMAPREC
;; 	(ksiz :int)
;; 	(vsiz :int)
;; 	(hash :unsigned-int)
;; 	(left :pointer)
;; 	(right :pointer)
;; 	(prev :pointer)
;; 	(next :pointer))

;; (cffi:defcstruct TCMAP
;; 	(buckets :pointer)
;; 	(first :pointer)
;; 	(last :pointer)
;; 	(cur :pointer)
;; 	(bnum :pointer)
;; 	(rnum :pointer)
;; 	(msiz :pointer))

;; (cffi:defcfun ("tcmapnew" tcmapnew) :pointer)

;; (cffi:defcfun ("tcmapnew2" tcmapnew2) :pointer
;;   (bnum :pointer))

;; (cffi:defcfun ("tcmapdup" tcmapdup) :pointer
;;   (map :pointer))

;; (cffi:defcfun ("tcmapdel" tcmapdel) :void
;;   (map :pointer))

;; (cffi:defcfun ("tcmapput" tcmapput) :void
;;   (map :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcmapput2" tcmapput2) :void
;;   (map :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcmapput3" tcmapput3) :void
;;   (map :pointer)
;;   (kbuf :string)
;;   (ksiz :int)
;;   (fvbuf :pointer)
;;   (fvsiz :int)
;;   (lvbuf :string)
;;   (lvsiz :int))

;; (cffi:defcfun ("tcmapputkeep" tcmapputkeep) :pointer
;;   (map :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcmapputkeep2" tcmapputkeep2) :pointer
;;   (map :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcmapputcat" tcmapputcat) :void
;;   (map :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcmapputcat2" tcmapputcat2) :void
;;   (map :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcmapout" tcmapout) :pointer
;;   (map :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tcmapout2" tcmapout2) :pointer
;;   (map :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcmapget" tcmapget) :pointer
;;   (map :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmapget2" tcmapget2) :string
;;   (map :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcmapget3" tcmapget3) :pointer
;;   (map :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmapmove" tcmapmove) :pointer
;;   (map :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (head :pointer))

;; (cffi:defcfun ("tcmapmove2" tcmapmove2) :pointer
;;   (map :pointer)
;;   (kstr :string)
;;   (head :pointer))

;; (cffi:defcfun ("tcmapiterinit" tcmapiterinit) :void
;;   (map :pointer))

;; (cffi:defcfun ("tcmapiternext" tcmapiternext) :pointer
;;   (map :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmapiternext2" tcmapiternext2) :string
;;   (map :pointer))

;; (cffi:defcfun ("tcmapiterval" tcmapiterval) :pointer
;;   (kbuf :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmapiterval2" tcmapiterval2) :string
;;   (kstr :string))

;; (cffi:defcfun ("tcmaprnum" tcmaprnum) :pointer
;;   (map :pointer))

;; (cffi:defcfun ("tcmapmsiz" tcmapmsiz) :pointer
;;   (map :pointer))

;; (cffi:defcfun ("tcmapkeys" tcmapkeys) :pointer
;;   (map :pointer))

;; (cffi:defcfun ("tcmapvals" tcmapvals) :pointer
;;   (map :pointer))

;; (cffi:defcfun ("tcmapaddint" tcmapaddint) :void
;;   (map :pointer)
;;   (kbuf :string)
;;   (ksiz :int)
;;   (num :int))

;; (cffi:defcfun ("tcmapclear" tcmapclear) :void
;;   (map :pointer))

;; (cffi:defcfun ("tcmapcutfront" tcmapcutfront) :void
;;   (map :pointer)
;;   (num :int))

;; (cffi:defcfun ("tcmapdump" tcmapdump) :pointer
;;   (map :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmapload" tcmapload) :pointer
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tcmaploadone" tcmaploadone) :pointer
;;   (ptr :pointer)
;;   (size :int)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (sp :pointer))

;; (cffi:defcstruct TCMDB
;; 	(mmtxs :pointer)
;; 	(imtx :pointer)
;; 	(maps :pointer)
;; 	(iter :int))

;; (cffi:defcfun ("tcmdbnew" tcmdbnew) :pointer)

;; (cffi:defcfun ("tcmdbnew2" tcmdbnew2) :pointer
;;   (bnum :pointer))

;; (cffi:defcfun ("tcmdbdel" tcmdbdel) :void
;;   (mdb :pointer))

;; (cffi:defcfun ("tcmdbput" tcmdbput) :void
;;   (mdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcmdbput2" tcmdbput2) :void
;;   (mdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcmdbput3" tcmdbput3) :void
;;   (mdb :pointer)
;;   (kbuf :string)
;;   (ksiz :int)
;;   (fvbuf :pointer)
;;   (fvsiz :int)
;;   (lvbuf :string)
;;   (lvsiz :int))

;; (cffi:defcfun ("tcmdbputkeep" tcmdbputkeep) :pointer
;;   (mdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcmdbputkeep2" tcmdbputkeep2) :pointer
;;   (mdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcmdbputcat" tcmdbputcat) :void
;;   (mdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcmdbputcat2" tcmdbputcat2) :void
;;   (mdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcmdbout" tcmdbout) :pointer
;;   (mdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tcmdbout2" tcmdbout2) :pointer
;;   (mdb :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcmdbget" tcmdbget) :pointer
;;   (mdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmdbget2" tcmdbget2) :string
;;   (mdb :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcmdbget3" tcmdbget3) :pointer
;;   (mdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmdbvsiz" tcmdbvsiz) :int
;;   (mdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tcmdbvsiz2" tcmdbvsiz2) :int
;;   (mdb :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcmdbiterinit" tcmdbiterinit) :void
;;   (mdb :pointer))

;; (cffi:defcfun ("tcmdbiternext" tcmdbiternext) :pointer
;;   (mdb :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmdbiternext2" tcmdbiternext2) :string
;;   (mdb :pointer))

;; (cffi:defcfun ("tcmdbfwmkeys" tcmdbfwmkeys) :pointer
;;   (mdb :pointer)
;;   (pbuf :pointer)
;;   (psiz :int)
;;   (max :int))

;; (cffi:defcfun ("tcmdbfwmkeys2" tcmdbfwmkeys2) :pointer
;;   (mdb :pointer)
;;   (pstr :string)
;;   (max :int))

;; (cffi:defcfun ("tcmdbrnum" tcmdbrnum) :pointer
;;   (mdb :pointer))

;; (cffi:defcfun ("tcmdbmsiz" tcmdbmsiz) :pointer
;;   (mdb :pointer))

;; (cffi:defcfun ("tcmdbvanish" tcmdbvanish) :void
;;   (mdb :pointer))

;; (cffi:defcfun ("tcmdbcutfront" tcmdbcutfront) :void
;;   (mdb :pointer)
;;   (num :int))

;; (cffi:defcstruct TCMPELEM
;; 	(ptr :pointer)
;; 	(del :pointer))

;; (cffi:defcstruct TCMPOOL
;; 	(mutex :pointer)
;; 	(elems :pointer)
;; 	(anum :int)
;; 	(num :int))

;; (cffi:defcfun ("tcmpoolnew" tcmpoolnew) :pointer)

;; (cffi:defcfun ("tcmpooldel" tcmpooldel) :void
;;   (mpool :pointer))

;; (cffi:defcfun ("tcmpoolput" tcmpoolput) :void
;;   (mpool :pointer)
;;   (ptr :pointer)
;;   (del :pointer))

;; (cffi:defcfun ("tcmpoolputptr" tcmpoolputptr) :void
;;   (mpool :pointer)
;;   (ptr :pointer))

;; (cffi:defcfun ("tcmpoolputxstr" tcmpoolputxstr) :void
;;   (mpool :pointer)
;;   (xstr :pointer))

;; (cffi:defcfun ("tcmpoolputlist" tcmpoolputlist) :void
;;   (mpool :pointer)
;;   (list :pointer))

;; (cffi:defcfun ("tcmpoolputmap" tcmpoolputmap) :void
;;   (mpool :pointer)
;;   (map :pointer))

;; (cffi:defcfun ("tcmpoolmalloc" tcmpoolmalloc) :pointer
;;   (mpool :pointer)
;;   (size :pointer))

;; (cffi:defcfun ("tcmpoolxstrnew" tcmpoolxstrnew) :pointer
;;   (mpool :pointer))

;; (cffi:defcfun ("tcmpoollistnew" tcmpoollistnew) :pointer
;;   (mpool :pointer))

;; (cffi:defcfun ("tcmpoolmapnew" tcmpoolmapnew) :pointer
;;   (mpool :pointer))

;; (cffi:defcfun ("tcmpoolglobal" tcmpoolglobal) :pointer)

;; (cffi:defcfun ("tclmax" tclmax) :long
;;   (a :long)
;;   (b :long))

;; (cffi:defcfun ("tclmin" tclmin) :long
;;   (a :long)
;;   (b :long))

;; (cffi:defcfun ("tclrand" tclrand) :unsigned-long)

;; (cffi:defcfun ("tcdrand" tcdrand) :double)

;; (cffi:defcfun ("tcdrandnd" tcdrandnd) :double
;;   (avg :double)
;;   (sd :double))

;; (cffi:defcfun ("tcstricmp" tcstricmp) :int
;;   (astr :string)
;;   (bstr :string))

;; (cffi:defcfun ("tcstrfwm" tcstrfwm) :pointer
;;   (str :string)
;;   (key :string))

;; (cffi:defcfun ("tcstrifwm" tcstrifwm) :pointer
;;   (str :string)
;;   (key :string))

;; (cffi:defcfun ("tcstrbwm" tcstrbwm) :pointer
;;   (str :string)
;;   (key :string))

;; (cffi:defcfun ("tcstribwm" tcstribwm) :pointer
;;   (str :string)
;;   (key :string))

;; (cffi:defcfun ("tcstrdist" tcstrdist) :int
;;   (astr :string)
;;   (bstr :string))

;; (cffi:defcfun ("tcstrdistutf" tcstrdistutf) :int
;;   (astr :string)
;;   (bstr :string))

;; (cffi:defcfun ("tcstrtoupper" tcstrtoupper) :string
;;   (str :string))

;; (cffi:defcfun ("tcstrtolower" tcstrtolower) :string
;;   (str :string))

;; (cffi:defcfun ("tcstrtrim" tcstrtrim) :string
;;   (str :string))

;; (cffi:defcfun ("tcstrsqzspc" tcstrsqzspc) :string
;;   (str :string))

;; (cffi:defcfun ("tcstrsubchr" tcstrsubchr) :string
;;   (str :string)
;;   (rstr :string)
;;   (sstr :string))

;; (cffi:defcfun ("tcstrcntutf" tcstrcntutf) :int
;;   (str :string))

;; (cffi:defcfun ("tcstrcututf" tcstrcututf) :string
;;   (str :string)
;;   (num :int))

;; (cffi:defcfun ("tcstrutftoucs" tcstrutftoucs) :void
;;   (str :string)
;;   (ary :pointer)
;;   (np :pointer))

;; (cffi:defcfun ("tcstrucstoutf" tcstrucstoutf) :void
;;   (ary :pointer)
;;   (num :int)
;;   (str :string))

;; (cffi:defcfun ("tcstrsplit" tcstrsplit) :pointer
;;   (str :string)
;;   (delims :string))

;; (cffi:defcfun ("tcstrjoin" tcstrjoin) :string
;;   (list :pointer)
;;   (delim :char))

;; (cffi:defcfun ("tctime" tctime) :double)

;; (cffi:defcfun ("tccalendar" tccalendar) :void
;;   (t_arg0 :pointer)
;;   (jl :int)
;;   (yearp :pointer)
;;   (monp :pointer)
;;   (dayp :pointer)
;;   (hourp :pointer)
;;   (minp :pointer)
;;   (secp :pointer))

;; (cffi:defcfun ("tcdatestrwww" tcdatestrwww) :void
;;   (t_arg0 :pointer)
;;   (jl :int)
;;   (buf :string))

;; (cffi:defcfun ("tcdatestrhttp" tcdatestrhttp) :void
;;   (t_arg0 :pointer)
;;   (jl :int)
;;   (buf :string))

;; (cffi:defcfun ("tcstrmktime" tcstrmktime) :pointer
;;   (str :string))

;; (cffi:defcfun ("tcdayofweek" tcdayofweek) :int
;;   (year :int)
;;   (mon :int)
;;   (day :int))

;; (cffi:defcfun ("tcrealpath" tcrealpath) :string
;;   (path :string))

;; (cffi:defcfun ("tcreadfile" tcreadfile) :pointer
;;   (path :string)
;;   (limit :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcreadfilelines" tcreadfilelines) :pointer
;;   (path :string))

;; (cffi:defcfun ("tcwritefile" tcwritefile) :pointer
;;   (path :string)
;;   (ptr :pointer)
;;   (size :int))

;; (cffi:defcfun ("tccopyfile" tccopyfile) :pointer
;;   (src :string)
;;   (dest :string))

;; (cffi:defcfun ("tcreaddir" tcreaddir) :pointer
;;   (path :string))

;; (cffi:defcfun ("tcremovelink" tcremovelink) :pointer
;;   (path :string))

;; (cffi:defcfun ("tcwrite" tcwrite) :pointer
;;   (fd :int)
;;   (buf :pointer)
;;   (size :pointer))

;; (cffi:defcfun ("tcread" tcread) :pointer
;;   (fd :int)
;;   (buf :pointer)
;;   (size :pointer))

;; (cffi:defcfun ("tclock" tclock) :pointer
;;   (fd :int)
;;   (ex :pointer)
;;   (nb :pointer))

;; (cffi:defcfun ("tcurlencode" tcurlencode) :string
;;   (ptr :string)
;;   (size :int))

;; (cffi:defcfun ("tcurldecode" tcurldecode) :string
;;   (str :string)
;;   (sp :pointer))

;; (cffi:defcfun ("tcurlbreak" tcurlbreak) :pointer
;;   (str :string))

;; (cffi:defcfun ("tcurlresolve" tcurlresolve) :string
;;   (base :string)
;;   (target :string))

;; (cffi:defcfun ("tcbaseencode" tcbaseencode) :string
;;   (ptr :string)
;;   (size :int))

;; (cffi:defcfun ("tcbasedecode" tcbasedecode) :string
;;   (str :string)
;;   (sp :pointer))

;; (cffi:defcfun ("tcquoteencode" tcquoteencode) :string
;;   (ptr :string)
;;   (size :int))

;; (cffi:defcfun ("tcquotedecode" tcquotedecode) :string
;;   (str :string)
;;   (sp :pointer))

;; (cffi:defcfun ("tcmimeencode" tcmimeencode) :string
;;   (str :string)
;;   (encname :string)
;;   (base :pointer))

;; (cffi:defcfun ("tcmimedecode" tcmimedecode) :string
;;   (str :string)
;;   (enp :string))

;; (cffi:defcfun ("tcpackencode" tcpackencode) :string
;;   (ptr :string)
;;   (size :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcpackdecode" tcpackdecode) :string
;;   (ptr :string)
;;   (size :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcbsencode" tcbsencode) :string
;;   (ptr :string)
;;   (size :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcbsdecode" tcbsdecode) :string
;;   (ptr :string)
;;   (size :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcdeflate" tcdeflate) :string
;;   (ptr :string)
;;   (size :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcinflate" tcinflate) :string
;;   (ptr :string)
;;   (size :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcgzipencode" tcgzipencode) :string
;;   (ptr :string)
;;   (size :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcgzipdecode" tcgzipdecode) :string
;;   (ptr :string)
;;   (size :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcgetcrc" tcgetcrc) :unsigned-int
;;   (ptr :string)
;;   (size :int))

;; (cffi:defcfun ("tcberencode" tcberencode) :string
;;   (ary :pointer)
;;   (anum :int)
;;   (sp :pointer))

;; (cffi:defcfun ("tcberdecode" tcberdecode) :pointer
;;   (ptr :string)
;;   (size :int)
;;   (np :pointer))

;; (cffi:defcfun ("tcxmlescape" tcxmlescape) :string
;;   (str :string))

;; (cffi:defcfun ("tcxmlunescape" tcxmlunescape) :string
;;   (str :string))

;; (cffi:defcfun ("tcxmlbreak" tcxmlbreak) :pointer
;;   (str :string))

;; (cffi:defcfun ("tcxmlattrs" tcxmlattrs) :pointer
;;   (str :string))

;; (cffi:defcstruct TCBITSTRM
;; 	(sp :pointer)
;; 	(cp :pointer)
;; 	(idx :int)
;; 	(size :int))

;; (cl:defconstant _TC_VERSION "1.2.1")

;; (cl:defconstant _TC_LIBVER 302)

;; (cl:defconstant _TC_FORMATVER "1.0")

;; (cffi:defcfun ("tcmyfatal" tcmyfatal) :pointer
;;   (message :string))

;; (cffi:defcfun ("tcglobalmutexlock" tcglobalmutexlock) :pointer)

;; (cffi:defcfun ("tcglobalmutexlockshared" tcglobalmutexlockshared) :pointer)

;; (cffi:defcfun ("tcglobalmutexunlock" tcglobalmutexunlock) :pointer)

;; (cffi:defcfun ("tcbwtencode" tcbwtencode) :string
;;   (ptr :string)
;;   (size :int)
;;   (idxp :pointer))

;; (cffi:defcfun ("tcbwtdecode" tcbwtdecode) :string
;;   (ptr :string)
;;   (size :int)
;;   (idx :int))


(cffi:defcfun ("tchdberrmsg" tchdberrmsg) :string
  (ecode :int))

(cffi:defcfun ("tchdbnew" tchdbnew) :pointer)

(cffi:defcfun ("tchdbdel" tchdbdel) :void
  (hdb :pointer))

(cffi:defcfun ("tchdbecode" tchdbecode) :int
  (hdb :pointer))

;; (cffi:defcfun ("tchdbsetmutex" tchdbsetmutex) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbtune" tchdbtune) :pointer
;;   (hdb :pointer)
;;   (bnum :pointer)
;;   (apow :pointer)
;;   (fpow :pointer)
;;   (opts :pointer))

;; (cffi:defcfun ("tchdbsetcache" tchdbsetcache) :pointer
;;   (hdb :pointer)
;;   (rcnum :pointer))

(cffi:defcfun ("tchdbopen" tchdbopen) :boolean
  (hdb :pointer)
  (path :string)
  (omode :int))

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

;; (cffi:defcfun ("tchdbputkeep" tchdbputkeep) :pointer
;;   (hdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tchdbputkeep2" tchdbputkeep2) :pointer
;;   (hdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tchdbputcat" tchdbputcat) :pointer
;;   (hdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tchdbputcat2" tchdbputcat2) :pointer
;;   (hdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tchdbputasync" tchdbputasync) :pointer
;;   (hdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tchdbputasync2" tchdbputasync2) :pointer
;;   (hdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tchdbout" tchdbout) :pointer
;;   (hdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tchdbout2" tchdbout2) :pointer
;;   (hdb :pointer)
;;   (kstr :string))

(cffi:defcfun ("tchdbget" tchdbget) :pointer
  (hdb :pointer)
  (kbuf :pointer)
  (ksiz :int)
  (sp :pointer))

(cffi:defcfun ("tchdbget2" tchdbget2) :string
  (hdb :pointer)
  (kstr :string))

;; (cffi:defcfun ("tchdbget3" tchdbget3) :int
;;   (hdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (max :int))

;; (cffi:defcfun ("tchdbvsiz" tchdbvsiz) :int
;;   (hdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tchdbvsiz2" tchdbvsiz2) :int
;;   (hdb :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tchdbiterinit" tchdbiterinit) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbiternext" tchdbiternext) :pointer
;;   (hdb :pointer)
;;   (sp :pointer))

;; (cffi:defcfun ("tchdbiternext2" tchdbiternext2) :string
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbiternext3" tchdbiternext3) :pointer
;;   (hdb :pointer)
;;   (kxstr :pointer)
;;   (vxstr :pointer))

;; (cffi:defcfun ("tchdbfwmkeys" tchdbfwmkeys) :pointer
;;   (hdb :pointer)
;;   (pbuf :pointer)
;;   (psiz :int)
;;   (max :int))

;; (cffi:defcfun ("tchdbfwmkeys2" tchdbfwmkeys2) :pointer
;;   (hdb :pointer)
;;   (pstr :string)
;;   (max :int))

;; (cffi:defcfun ("tchdbsync" tchdbsync) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdboptimize" tchdboptimize) :pointer
;;   (hdb :pointer)
;;   (bnum :pointer)
;;   (apow :pointer)
;;   (fpow :pointer)
;;   (opts :pointer))

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

;; (cffi:defcfun ("tchdbsetecode" tchdbsetecode) :void
;;   (hdb :pointer)
;;   (ecode :int)
;;   (filename :string)
;;   (line :int)
;;   (func :string))

;; (cffi:defcfun ("tchdbsettype" tchdbsettype) :void
;;   (hdb :pointer)
;;   (type :pointer))

;; (cffi:defcfun ("tchdbsetdbgfd" tchdbsetdbgfd) :void
;;   (hdb :pointer)
;;   (fd :int))

;; (cffi:defcfun ("tchdbdbgfd" tchdbdbgfd) :int
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbmemsync" tchdbmemsync) :pointer
;;   (hdb :pointer)
;;   (phys :pointer))

;; (cffi:defcfun ("tchdbbnum" tchdbbnum) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbalign" tchdbalign) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbfbpmax" tchdbfbpmax) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbinode" tchdbinode) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbmtime" tchdbmtime) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbomode" tchdbomode) :int
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbtype" tchdbtype) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbflags" tchdbflags) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbopts" tchdbopts) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbopaque" tchdbopaque) :string
;;   (hdb :pointer))

;; (cffi:defcfun ("tchdbbnumused" tchdbbnumused) :pointer
;;   (hdb :pointer))

;; (cffi:defcfun ("tcbdbsetecode" tcbdbsetecode) :void
;;   (bdb :pointer)
;;   (ecode :int)
;;   (filename :string)
;;   (line :int)
;;   (func :string))

;; (cffi:defcfun ("tcbdbsetdbgfd" tcbdbsetdbgfd) :void
;;   (bdb :pointer)
;;   (fd :int))

;; (cffi:defcfun ("tcbdbdbgfd" tcbdbdbgfd) :int
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbmemsync" tcbdbmemsync) :pointer
;;   (bdb :pointer)
;;   (phys :pointer))

;; (cffi:defcfun ("tcbdblmemb" tcbdblmemb) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbnmemb" tcbdbnmemb) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdblnum" tcbdblnum) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbnnum" tcbdbnnum) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbbnum" tcbdbbnum) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbalign" tcbdbalign) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbfbpmax" tcbdbfbpmax) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbinode" tcbdbinode) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbmtime" tcbdbmtime) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbflags" tcbdbflags) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbopts" tcbdbopts) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbopaque" tcbdbopaque) :string
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbbnumused" tcbdbbnumused) :pointer
;;   (bdb :pointer))

;; (cffi:defcfun ("tcbdbsetlsmax" tcbdbsetlsmax) :pointer
;;   (bdb :pointer)
;;   (lsmax :pointer))

;; (cffi:defcfun ("tcbdbputdupback" tcbdbputdupback) :pointer
;;   (bdb :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int)
;;   (vbuf :pointer)
;;   (vsiz :int))

;; (cffi:defcfun ("tcbdbputdupback2" tcbdbputdupback2) :pointer
;;   (bdb :pointer)
;;   (kstr :string)
;;   (vstr :string))

;; (cffi:defcfun ("tcbdbcurjumpback" tcbdbcurjumpback) :pointer
;;   (cur :pointer)
;;   (kbuf :pointer)
;;   (ksiz :int))

;; (cffi:defcfun ("tcbdbcurjumpback2" tcbdbcurjumpback2) :pointer
;;   (cur :pointer)
;;   (kstr :string))

;; (cffi:defcfun ("tcbdbcmplexical" tcbdbcmplexical) :int
;;   (aptr :string)
;;   (asiz :int)
;;   (bptr :string)
;;   (bsiz :int)
;;   (op :pointer))

;; (cffi:defcfun ("tcbdbcmpdecimal" tcbdbcmpdecimal) :int
;;   (aptr :string)
;;   (asiz :int)
;;   (bptr :string)
;;   (bsiz :int)
;;   (op :pointer))

;; (cffi:defcfun ("tcbdbcmpint32" tcbdbcmpint32) :int
;;   (aptr :string)
;;   (asiz :int)
;;   (bptr :string)
;;   (bsiz :int)
;;   (op :pointer))

;; (cffi:defcfun ("tcbdbcmpint64" tcbdbcmpint64) :int
;;   (aptr :string)
;;   (asiz :int)
;;   (bptr :string)
;;   (bsiz :int)
;;   (op :pointer))


