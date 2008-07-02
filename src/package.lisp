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

(defpackage #:tcab-cffi
  (:use #:common-lisp :cffi)
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

   #:tchdbpath
   #:tchdbrnum
   #:tchdbfsiz))

(defpackage #:cl-tcab
  (:use #:common-lisp :cffi :tcab-cffi :cl-gp-utilities)
  (:nicknames #:tc)
  (:export
   ;; Constants

   ;; Variables

   ;; Classes
   #:tcab-bdb
   #:tcab-hdb

   ;; Conditions
   #:dbm-error

   ;; Generics
   #:dbm-open
   #:dbm-close
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
   #:set-comparator
   
   ;; Functions
   ))
