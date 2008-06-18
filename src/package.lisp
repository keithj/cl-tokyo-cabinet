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
   #:+bdboreader+
   #:+bdbowriter+
   #:+bdbocreat+
   #:+bdbotrunc+
   #:+bdbonolck+
   #:+bdbolcknb+
   #:+bdbtlarge+
   #:+bdbtdeflate+
   #:+bdbttcbs+
   #:+hdbtlarge+
   #:+hdbtdeflate+
   #:+hdbttcbs+
   #:+bdbcpcurrent+
   #:+bdbcpbefore+
   #:+bdbcpafter+
   #:+hdboreader+
   #:+hdbowriter+
   #:+hdbocreat+
   #:+hdbotrunc+
   #:+hdbonolck+
   #:+hdbolcknb+
   #:+hdbtlarge+
   #:+hdbtdeflate+
   #:+hdbttcbs+
   
   ;; Variables

   ;; Functions
   #:tcbdberrmsg
   #:tcbdbnew
   #:tcbdbdel
   #:tcbdbecode
   #:tcbdbsetcmpfunc
   #:tcbdbtune
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
   
   #:tcbdbget
   #:tcbdbget2

   #:tcbdboptimize
   #:tcbdbvanish

   #:tcbdbtranbegin
   #:tcbdbtrancommit
   #:tcbdbtranabort

   #:tcbdbrnum
   #:tcbdbfsiz

   #:tchdberrmsg
   #:tchdbnew
   #:tchdbdel
   #:tchdbecode

   #:tchdbopen
   #:tchdbclose
   #:tchdbput
   #:tchdbput2

   #:tchdbget
   #:tchdbget2

   
   #:tchdboptimize
   #:tchdbvanish
   #:tchdbpath
   #:tchdbrnum
   #:tchdbfsiz
   
   
   ))

(defpackage #:cl-tcab
  (:use #:common-lisp :cffi :tcab-cffi :cl-gp-utilities)
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
   #:dbm-num-records
   #:dbm-file-size
   #:dbm-optimize
   #:set-comparator
   
   ;; Functions

   ))
