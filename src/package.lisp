
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
   
   #:tcbdbopen
   #:tcbdbclose

   #:tcbdbput
   #:tcbdbput2

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
   #:set-comparator
   
   ;; Functions

   ))
