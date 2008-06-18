
(in-package #:cl-tcab-system)

(defsystem cl-tcab-test
    :depends-on (:cl-tcab :fiveam :cl-io-utilities)
    :components ((:module :cl-tcab-test
                          :serial t
                          :pathname "src/test/"
                          :components ((:file "package")
                                       (:file "cl-tcab-test")
                                       (:file "tcab-hdb-test")
                                       (:file "tcab-bdb-test")))))
