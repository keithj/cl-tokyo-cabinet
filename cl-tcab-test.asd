
(in-package #:cl-tc-system)

(defsystem cl-tc-test
    :depends-on (:cl-tokyo-cabinet :fiveam :cl-io-utilities)
    :components ((:module :cl-tc-test
                          :serial t
                          :pathname "src/test/"
                          :components ((:file "package")
                                       (:file "cl-tc-test")
                                       (:file "tc-hdb-test")
                                       (:file "tc-bdb-test")))))
