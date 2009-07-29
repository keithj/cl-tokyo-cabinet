;;;
;;; Copyright (C) 2008-2009 Keith James. All rights reserved.
;;;
;;; This file is part of cl-tokyo-cabinet.
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

(in-package :cl-tokyo-cabinet-system)

(defsystem cl-tokyo-cabinet-test
    :depends-on (:cl-tokyo-cabinet :lift :deoxybyte-io)
    :components ((:module :cl-tokyo-cabinet-test
                          :serial t
                          :pathname "src/test/"
                          :components ((:file "package")
                                       (:file "cl-tokyo-cabinet-test")
                                       (:file "tokyo-cabinet-bdb-test")
                                       (:file "tokyo-cabinet-hdb-test")))))
