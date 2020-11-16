;;;; -*- mode: lisp -*-

(defsystem :cl-getent
    :name "cl-getent"
    :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
    :licence "Lessor Lisp General Public License"
    :version "1.0.1.1"
    :description "Library that uses getent to retreive user system information"
    :depends-on (#:cl-bazon
                 #:uiop)
    :components ((:module "src"
                  :components ((:file "package")
                               (:file "getent"
                                :depends-on ("package"))))))
