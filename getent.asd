;;;; -*- mode: lisp -*-

(defsystem :getent
    :name "getent"
    :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
    :licence "Lessor Lisp General Public License"
    :version "1.0.1.0"
    :description "Library that uses getent to retreive user system information"
    :depends-on (external-program-extender
		 split-sequence)
    :components ((:module "src"
			  :components
			  ((:file "package")
			   (:file "getent" :depends-on ("package"))))))
