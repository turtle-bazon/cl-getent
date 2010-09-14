;;;; -*- mode: lisp -*-

(defsystem :lisp-getent
    :name "lisp-getent"
    :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
    :licence "Lessor Lisp General Public License"
    :version "0.0.1.0"
    :description "Library that uses getent to retreive user system information"
    :depends-on (external-program
		 external-program-extender
		 split-sequence)
    :components ((:module "src"
			  :components
			  ((:file "package")
			   (:file "lisp-getent" :depends-on ("package"))))))
