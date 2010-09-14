;;;; -*- mode: lisp -*-

(defpackage #:ru.bazon.server-tools.lisp-getent
  (:nicknames #:lisp-getent)
  (:use #:cl
        #:external-program
	#:external-program-extender
	#:split-sequence)
  (:export
   #:user-uid
   #:user-gid
   #:user-name
   #:group-gid
   #:group-name
   #:find-user-by-uid
   #:find-user-by-name
   #:find-group-by-gid
   #:find-group-by-name))

