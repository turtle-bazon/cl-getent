;;;; -*- mode: lisp -*-

(defpackage #:ru.bazon.server-tools.lisp-getent
  (:nicknames #:lisp-getent)
  (:use #:cl
        #:external-program-extender
	#:split-sequence)
  (:export
   #:getent-users
   #:getent-groups
   #:user-p
   #:user-uid
   #:user-gid
   #:user-name
   #:group-p
   #:group-gid
   #:group-name
   #:group-member-names
   #:find-user-by-uid
   #:find-user-by-name
   #:find-group-by-gid
   #:find-group-by-name
   #:user-groups
   #:group-users))

