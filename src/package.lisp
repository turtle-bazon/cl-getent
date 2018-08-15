;;;; -*- mode: lisp -*-

(defpackage #:ru.bazon.server-libs.getent
  (:nicknames #:getent)
  (:use #:cl
        #:cl-bazon)
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

