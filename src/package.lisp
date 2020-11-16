;;;; -*- mode: lisp -*-

(defpackage #:cl-getent
  (:nicknames #:getent)
  (:use #:cl
        #:cl-bazon)
  (:export
   #:users
   #:groups
   #:user-p
   #:user-uid
   #:user-gid
   #:user-name
   #:group-p
   #:group-gid
   #:group-name
   #:group-members
   #:find-user-by-uid
   #:find-user-by-name
   #:find-group-by-gid
   #:find-group-by-name
   #:user-groups
   #:group-users))

