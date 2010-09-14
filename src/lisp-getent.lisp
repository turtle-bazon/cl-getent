;;;; -*- mode: lisp -*-

(in-package :ru.bazon.server-tools.lisp-getent)

(defun find-entry (key entries entry-key-fn)
  (let ((entries (find-entries key entries entry-key-fn)))
    (if (= (length entries) 1)
	(first entries)
	(error "Multiple occuencies (~a) for key ~a" entries key))))

(defun find-entries (key entries entry-key-fn)
  (remove-if-not
   #'(lambda (entry)
       (equal key (funcall entry-key-fn entry)))
   entries))

(defun user-uid (user-info)
  (parse-integer (elt user-info 2)))

(defun user-gid (user-info)
  (parse-integer (elt user-info 3)))

(defun user-name (user-info)
  (elt user-info 0))

(defun group-gid (group-info)
  (parse-integer (elt group-info 2)))

(defun group-name (group-info)
  (elt group-info 0))

(defun find-user-by-uid (uid user-infos)
  (find-entry uid user-infos #'user-uid))

(defun find-user-by-name (name user-infos)
  (find-entry name user-infos #'user-name))

(defun find-users-by-gid (gid user-infos)
  (find-entries gid user-infos #'user-gid))

(defun find-group-by-gid (gid group-infos)
  (find-entry gid group-infos #'group-gid))

(defun find-group-by-name (name group-infos)
  (find-entry name group-infos #'group-name))

(defun group-members-by-group-users (group-info user-infos)
  (mapcar #'(lambda (name) (find-user-by-name name user-infos))
	  (split-sequence #\, (elt group-info 3))))

(defun group-members-by-primary-gid (gid user-infos)
  (find-users-by-gid gid user-infos))

(defun group-members (name user-infos group-infos)
  (let ((group-info (find-group-by-name name group-infos)))
    (sort
     (remove-duplicates 
      (append (group-members-by-group-users group-info user-infos)
	      (group-members-by-primary-gid (group-gid group-info)
					    user-infos))
      :test #'(lambda (u1 u2) (equal (user-uid u1) (user-uid u2))))
     #'(lambda (u1 u2) (string-lessp (user-name u1) (user-name u2))))))

(defun getent-user-infos ()
  (mapcar
   #'(lambda (user-string)
       (split-sequence #\: user-string))
   (remove-empty-lines
    (run/strings "getent" '("passwd")))))

(defun getent-group-infos ()
  (mapcar
   #'(lambda (group-string)
       (split-sequence #\: group-string))
   (remove-empty-lines
    (run/strings "getent" '("group")))))
