;;;; -*- mode: lisp -*-

(in-package :ru.bazon.server-libs.getent)

(defun find-entries (entries predicatefn)
  (remove-if-not
   #'(lambda (entry)
       (funcall predicatefn entry))
   entries))

(defun find-entries-by-key (key entries entry-key-fn)
  (find-entries
   entries
   #'(lambda (entry)
       (equal key (funcall entry-key-fn entry)))))

(defun find-entry-by-key (key entries entry-key-fn)
  (let ((entries (find-entries-by-key key entries entry-key-fn)))
    (cond ((eq '() entries) nil)
	  ((= 1 (length entries)) (first entries))
	  (t (error "Multiple occuencies (~a) for key ~a" entries key)))))

(defstruct user
  (uid nil :type integer)
  (gid nil :type integer)
  (name nil :type string)
  (fullname nil :type string)
  (homedir nil :type string)
  (shell nil :type string))

(defstruct group
  (gid nil :type integer)
  (name nil :type string)
  (member-names '() :type list))

(defun getent-users ()
  (remove-if
   #'(lambda (x) (eq x :empty))
   (mapcar
    #'(lambda (user-string)
	(if (not (equal user-string ""))
	    (let ((user-list (split-sequence #\: user-string)))
	      (make-user
	       :uid (parse-integer (elt user-list 2))
	       :gid (parse-integer (elt user-list 3))
	       :name (elt user-list 0)
	       :fullname (elt user-list 4)
	       :homedir (elt user-list 5)
	       :shell (elt user-list 6)))
	    :empty))
    (run/strings "getent" '("passwd")))))

(defun getent-groups ()
  (remove-if
   #'(lambda (x) (eq x :empty))
   (mapcar
    #'(lambda (group-string)
	(if (not (equal group-string ""))
	    (let ((group-list (split-sequence #\: group-string)))
	      (make-group
	       :gid (parse-integer (elt group-list 2))
	       :name (elt group-list 0)
	       :member-names (loop for username in (split-sequence
						    #\, (elt group-list 3)
						    :remove-empty-subseqs t)
				collect (string-trim '(#\Space) username))))
	    :empty))
    (run/strings "getent" '("group")))))

(defun find-user-by-uid (uid &optional (users (getent-users)))
  (find-entry-by-key uid users #'user-uid))

(defun find-user-by-name (name &optional (users (getent-users)))
  (find-entry-by-key name users #'user-name))

(defun find-group-by-gid (gid &optional (groups (getent-groups)))
  (find-entry-by-key gid groups #'group-gid))

(defun find-group-by-name (name &optional (groups (getent-groups)))
  (find-entry-by-key name groups #'group-name))

(defun user-groups (user &optional
		    (groups (getent-groups)))
  (union
   (list (find-group-by-gid (user-gid user) groups))
   (find-entries
    groups
    #'(lambda (entry)
	(find (user-name user)
	      (group-member-names entry) :test #'equal)))
   :test #'(lambda (group1 group2)
	     (eql (group-gid group1) (group-gid group2)))))

(defun group-users (group &optional
		    (users (getent-users)))
  (union
   (loop for username in (group-member-names group)
      for user = (find-user-by-name username users)
      when (not (null user)) collect user)
   (find-entries
    users
    #'(lambda (entry) (eql (user-gid entry)
			   (group-gid group))))
   :test #'(lambda (user1 user2)
	     (eql (user-uid user1) (user-uid user2)))))
