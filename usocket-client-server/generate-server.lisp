(in-package "COMMON-LISP-USER")

;;; --------------------------------------------------------------------
;;; Load the generator

(load (make-pathname :name "generate" :type "lisp" :version nil
                     :defaults (or *load-pathname* #P"./")))

;;; --------------------------------------------------------------------
;;; generate the program
;;;

(defparameter *source-directory*  (make-pathname :name nil :type nil :version nil
                                                 :defaults (or *load-pathname* (truename (first (directory #P"./*.lisp"))))))
(defparameter *asdf-directories*  (mapcar (lambda (path) (make-pathname :name nil :type nil :version nil :defaults path))
                                          (append (directory (merge-pathnames "**/*.asd" *source-directory* nil))
                                                  (list *source-directory*))))
(defparameter *release-directory* *source-directory* #|#P"HOME:bin;"|# "Where the executable will be stored." )

(generate-program :program-name "server"
                  :main-function "TEST.SERVER:MAIN"
                  :system-name "server"
                  :system-list '()
                  :init-file nil
                  :version "1.0.0"
                  :copyright (format nil "Copyright Pascal J. Bourguignon 2021 - 2021~%License: AGPL3")
                  :source-directory  *source-directory*
                  :asdf-directories  *asdf-directories*
                  :release-directory *release-directory*)

