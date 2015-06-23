(defparameter *path* "~/Documents/dbProject/")

(load "~/quicklisp/setup.lisp")
(load (concatenate 'string *path* "database/database.lisp"))

;(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(use-package 'cl-who)

(defun test ()
  (with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:iframes :url "test")))
