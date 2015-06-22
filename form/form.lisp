(defparameter *path* "~/Documents/dbProject/")

(load "~/quicklisp/setup.lisp")
(load (concatenate 'string *path* "database/database.lisp"))

(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(defpackage :com.monsterpidg.form
  (:use :cl
        :cl-who
        :hunchentoot
        :com.monsterpidg.database)
  (:export :define-standard-url
           :*server*
           ;from Hunchentoot
           :easy-acceptor))
(in-package :com.monsterpidg.form)


(SETF (HTML-MODE) :HTML5)

(defparameter *server* nil)


;;;---------------------------------------
;;; Naming fonction
;;;---------------------------------------
(defun standard-name (table)
  (format nil "~:@(~a~)" (table-name table)))

(defun standard-name-adder (table)
  (format nil "~:@(~a~)-ADDER" (table-name table)))

(defun standard-name-remover (table)
  (format nil "~:@(~a~)-REMOVER" (table-name table)))

(defun standard-page-name (table)
  (format nil "/~(~a~)" (standard-name table)))

(defun standard-page-name-adder (table)
  (format nil "/~(~a~)" (standard-name-adder table)))

(defun standard-page-name-remover (table)
  (format nil "/~(~a~)" (standard-name-remover table)))


;;;---------------------------------------
;;; Dispatchers
;;;---------------------------------------
(defun standard-table-dispatcher (table)
  (lambda ()
    (push (create-prefix-dispatcher (standard-page-name table) (intern (standard-name table)))
          *dispatch-table*)
    (push (create-prefix-dispatcher (standard-page-name-adder table) (intern (standard-name-adder table)))
          *dispatch-table*)
    (push (create-prefix-dispatcher (standard-page-name-remover table) (intern (standard-name-remover table)))
          *dispatch-table*)))

(defun standard-table-adder (table)
  (lambda ()
     (let ((param ;(post-parameters*)))
             (loop
               for pair in (post-parameters*);'(("name" . "test") ("phone" . "3"))
               for tag = (intern (format nil "~:@(~a~)" (car pair)) "KEYWORD")
               for column = (find-column tag (schema table))
               for val = (funcall
                            (value-converter column)
                            (cdr pair)
                            column)
               collect tag
               collect val)))
       (print param)
       (insert-row param table)
       (redirect (standard-page-name table)))))

;(defun standard-table-remover (table)
;  (lambda ()
;     (let* ((id-column (car (schema table)))
;            (pair (car (post-parameters*)))
;            (id (funcall (value-converter id-column) (cdr pair) id-column)))  
;       (print "*********************")
;       (print id)
;       (delete-row id table)
;       (redirect (standard-page-name table)))))


;;;---------------------------------------
;;; Standard page builder
;;;---------------------------------------
(defmacro define-standard-url ()
  (let ((fn-adder (mapcar
                    #'(lambda (table)
                        `(defun ,(intern (standard-name-adder table)) ()
                           (funcall ,(standard-table-adder table))))
                    *db*))
        ;(fn-remover (mapcar
        ;            #'(lambda (table)
        ;                `(defun ,(intern (standard-name-remover table)) ()
        ;                   (funcall ,(standard-table-remover table))))
        ;            *db*))
        (fn-viewer (mapcar
                     #'(lambda (table)
                         `(defun ,(intern (standard-name table)) ()
                            (eval (funcall ,(standard-table-page table)))))
                     *db*))
        (dispatcher (mapcar
                       #'(lambda (table)
                           `(funcall ,(standard-table-dispatcher table)))
                       *db*)))   
    `(progn
       ,@fn-adder
       ;,@fn-remover
       ,@fn-viewer
       ,@dispatcher)))

(defun standard-table-page (table)
  (lambda ()
    (let ((header (standard-page-header))
          (display (standard-page-display table))
          (input (standard-page-input-form table))
          ;(remov (standard-page-remove-form table))
          )
      `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
         (:head (:title ,(standard-name table)))
         (:body
           ,@header
           ,@display
           ,@input
           ;,@remov
           )))))


;;;---------------------------------------
;;; Standard header
;;;---------------------------------------
(defun standard-page-header ()
  (let* ((name (mapcar #'standard-name *db*))
         (url (mapcar #'standard-page-name *db*))
         (link (mapcar  #'(lambda (name url) `(:a :href ,url ,name)) name url)))
    `(,@link
      (:hr))))


;;;---------------------------------------
;;; Standard display
;;;---------------------------------------
(defun standard-page-display (table)
  (let* ((tab (printable-table table))
         (header (standard-page-display-header (car tab)))
         (rows (mapcar
                 #'(lambda (row) (standard-page-display-row row (table-headers table)))
                 (cdr tab))))
    `((:table ,header ,@rows)
      (:hr))))

(defun standard-page-display-header (headers)
  `(:tr
     ,@(mapcar
        #'(lambda (header) `(:th ,header))
        headers)))

(defun standard-page-display-row (row headers)
  `(:tr
     ,@(mapcar 
         #'(lambda (header) `(:td ,(getf row header)))
         headers)))


;;;---------------------------------------
;;; Standard remove
;;;---------------------------------------
;(defun standard-page-remove-form (table)
;  (let ((field (remove-field table))
;        (page-name (standard-page-name-remover table)))
;    `((:h3 "Suppression")
;      (:form :action ,page-name :method "post"
;            ,field
;            (:p (:input :type "submit"
;                        :value "Remove")))
;      (:hr))))

;(defun remove-field (table)
;  (let ((options (mapcar
;                  #'(lambda (row) `(:option :value ,(write-to-string (getf row :id)) ,(write-to-string (getf row :id))))
;                  (rows table)))
;        (name  (format nil "~(~a~)" (table-name table))))
;    `(:p "ID" (:br)
;         (:select :name ,name
;                  :required "0"
;                  (:option :value "" "Please select")
;                  ,@options))))


;;;---------------------------------------
;;; Standard input
;;;---------------------------------------
(defun standard-page-input-form (table)
  (let* ((prim-key (primary-key table))
         (inputs (mapcar #'input-field (remove-if #'(lambda (col) (eql prim-key (column-name col))) (schema table))))
         (page-name (standard-page-name-adder table)))
    `((:h3 "Ajout")
      (:form :action ,page-name :method "post"
            ,@inputs
            (:p (:input :type "submit"
                        :value "Add")))
      (:hr))))

(defun input-field (column)
  (let ((name (symbol-name (column-name column)))
        (input (input-type-and-attribute column)))
    `(:p ,name (:br)
         ,input)))

(defun input-type-and-attribute (column)
  (let ((type (value-normalizer column))
        (null (if (not (nullable column))
                '(:required "0")
                '()))
        (name  (format nil "~(~a~)" (column-name column))))
    (cond ((eql type #'validate-integer)
           `(:input :type "number"
                    :min "0"
                    :step "1"
                    ,@null 
                    :name ,name))
          ((eql type #'validate-number)
           `(:input :type "number"
                    :step "0.000001"
                    ,@null
                    :name ,name))
          ((eql type #'validate-string)
           `(:input :type "text"
                    ,@null
                    :name ,name))
          ((eql type #'validate-key)
           (let ((options (get-id-tag column)))
             `(:select :name ,name
                       ,@null
                       (:option :value "" "Please select")
                       ,@options)))
          (t
            (error "In : input-type-and-attribute, unknown normalizer type ~a" type)))))

(defun get-id-tag (column)
  (let* ((table (find-table (column-key-table column)))
         (col-name (column-key-ref column))
         (rows (select :columns (list (primary-key table) col-name) :from table :raw t :no-tag t)))
    (mapcar
      #'(lambda (row) `(:option :value ,(write-to-string (car row)) ,(cadr row)))
      rows)))



