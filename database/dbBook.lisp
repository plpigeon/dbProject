(load "database.lisp")
(use-package :com.monsterpidg.database)

;(defparameter *file-db* "dbTest.db")

(defparameter *author-schema-descriptor*
  '((:name string nil)))

(defparameter *book-schema-descriptor*
  '((:title string nil)
    (:author key nil :column-key-table :author :column-key-ref :name)
    (:year integer t)))

;ici il serait interessant d'avoir un type de table "input"
;qui s'occuperait de générer les formulaire associé
;Et la même chose mais en "output" pour la tables "sales"
(defparameter *stock-schema-descriptor*
  '((:book key nil :column-key-table :book :column-key-ref :title)
    (:qt integer t :default-value 0)))

;Et la même chose mais en "output" pour la tables "sales"
(defparameter *sales-schema-descriptor*
  '((:book key nil :column-key-table :book :column-key-ref :title)
    (:qt integer nil :default-value 1)))


(defparameter *db* `(,(make-table :author *author-schema-descriptor*)
                     ,(make-table :book *book-schema-descriptor*)
                     ,(make-table :stock *stock-schema-descriptor*)
                     ,(make-table :sales *sales-schema-descriptor*)))

(define-tables)

