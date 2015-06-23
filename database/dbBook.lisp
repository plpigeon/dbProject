(load "database.lisp")
(use-package :com.monsterpidg.database)

;(defparameter *file-db* "dbTest.db")

(defparameter *author-schema-descriptor*
  '((:id integer nil)
    (:name string nil)))

(defparameter *book-schema-descriptor*
  '((:id integer nil)
    (:title string nil)
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


(defparameter *db* (list
                     (make-table :author *author-schema-descriptor*
                                 :primary-key :id)
                     (make-table :book *book-schema-descriptor*
                                 :primary-key :id)
                     (make-table :stock *stock-schema-descriptor*
                                 :unique-keys '(:book :qt))
                     (make-table :sales *stock-schema-descriptor*
                                 :unique-keys '(:book))))

(define-tables)

;sample
(insert-row '(:name "author1") *author*)
(insert-row '(:name "author2") *author*)
(insert-row '(:name "author3") *author*)
(insert-row '(:title "book1" :author 1) *book*)
(insert-row '(:title "book2" :author 2) *book*)
(insert-row '(:title "book3" :author 3) *book*)
(insert-row '(:book 1) *stock*)
(insert-row '(:book 2) *stock*)

