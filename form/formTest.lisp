(defparameter *path* "~/Documents/dbProject/")

(load "form.lisp")
(load (concatenate 'string *path* "database/database.lisp"))
(use-package :com.monsterpidg.form)
(use-package :com.monsterpidg.database)

; l'ajout de ":document-root" permet au serveur de retrouver facilement images et css)
(defparameter *server* (make-instance 'easy-acceptor :port 8080 :document-root #p "~/Documents/dbProject/"))

;;;---------------------------------------
;;; Database definition
;;;---------------------------------------
(defparameter *person-schema-descriptor* 
  '((:idPerson integer nil)
    (:name string nil)
    (:phone integer t)))

(defparameter *order-schema-descriptor* 
  '((:id integer nil)
    (:date integer nil)
    (:person key nil :column-key-table :person :column-key-ref :name)))

(defparameter *test-schema-descriptor* 
  '((:name string nil)))

;;;---------------------------------------
;;; Load database
;;;---------------------------------------
(defparameter *db* `(,(make-table :person *person-schema-descriptor* :primary-key :idPerson)
                     ,(make-table :order *order-schema-descriptor* :primary-key :id)
                     ,(make-table :test *test-schema-descriptor* )))

(define-tables)

;sample
(insert-row '(:name "Shann" :phone 5145254887) *person*)
(insert-row '(:name "Shann" :phone 5145254444) *person*)
(insert-row '(:name "Shann") *person*)
(insert-row '(:name "Pierre") *person*)
(insert-row '(:name "Mario" :phone 4507956508) *person*)
(insert-row '(:date 11 :person 1) *order*)
(insert-row '(:date 22 :person 2) *order*)
(insert-row '(:date 33 :person 3) *order*)
(insert-row '(:date 44 :person 4) *order*)
(insert-row '(:date 55 :person 5) *order*)
(insert-row '(:name "test") *test*)
(insert-row '(:name "test1") *test*)

;(loop 
;  for i from 1 to 10000
;  do (insert-row (list :name "test" :phone i) *person*))


(define-standard-url)
