(load "database.lisp")
(use-package :com.monsterpidg.database)

;(defparameter *file-db* "dbTest.db")

(defparameter *person-schema-descriptor* 
  '((:idPerson integer nil)
    (:name string nil)
    (:phone integer t)))

(defparameter *order-schema-descriptor* 
  '((:date number nil)
    (:person key nil :column-key-table :person :column-key-ref :name)))

;make database from file
(defparameter *db* `(,(make-table :person *person-schema-descriptor* :primary-key :idPerson)
                     ,(make-table :order *order-schema-descriptor*)))
;(defparameter *db* (mapcar #'make-table-from-serialized-table (load-db *file-db*)))


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

;(loop 
;  for i from 1 to 10000
;  do (insert-row (list :name "test" :phone i) *person*))

;select stataement
;(print-table (select :columns '(:id :name :phone) :from *person* :where (matching *person* :name "Shann")))
;(print-table (select :columns '(:date :person) :from *order* :where (in :person :id (select :columns :id :from *person* :where (matching *person* :name "Pierre")))))

;(update-rows *person* '(:phone 1112223333) (matching *person* :phone nil))
