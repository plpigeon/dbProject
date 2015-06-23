(defpackage :com.monsterpidg.database
  (:use :common-lisp)
  (:export  :key
            :*db*
            ;class
            :table
            :column
            :column-with-key
            ;function
            :nullable
            :column-name
            :column-key-table
            :column-key-ref
            :table-name
            :table-headers
            :primary-key
            :find-table
            :rows
            :find-column
            :schema
            :schema-descriptor
            :extract-schema-descriptor
            :value-normalizer
            :make-table
            :make-table-from-serialized-table
            :select
            :matching
            :in
            :insert-row
            :update-rows
            :delete-rows
            :print-table
            :printable-table
            :save-db
            :load-db
            :validate-integer
            :validate-string
            :validate-number
            :validate-key
            :value-converter
            ;temp
            :row-normalizer
            :remove-tag-rows
            ;macro
            :define-tables
            ))

(in-package :com.monsterpidg.database)

(defparameter *db* '())


;;;---------------------------------------
;;; Table Class definition
;;;---------------------------------------
(defclass table ()
  ((name
     :reader table-name
     :initarg :name
     :initform "unnamed table")
   (rows
     :accessor rows
     :initarg :rows
     :initform '())
   (schema
     :accessor schema
     :initarg :schema
     :initform (error "must provide a schema"))
   (schema-descriptor
     :reader schema-descriptor
     :initarg :schema-descriptor
     :initform (error "must provide a schema descriptor"))
   (primary-key
     :reader primary-key
     :initarg :primary-key
     :initform nil)
   (unique-keys
     :reader unique-keys
     :initarg :unique-keys
     :initform '())
   (index
     :accessor table-index
     :initarg :table-index
     :initform 1)))

    
;;;---------------------------------------
;;; row-normalizer
;;;---------------------------------------
(defun primary-key-normalizer (normalized-row table)
  (let ((id-field (list (primary-key table) (table-index table))))
    (if (getf normalized-row (primary-key table))
      (error "It's not possible to declare yourself a PRIMARY-KEY")
      (append id-field normalized-row))))

(defun unique-keys-normalizer (normalized-row table)
  (let* ((unique-column
           (loop
             for key in (unique-keys table)
             collect key
             collect (getf normalized-row key)))
         (rows (select :columns (unique-keys table) :from table :where (eval `(matching ,table ,@unique-column)) :raw t)))
    (if rows
      (error "Row ~a already exist in table ~a with unique keys" normalized-row (table-name table))
      normalized-row)))


;;;---------------------------------------
;;; Column Class definition
;;;---------------------------------------
(defclass column ()
  ((name               
    :reader column-name
    :initarg :name)
   (equality-predicate
    :reader equality-predicate
    :initarg :equality-predicate)
   (comparator
    :reader comparator
    :initarg :comparator)
   (default-value
    :reader default-value
    :initarg :default-value
    :initform nil)
   (nullable
     :reader nullable
     :initarg :nullable
     :initform t)
   (value-normalizer
    :reader value-normalizer
    :initarg :value-normalizer
    :initform #'(lambda (v column) (declare (ignore column)) v))
   (value-printer
     :reader value-printer
     :initarg :value-printer
     :initform #'printer-default)
   (value-converter
     :reader value-converter
     :initarg :value-converter
     :initform #'(lambda (v column) (declare (ignore column)) v))))

(defclass column-with-key (column)
  ((table
     :reader column-key-table
     :initarg :column-key-table
     :initform (error "Must supply a table for the column key"))
   (column-key-ref
     :reader column-key-ref
     :initarg :column-key-ref
     :initform :id)))

;;;---------------------------------------
;;; Schemas
;;;---------------------------------------
(defun make-schema (spec)
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

(defun find-column (column-name schema)
  (or (find column-name schema :key #'column-name)
      (error "No column: ~a in schema: ~a" column-name schema)))

(defun find-table (table-name)
  (or (find table-name *db* :key #'table-name)
      (error "No table: ~a" table-name)))


;;;---------------------------------------
;;; Value converter
;;;---------------------------------------
(defun converter-integer (value column)
  (if (equal "" value)
    (default-value column)
    (parse-integer value)))

(defun converter-string (value column)
  (if (equal "" value)
    (default-value column)
    value))

(defun converter-number (value column)
  (if (equal "" value)
    (default-value column)
    (error "Number converter not supported yet")))


;;;---------------------------------------
;;; Table constructors
;;;---------------------------------------
(defun make-table (name schema-descriptor &key (primary-key nil) (unique-keys '()) (rows '()))
  (make-instance
    'table
    :name name
    ;*** devra vérifier si les rows répondent au schema
    :rows rows
    :schema-descriptor schema-descriptor
    :schema (make-schema schema-descriptor)
    :primary-key primary-key
    :unique-keys unique-keys))

;(defun make-table-from-serialized-table (serialize-table)
;  (let* ((schema-descriptor (getf serialize-table :schema-descriptor))
;         (id-field '(:id number))
;         (schema (cons id-field schema-descriptor)))
;    (make-instance
;      'table
;      :name (getf serialize-table :name)
;      :rows (getf serialize-table :rows)
;      :schema-descriptor schema-descriptor
;      :schema (make-schema schema)
;      :table-index (getf serialize-table :table-index))))


;;;---------------------------------------
;;; Column constructors
;;;---------------------------------------
(defgeneric make-column (name type nullable &key default-value column-key-table column-key-ref))

(defmethod make-column (name (type (eql 'string)) nullable &key default-value column-key-table column-key-ref)
  (make-instance
   'column 
   :name name
   :comparator #'string< 
   :equality-predicate #'string=
   :default-value default-value
   :nullable nil
   :value-normalizer #'validate-string
   :value-converter #'converter-string))

(defmethod make-column (name (type (eql 'integer)) nullable &key default-value column-key-table column-key-ref)
  (make-instance 
   'column
   :name name
   :comparator #'< 
   ;***need to change the comparators to handle 'nil, car sinon (= 3 nil) -> error
   :equality-predicate #'eql
   :default-value default-value
   :nullable nullable
   :value-normalizer #'validate-integer 
   :value-converter #'converter-integer))

(defmethod make-column (name (type (eql 'number)) nullable &key default-value column-key-table column-key-ref)
  (make-instance 
   'column
   :name name
   :comparator #'< 
   :equality-predicate #'eql
   :default-value default-value
   :nullable nullable
   :value-normalizer #'validate-number
   :value-converter #'converter-number))

(defmethod make-column (name (type (eql 'key)) nullable &key default-value column-key-table column-key-ref)
  (make-instance 
   'column-with-key
   :name name
   :comparator #'< 
   :equality-predicate #'eql
   :default-value default-value
   :nullable nullable
   :value-normalizer #'validate-key
   :value-printer #'printer-key-ref
   :value-converter #'converter-integer
   :column-key-table column-key-table
   :column-key-ref column-key-ref))

(defun validate-not-nullable (value column)
  (or value (error "Column ~a can't be null" (column-name column))))

(defun validate-integer (value column)
  (validate-type 'integer))

(defun validate-string (value column)
  (validate-type 'string))

(defun validate-number (value column)
  (validate-type 'number))

(defmacro validate-type (type)
  `(if value
     (if (typep value ,type)
          value
          (error "Column ~a must be of type ~a" (column-name column) ,type))
     value))

(defun validate-key (value column)
  (let ((table (find-table (column-key-table column))))
  (or (and (select :columns (column-key-ref column) :from table :where (matching table (primary-key table) value) :raw t :no-tag t)
           (validate-integer value column))
      (error "No key ~d in column ~a" value (column-name column)))))

(defun printer-default (value column)
  (format nil "~a" value))

(defun printer-key-ref (value column)
  (let ((table (find-table (column-key-table column))))
    (or (if (not value)
          (printer-default value column)
          (printer-default
            (select :columns (column-key-ref column) :from table :where (matching table (primary-key table) value) :raw t :no-tag t)
            column))
        (error "No key ~d in column ~a" value (column-name column)))))


;;;---------------------------------------
;;; INSERT-ROW
;;;---------------------------------------
(defun insert-row (names-and-values table)
  (let* ((normalized-row (normalize-row names-and-values table)))
    (with-accessors ((table-rows rows)) table
      (setf table-rows (cons normalized-row (rows table)))) 
    (with-accessors ((id table-index)) table
      (incf id))))

(defun normalize-row (names-and-values table)
  (let* ((row-values (normalize-row-values names-and-values table))
         (row-prim (if (primary-key table)
                     (normalize-row-primary-key row-values table)
                     row-values))
         (row-null (normalize-row-nullable row-prim (schema table)))
         (row-uniq (if (unique-keys table)
                     (normalize-row-unique-keys row-null table)
                     row-null)))
    row-uniq))

(defun normalize-row-values (names-and-values table)
  (loop
     for column in (remove-if #'(lambda (col) (eql (column-name col) (primary-key table))) (schema table))
     for name = (column-name column)
     for value = (or (getf names-and-values name) (default-value column))
     collect name
     collect (normalize-for-column value column)))

(defun normalize-row-primary-key (normalized-row table)
  (primary-key-normalizer normalized-row table))

(defun normalize-row-unique-keys (normalized-row table)
  (unique-keys-normalizer normalized-row table))

(defun normalize-for-column (value column)
  (funcall (value-normalizer column) value column))

(defun normalize-row-nullable (normalized-row schema)
  (loop
    for column in schema
    for name = (column-name column)
    for value = (or (getf normalized-row name) (default-value column))
    for validate = (if (not (nullable column))
                     (validate-not-nullable value column)))
  normalized-row)


;;;---------------------------------------
;;; SELECT
;;;---------------------------------------
(defun select (&key (columns t) from where distinct order-by no-tag raw (new-name :select-table))
  (let ((rows (rows from))
        (schema-descriptor (schema-descriptor from))
        (schema (schema from)))

    (when where
      (setf rows (restrict-rows rows where)))

    (unless (eql columns 't)
      (setf schema (extract-schema (mklist columns) schema))
      (setf schema-descriptor (extract-schema-descriptor (mklist columns) schema-descriptor))
      (setf rows (collect-columns rows schema)))

    ;(when distinct
    ;  (setf rows (distinct-rows rows schema)))

    ;(when order-by
    ;  (setf rows (sorted-rows rows schema (mklist order-by))))
    
    (if raw
      (if no-tag
        (simple-flatten (remove-tag-rows rows))
        (simple-flatten rows))
      (make-table new-name schema-descriptor :rows rows))))

(defun simple-flatten (rows)
  (if (= 1 (length rows))
    (if (= 1 (length (car rows)))
      (caar rows)
      (car rows))
    (if (= 1 (length (car rows)))
      (mapcar 'car rows)
      rows)))

(defun remove-tag-rows (rows)
  (mapcar #'remove-tag-row rows))

(defun remove-tag-row (row)
  (loop for c in row
       when (not (and (typep c 'symbol) (not (eql c nil)))) collect c))

(defun restrict-rows (rows where)
  (remove-if-not where rows))

(defun collect-columns (rows schema)
  (let ((names (mapcar #'column-name schema)))
    (mapcar
      #'(lambda (row) (loop for c in names collect c collect (getf row c)))
      rows)))

(defun mklist (thing)
  (if (listp thing) thing (list thing)))

(defun extract-schema (column-names schema)
  (loop for c in column-names collect (find-column c schema)))

;**loin d'être optimal
(defun extract-schema-descriptor (column-names schema)
  (loop for c in column-names collect (car (remove-if-not #'(lambda (col) (eql (car col) c)) schema))))


;;;---------------------------------------
;;; MATCHING and IN
;;;---------------------------------------
(defun matching (table &rest names-and-values)
  "Build a where function that matches rows with the given column values."
  (let ((matchers (column-matchers (schema table) names-and-values)))
    #'(lambda (row)
        (every #'(lambda (matcher) (funcall matcher row)) matchers))))

(defun column-matchers (schema names-and-values)
  (loop for (name value) on names-and-values by #'cddr
     ;when value collect
     collect
       (column-matcher (find-column name schema) value)))

(defun column-matcher (column value)
  (let ((name (column-name column))
        (predicate (equality-predicate column))
        (normalized (normalize-for-column value column)))
    #'(lambda (row) (funcall predicate (getf row name) normalized))))

(defun in (column-in column-from table)
  (let ((test (equality-predicate (find-column column-from (schema table))))
        (values (map 'list #'(lambda (r) (getf r column-from)) (rows table))))
    #'(lambda (row)
        (member (getf row column-in) values :test test))))


;;;---------------------------------------
;;; UPDATE-ROWS
;;;---------------------------------------
(defun update-rows (table names-and-values where)
  (let ((new-rows (mapcar
                    #'(lambda (row)
                        (if (funcall where row)
                          (normalize-row (loop
                                           for col in (table-headers table)
                                           for value = (if (find col names-and-values)
                                                         (getf names-and-values col)
                                                         (getf row col))
                                           collect col
                                           collect value)
                                         table)
                          row))
                    (rows table))))
    (with-accessors ((table-rows rows)) table
      (setf table-rows new-rows))))


;;;---------------------------------------
;;; DELETE-ROWS
;;;---------------------------------------
(defun delete-rows (table where)
  ;*** La suppression de valeur référencé par une clef n'est toujours pas géré
  ;(if (find t (mapcar #'(lambda (col) (typep col 'column-with-key)) (schema table)))
  ;  (error "Removing row from object column-with-key is not supported"))
  (with-accessors ((table-rows rows)) table
    (setf table-rows (remove-if where (rows table)))))


;;;---------------------------------------
;;; other function
;;;---------------------------------------
(defun printable-table (table &key raw)
  (let ((header (mapcar #'symbol-name (table-headers table)))
        (table-rows
          (if raw
            (rows table)
            (mapcar #'(lambda (row) (print-row row (schema table))) (rows table)))))
    (cons header table-rows)))

(defun print-table (table &key raw)
  (let ((header (table-headers table))
        (table-rows
          (if raw
            (rows table)
            (mapcar #'(lambda (row) (print-row row (schema table))) (rows table)))))
    (format t "~{~a   ~}~%" header)
    (format t "~{~{~*~a   ~}~%~}" table-rows)))

(defun print-row (row schema)
  (loop
     for column in schema
     for name = (column-name column)
     for value = (getf row name)
     collect name
     collect (print-for-column value column)))

(defun print-for-column (value column)
  (funcall (value-printer column) value column))

(defun table-headers (table)
  (mapcar #'column-name (schema table)))


;;;---------------------------------------
;;; Tables generation
;;;---------------------------------------
(defmacro define-tables ()
  (let ((def-tables (generate-tables *db*)))
     `(progn
        ,@def-tables)))

(defun generate-tables (db)
  (mapcar #'generate-table db))

(defun generate-table (table)
  `(defparameter ,(intern (concatenate 'string "*" (symbol-name (table-name table)) "*")) ,table))


;;;---------------------------------------
;;; SAVE\LOAD database
;;;---------------------------------------
;(defun serialize-table (table)
;  (list :name (table-name table)
;        :rows (rows table)
;        :schema-descriptor (schema-descriptor table)
;        :table-index (table-index table)))

;(defun save-table (filename table)
;  (with-open-file (out filename
;                       :direction :output
;                       :if-exists :supersede)
;    (with-standard-io-syntax
;       (print (serialize-table table) out))))

;(defun save-db (filename db)
;  (with-open-file (out filename
;                       :direction :output
;                       :if-exists :supersede)
;    (with-standard-io-syntax
;       (print (mapcar #'serialize-table db) out))))

;(defun load-table (filename)
;  (with-open-file (in filename)
;    (with-standard-io-syntax
;      (read in))))

;(defun load-db (filename)
;  (with-open-file (in filename)
;    (with-standard-io-syntax
;      (read in))))


