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

(defun standard-name-displayer (table)
  (format nil "~:@(~a~)-DISPLAYER" (table-name table)))

(defun standard-page-name (table)
  (format nil "/~(~a~)" (standard-name table)))

(defun standard-page-name-adder (table)
  (format nil "/~(~a~)" (standard-name-adder table)))

(defun standard-page-name-remover (table)
  (format nil "/~(~a~)" (standard-name-remover table)))

(defun standard-page-name-displayer (table)
  (format nil "/~(~a~)" (standard-name-displayer table)))


;;;---------------------------------------
;;; Other fonction
;;;---------------------------------------
(defun build-list-parameters (param table)
  (loop
    for pair in param
    for tag = (intern (format nil "~:@(~a~)" (car pair)) "KEYWORD")
    for column = (find-column tag (schema table))
    for val = (if (equal "" (cdr pair))
                ""
                (funcall
                  (value-converter column)
                  (cdr pair)
                  column))
    when (not (equal val "")) collect tag
    when (not (equal val "")) collect val))


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
          *dispatch-table*)
    (push (create-prefix-dispatcher (standard-page-name-displayer table) (intern (standard-name-displayer table)))
          *dispatch-table*)))

(defun standard-table-adder (table)
  (lambda ()
     (let ((param (build-list-parameters (post-parameters*) table)))
       (print param)
       (insert-row param table)
       (redirect (standard-page-name table)))))

(defun standard-table-remover (table)
  (lambda ()
     (let* ((id-column (car (schema table)))
            (pair (car (post-parameters*)))
            (id (funcall (value-converter id-column) (cdr pair) id-column)))  
       (print "*********************")
       (print id)
       (delete-rows table (matching table (primary-key table) id))
       (redirect (standard-page-name table)))))



;;;---------------------------------------
;;; Standard page builder
;;;---------------------------------------
(defmacro define-standard-url ()
  (let ((fn-adder (mapcar
                    #'(lambda (table)
                        `(defun ,(intern (standard-name-adder table)) ()
                           (funcall ,(standard-table-adder table))))
                    *db*))
        (fn-remover (mapcar
                    #'(lambda (table)
                        `(defun ,(intern (standard-name-remover table)) ()
                           (funcall ,(standard-table-remover table))))
                    *db*))
        (fn-displayer (mapcar
                        #'(lambda (table)
                           `(defun ,(intern (standard-name-displayer table)) ()
                               (eval (funcall ,(standard-table-page-displayer table)))))
                        ;*** En fait, garder le modèle actuel, mais prendre le temps de lire les paremetre POST
                        ;       pour générer le where et la page
                        *db*))
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
       ,@fn-remover
       ,@fn-displayer
       ,@fn-viewer
       ,@dispatcher)))

(defun standard-table-page (table)
  (lambda ()
    (let ((header (standard-page-header))
          (display (standard-page-display table))
          (input (standard-page-input-form table))
          (remov (standard-page-remove-form table))
          )
      `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
         (:head 
            (:title ,(standard-name table))
          ; Load les fichier css
            (:link :rel "stylesheet" :href "../css/grid.css")
            (:link :rel "stylesheet" :href "../css/style.css")
            (:link :rel "stylesheet" :href "../css/alpha_background.css")
            (:link :rel "stylesheet" :type "text/css" :href "http://yui.yahooapis.com/3.8.0/build/cssreset/cssreset-min.css")
            (:link :type "text/css" :rel "stylesheet" :href "http://fonts.googleapis.com/css?family=PT+Serif:400,700,400italic,700italic|Open+Sans:400italic,700italic,400,700")
         )
         (:body
           ,@header
           ,@display
           ,@input
           ,@remov
           )
        )
    )
  )
)

;;;---------------------------------------
;;; Standard header
;;;---------------------------------------
(defun standard-page-header ()
  (let* (
         (name (mapcar #'standard-name *db*))
         (url (mapcar #'standard-page-name *db*))
         (link (mapcar  #'(lambda (name url) `(:li :class "menu-item" (:a :href ,url ,name))) name url))
        )

`((:div :id "section-standard-header"
    (:div :id "section-standard-header-inner"
      (:div :class "container"
        (:div :class "sixteen columns"
            (:ul :id "nav" :class "nav"
              ,@link
            )
          (:div :class "clear")
        ) ; // six
      ) ; // container
      (:div :class "clear")
    ) ; // div inner
  ) ; // div header
) ; // `
))

;; (hunchentoot:start *server*)
;;;---------------------------------------
;;; Standard display
;;;---------------------------------------
(defun standard-page-display (table)
  (let* ((tab (printable-table table))
         (header (standard-page-display-header (car tab)))
         (rows (mapcar
                 #'(lambda (row) (standard-page-display-row row (table-headers table)))
                 (cdr tab)))
         (page-name (standard-page-name-displayer table))
         (frame-name "display-frame")
         (inputs (mapcar #'display-field (schema table))))

`((:div :id "section-standard-display"
        (:div :class "container"
          (:div :class "eight columns"
              (:iframe :name ,frame-name :width "300" :heigth "300" :src ,page-name)
          ) ; // div eight
          (:div :class "eight columns"
                (:form :action ,page-name :method "get" :target ,frame-name ,@inputs (:p (:input :type "submit" :value "Search")))
          ) ; // div eight
    ) ; // div container
  ) ; // div display
) ; // `
))

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
;;; Standard input
;;;---------------------------------------
(defun standard-page-input-form (table)
  (let* ((prim-key (primary-key table))
         (inputs (mapcar #'input-field (remove-if #'(lambda (col) (eql prim-key (column-name col))) (schema table))))
         (page-name (standard-page-name-adder table)))
`(
  (:div :id "section-standard-input"
    (:div :class "container"
      (:div :class "five columns"
      (:h3 "Ajout")
      (:form :action ,page-name :method "post"
            ,@inputs
            (:p (:input :type "submit"
                        :value "Add")))
      ) ; // div five
    ) ; // div container
  ) ; // div input
) ; // `
))

(defun input-field (column)
  (let ((name (symbol-name (column-name column)))
        (input (input-type-and-attribute column :include-nullable t)))
    `(:p ,name (:br)
         ,input)))

(defun input-type-and-attribute (column &key (include-nullable nil))
  (let ((type (value-normalizer column))
        (null (if (and include-nullable (not (nullable column)))
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

;;;---------------------------------------
;;; Standard remove
;;;---------------------------------------
(defun standard-page-remove-form (table)
  (if (primary-key table)
    (let ((field (remove-field table))
          (page-name (standard-page-name-remover table)))
`(
  (:div :id "section-standard-remove"
    (:div :class "container"
      (:div :class "five columns"

  (:h3 "Suppression")
        (:form :action ,page-name :method "post"
              ,field
              (:p (:input :type "submit"
                          :value "Remove")))
      ) ; // div five
    ) ; // div container
  ) ; // div remove
) ; // `
)

`(
  (:div :id "section-standard-remove"
    (:div :class "container"
      (:div :class "five columns"

      (:h3 "Suppression")
      (:p "Suppression : not yet supported for table without a primary-key")

      ) ; // div five
    ) ; // div container
  ) ; // div remove
) ; // `
))

(defun remove-field (table)
  (let* ((rows (select :columns (primary-key table) :from table :raw t :no-tag t))
         (options (mapcar
                   #'(lambda (row) `(:option :value ,(write-to-string row) ,(write-to-string row)))
                   rows))
         (name  (format nil "~(~a~)" (table-name table))))
    `(:p "ID" (:br)
         (:select :name ,name
                  :required "0"
                  (:option :value "" "Please select")
                  ,@options))))


;;;---------------------------------------
;;; Standard page displayer
;;;---------------------------------------
(defun standard-table-page-displayer (table)
  (lambda ()
    (let* ((param (build-list-parameters (get-parameters*) table))
           (fn-match `(matching ,table ,@param))
           (tab (printable-table (select :from table :where (eval fn-match))))
           (header (standard-page-display-header (car tab)))
           (rows (mapcar
                   #'(lambda (row) (standard-page-display-row row (table-headers table)))
                   (cdr tab))))
      (print param)
      (print tab)
      `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
         (:head (:title ,(standard-name table)))
         (:body
           (:table ,header ,@rows))))))

(defun display-field (column)
  (let ((name (symbol-name (column-name column)))
        (input (input-type-and-attribute column)))
    `(:p ,name (:br)
         ,input)))






