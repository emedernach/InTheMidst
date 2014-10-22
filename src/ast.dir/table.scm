;; Copyright (C) 2013-2014 Emmanuel Medernach
;;
;; This file is part of "In The Midst".
;;
;; "In The Midst" is  free software: you can redistribute it
;; and/or  modify  it under  the  terms  of  the GNU  Lesser
;; General Public License as  published by the Free Software
;; Foundation, either version 3  of the License, or (at your
;; option) any later version.
;; 
;; "In The Midst" is distributed in the hope that it will be
;; useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the
;; implied  warranty  of MERCHANTABILITY  or  FITNESS FOR  A
;; PARTICULAR  PURPOSE.  See the  GNU Lesser  General Public
;; License for more details.
;; 
;; You should have received a copy of the GNU Lesser General
;; Public License  along with "In  The Midst".  If  not, see
;; <http://www.gnu.org/licenses/>.

;;; Author: Emmanuel Medernach



;; A field could be :
;; <field name>
;; <table name> . <field name>


(define-record-type :field
  (field_ table column)
  field?
  (table field->table)
  (column field->column))

(define (field table column)
  (if (or (symbol? table)
          (string? table))
      (field_ table column)
      (error "field: unknown table type."
             (list table column))))

(define (field-equal? f1 f2)
  (define (compare x y)
    (or (and (symbol? x) (symbol? y)
             (eq? x y))
        (and (string? x) (string? y)
             (string-ci=? x y))))
  (let ((f1-table (field->table f1))
        (f2-table (field->table f2)))
    (and (compare f1-table f2-table)
         (let ((f1-column (field->column f1))
               (f2-column (field->column f2)))
           (compare f1-column f2-column)))))
        

(define-record-type :alias
  ;; 'alias' already exists so we use 'make-alias'

  ;; In Scheme, we name object directly as (vector
  ;;  ...) or  (list ...)   and usually  don't use
  ;; make-* names.
  
  (make-alias_ name value)
  alias?
  (name  alias->name)
  (value alias->value))

(define (make-alias name value)
  (if (or (alias? value)
          (and (parenthesised-expression? value)
               (alias? (parenthesised-expression->expr value))))
      (error "make-alias: cannot make an alias from an alias"
             (list name value))
      (make-alias_ name value)))

(define (make-shadow-alias name value)
  (let remove-alias ((value value))
    (cond ((alias? value) (remove-alias (alias->value value)))
          ((and (parenthesised-expression? value)
                (alias? (parenthesised-expression->expr value)))
           (remove-alias (parenthesised-expression->expr value)))
          (else (make-alias_ name value)))))

;; Table alias should be surrounded by parenthesis
;; but not field aliases !
;; (define (make-alias name value)
;;   (cond ((field? value) (make-alias_ name value))
;;         ((parenthesised-expression? value) (make-alias_ name value))
;;         (else (error "Alias should be surrounded by parenthesis"
;;                      (pp (list name value))))))

;; A table could be :
;; <table name>
;; <database> . <table name>

;; BEWARE:  in  the second  pass  we  should do  a
;; lookup  of  this  table  name  in  our  current
;; database schema environment.
(define-record-type :table-record
  (table-record database name)
  table-record?
  (database table-record->database)
  (name table-record->name))

;; Either we  join on  common fields listed  or we
;; join on 2 fields with an equality constraint.
(define-record-type :join-table-using
  (join-table-using_ type left-table right-table join-fields)
  join-table-using?
  (type        join-table-using->type)
  (left-table  join-table-using->left-table)
  (right-table join-table-using->right-table)  
  (join-fields join-table-using->join-fields))

(define (join-table-using type left-table right-table join-fields)
  
  (define (empty?)
    (case type
      ((inner)
       (cond ((eq? left-table 'empty-table) right-table)
             ((eq? right-table 'empty-table) left-table)
             (else (verify-fields))))
      ((left)
       (cond ((eq? left-table 'empty-table) right-table)
             (else (verify-fields))))
      ((right)
       (cond ((eq? right-table 'empty-table) left-table)
             (else (verify-fields))))
      ((full) (verify-fields))))

  (define (verify-fields)
    (let loop ((tmp join-fields))
      (if (null? tmp)
          (join-table-using_ type left-table right-table join-fields)
          (let ((head (car tmp))
                (others (cdr tmp)))
            (if (field? head)
                (loop others)
                (error "join-table-using: Not a field list."
                       join-fields))))))
  (empty?))

(define-record-type :join-table-on
  (join-table-on type left-table right-table predicate)
  join-table-on?
  (type join-table-on->type)
  (left-table  join-table-on->left-table)
  (right-table join-table-on->right-table)
  (predicate   join-table-on->predicate))

;; These  partial  structures (join-table-using-partial  and
;; join-table-on-partial)    are    used   internally    for
;; incrementally building corresponding tables.
(define-record-type :join-table-using-partial
  (join-table-using-partial type table join-fields)
  join-table-using-partial?
  (type        join-table-using-partial->type)
  (table       join-table-using-partial->table)
  (join-fields join-table-using-partial->join-fields))

(define-record-type :join-table-on-partial
  (join-table-on-partial type table predicate)
  join-table-on-partial?
  (type      join-table-on-partial->type)
  (table     join-table-on-partial->table)
  (predicate join-table-on-partial->predicate))

(define-record-type :table-combinator
  (table-combinator_ table list-of-table-combinators)
  table-combinator?
  (table    table-combinator->table)
  (list-of-table-combinators table-combinator->list-of-table-combinators))

(define (table-combinator table list-of-table-combinators)
  (cond ((alias? table) (error "table-combinator: should not be an alias" table))
        ((table-record? table)
         ;; We add "SELECT * FROM <table>" around it.
         (let* ((all-fields (projection #f (list (field 'unknown "*"))))
                (wrapped-table 
                 (sql-record
                  all-fields table
                  'empty-where 'empty-group
                  'empty-order-by 'empty-limit)))
           (table-combinator_
            wrapped-table
            list-of-table-combinators)))
        (else (table-combinator_ table list-of-table-combinators))))

(define-record-type :table-combinator-partial
  (table-combinator-partial_ combinator table)
  table-combinator-partial?
  (combinator    table-combinator-partial->combinator)
  (table         table-combinator-partial->table))

(define (table-combinator-partial combinator table)
  (cond ((alias? table) (error "table-combinator-partial: should not be an alias" table))
        ((table-record? table)
         (let* ((all-fields (projection #f (list (field 'unknown "*"))))
                (wrapped-table 
                 (sql-record
                  all-fields table
                  'empty-where 'empty-group
                  'empty-order-by 'empty-limit)))
           (table-combinator-partial_ combinator wrapped-table)))
        (else (table-combinator-partial_ combinator table))))

;; ---------------------------------------- ;;

;; Inner  joins  are associative  so  we create  a
;; special  AST  structure  for  series  of  inner
;; joins.

(define-record-type :inner-joins
  (inner-joins_ table-list join-fields-list)
  inner-joins?
  (table-list        inner-joins->table-list)
  (join-fields-list  inner-joins->join-fields-list))

(define (inner-joins table-list join-fields-list)
  (cond ((< (length table-list) 2) (error "inner-joins: need at least 2 tables !" table-list))
        ((null? join-fields-list)  (error "inner-joins: join-fields-list is empty !"))
        (else (inner-joins_ table-list join-fields-list))))

;; ---------------------------------------- ;;

(define-record-type :cartesian-product
  (cartesian-product table-list)
  cartesian-product?
  (table-list cartesian-product->tables))

;; ---------------------------------------- ;;

(define (replace-table-with from table-name replacement)

  ;;  replacement  is  a  function  taking  a  table  to  be
  ;; replaced, an alias is added.
  
  (define (replace from)
    (cond ((table-record? from)
           (if (string-ci=?
                table-name
                (table-record->name from))
               (make-alias
                table-name
                (replacement from))
               from))

          ((alias? from)
           (if (string-ci=?
                table-name
                (alias->name from))
               (let ((content (alias->value from)))
                 ;; content must be a string naming a distributed table !
                 (if (table-record? content)
                     (let ((new-content (replacement content)))
                       (make-alias table-name new-content))
                     (error "replace-table-with-chunk: alias to replace is not a table" content)))
               from))
          
          ((cartesian-product? from)
           (let ((tables (cartesian-product->tables from)))
             (cartesian-product 
              (map replace tables))))

          ((join-table-using? from)
           (let ((type        (join-table-using->type from))
                 (left-table  (join-table-using->left-table from))
                 (right-table (join-table-using->right-table from))
                 (join-fields (join-table-using->join-fields from)))
             (join-table-using type
                               (replace left-table)
                               (replace right-table)
                               join-fields)))
          
          (else (error "replace-table-with-chunk: unknown type:" from))))

  (replace from))

