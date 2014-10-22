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



(define (insert-predicate table pred table->fields description)
  ;; table is an AST representing a table
  ;; We wish to insert the predicate pred inside it
  ;; But SQL disallow anonymous tables so we have to make an alias if necessary

  (cond ((sql-subquery? table)
         (insert-predicate-in-sql-subquery
          table pred table->fields description))
        ((combined-subquery? table)
         (insert-predicate-in-combined-subquery
          table pred table->fields description)) 
        ((distributed-subquery? table)
         (let* ((alias-name (alias->name table))
                (content (alias->value table))
                (query (parenthesised-expression->expr content)))
           (make-subquery (insert-predicate
                           query pred
                           table->fields description)
                          alias-name))) 
        ((distributed-query? table)
         (let ((query  (distributed-query->query table))
               (key    (distributed-query->key table))
               (number (distributed-query->number table)))
           (distributed-query
            (insert-predicate
             query pred
             table->fields description)
            key number)))
        ((join-table-using? table)
         (let* ((tmp (distribute-predicate-in-from-clause
                      table pred table->fields description))
                (new-table (vector-ref tmp 0))
                (success?  (vector-ref tmp 1)))
           (if success?
               new-table
               (wrap-table-with-predicate table pred))))
        ((sql-record? table)
         (insert-predicate-in-sql-record
          table pred
          table->fields description))
        ((table-record? table)
         (let* ((name (table-record->name table))
                (proj (projection #f (list (field 'unknown "*"))))
                (query (sql-record proj table pred
                                   'empty-group
                                   'empty-order-by
                                   'empty-limit)))
           (make-subquery query name )))
        ((alias? table) ;; table)
         (let ((alias-name (alias->name table))
               (content (alias->value table)))
           (make-alias
            alias-name
            (parenthesised-expression 
             (sql-record
              (projection #f (list (field 'unknown "*")))
              content
              pred
              'empty-group
              'empty-order-by
              'empty-limit)))))
        ((parenthesised-expression? table)
         (parenthesised-expression
          (insert-predicate
           (parenthesised-expression->expr
            table)
           pred
           table->fields
           description)))
        
        (else (error "insert-predicate: unknown table type"
                     (list table pred)))))

(define (insert-predicate-in-sql-subquery
         table pred
         table->fields description)
  ;; Then this is a subquery.
  ;; We should avoid this:
  ;; "SELECT * FROM (SELECT * FROM (SELECT * FROM T1 WHERE a = 123) AS T1 WHERE b > a) AS T1, T2 ;"    
  (let* ((name    (alias->name table))
         (content (alias->value table))
         (query (parenthesised-expression->expr content)))
    (let ((select    (sql-record->select  query))
          (from      (sql-record->from  query))
          (where     (sql-record->where  query))
          (group-by  (sql-record->group-by  query))
          (order-by  (sql-record->order-by  query))
          (limit     (sql-record->limit  query)))
      (let* ((new-pred  (rename-predicate pred))
             (new-where (insert-predicate-in-where
                         where new-pred
                         table->fields description
                         ))
             (ast (sql-record select from new-where group-by order-by limit))
             (subquery (make-subquery ast name)))
        (distribute-predicate subquery table->fields description)))))

(define (insert-predicate-in-sql-record
         ast pred
         table->fields description)
  (let ((select    (sql-record->select  ast))
        (from      (sql-record->from  ast))
        (where     (sql-record->where  ast))
        (group-by  (sql-record->group-by  ast))
        (order-by  (sql-record->order-by  ast))
        (limit     (sql-record->limit  ast)))
    (let* ((new-where (insert-predicate-in-where
                       where pred
                       table->fields description))
           (new-sql (sql-record select from new-where group-by order-by limit)))
      ;; (display (list 'insert-predicate-in-sql-record (AST->SQL ast) (AST->SQL pred))) (newline)
      (distribute-predicate new-sql table->fields description))))

(define (insert-predicate-in-where
         where pred
         table->fields description)
  (let ((pred-list (where->and-list where)))
    (if (null? pred-list)  pred
        (apply and-predicate pred pred-list))))

(define (insert-predicate-in-combined-subquery
         ast pred
         table->fields description)
  (let* ((name  (alias->name ast))
         (content (alias->value ast))
         (query (parenthesised-expression->expr content))
         (table  (table-combinator->table query))
         (others (table-combinator->list-of-table-combinators query))
         (new-pred (rename-predicate pred)))

    (cond ((sql-record? table)
           (let* ((new-table (insert-predicate-in-sql-record
                              table new-pred
                              table->fields description))
                  (partial-list (map (lambda (x)
                                       (let* ((combinator (table-combinator-partial->combinator x))
                                              (table      (table-combinator-partial->table x))
                                              (new-pred   (rename-predicate pred)))
                                         (cond ((sql-record? table)
                                                (table-combinator-partial
                                                 combinator 
                                                 (insert-predicate-in-sql-record
                                                  table new-pred
                                                  table->fields description)))
                                               (else (error "combined-subquery-dispatch: unknown type" table)))))
                                     others))
                  (new-ast (table-combinator new-table partial-list)))
             (make-subquery new-ast name)))
          (else (error "insert-predicate-in-combined-subquery: unknown type" table)))))  
  


