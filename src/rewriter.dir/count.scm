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




(include "../macros.dir/macros.scm")
(include "../ast.dir/ast-macros.scm")

;;      (export count-rewriter )


;; select count(*) from Object where <pred> ;
;; =>
;; select c1 + c2 + ...
;; from (select count(*) as c1 from master_object_000 where <pred>) as q1,
;;      (select count(*) as c2 from master_object_001 where <pred>) as q2,
;;      ...
;;      ;

;; select count(*), <fields>, ... from Object where <pred> ;
;; =>
;; select c1 + c2 + ..., <fields>, ... 
;; from (select count(*) as c1 from master_object_000 where <pred>) as q1,
;;      (select count(*) as c2 from master_object_001 where <pred>) as q2,
;;      ...
;;      <Object as Union> ;

;; Cartesian product without WHERE :
;; select count(*) from T1, T2, ... ; 
;; =>
;; select c1 * c2 * ...
;; from (select count(*) as c1 from T1) as q1,
;;      (select count(*) as c2 from T2) as q2,
;;      ...
;;      ;

;; Cartesian product with WHERE => no transformation

;; select count(*) from (T1 union T2 ...) where <pred> ;

;; How to transform ?
;; (??) Group by, Having
;; (??) Select count(*), f1, ...
;; 

(define (table->name table-ast)
  (cond ((table-record? table-ast) (table-record->name table-ast))
        ((alias? table-ast) (alias->name table-ast))
        (else (error "count-rewriter/table->name: unknown type" table-ast))))

(define-ast-walker count-rewriter 
  (cardinality-one? choice) ;; arguments
  (dispatch (sql-record? sql-record-dispatch)
            (modulo-partition? modulo-partition-dispatch))

  (define (modulo-partition-dispatch obj)
    (dispatch (modulo-partition->ast obj)))
  
  (define (count? select)
    (let ((distinct? (projection->distinct? select))
          (fields    (projection->fields select)))
      ;; Looking for count(*) in fields
      (let loop ((fields fields))
        (and (not (null? fields))
             (let ((head (car fields))
                   (others (cdr fields)))
               (and (function-call? head)
                    (let ((function   (function-call->function head))
                          (arguments  (function-call->arguments head)))
                      (or (and (string-ci=? function "count")
                               (= (length arguments) 1)
                               (string=? (field->column (car arguments)) "*"))
                          (loop others)))))))))

  (define (only-count? select)
    (let ((distinct? (projection->distinct? select))
          (fields    (projection->fields select)))
      (and (not (null? fields))
           (null? (cdr fields))
           (let ((head (car fields)))
             (and (function-call? head)
                  (let ((function   (function-call->function head))
                        (arguments  (function-call->arguments head)))
                    (and (string-ci=? function "count")
                         (= (length arguments) 1)
                         (string=? (field->column (car arguments)) "*"))))))))

  (define (all-combinators-are-union-all where)
    (let* ((list-of-table-combinators (table-combinator->list-of-table-combinators where))
           (list-of-combinators (map table-combinator-partial->combinator
                                     list-of-table-combinators)))
      (let loop ((list-of-combinators list-of-combinators))
        (or (null? list-of-combinators)
            (and (string-ci=? "UNION ALL" (car list-of-combinators))
                 (loop (cdr list-of-combinators)))))))      

  (define (constraints? table-ast)
    (cond ((table-record? table-ast) #f)
          ((sql-record? table-ast) (not (eq? (sql-record->where table-ast) 'empty-where)))
          ((alias? table-ast) (constraints? (alias->value table-ast)))
          ((parenthesised-expression? table-ast)
           (constraints? (parenthesised-expression->expr table-ast)))
          ((table-combinator? table-ast)
           (let* ((table (table-combinator->table table-ast))
                  (list-of-table-combinators (table-combinator->list-of-table-combinators table-ast))
                  (list-of-tables (map table-combinator-partial->table
                                       list-of-table-combinators)))
             (or (constraints? table)
                 (let loop ((list-of-tables list-of-tables))
                   (and (not (null? list-of-tables))
                        (or (constraints? (car list-of-tables))
                            (loop (cdr list-of-tables))))))))
          ((distributed-query? table-ast)
           (constraints? (distributed-query->query table-ast)))
          (else (error "count-rewriter/constraints?: unknown type" table-ast))))

  (define (rewrite-from from)
    (debug
     (display "rewrite-from:  ")
     (pp (ast->sql from)) (newline))
    (cond ((table-record? from) from)
          ((join-table-on? from) from) ;; TODO
          ((cartesian-product? from) from)
          ;; TODO: (error "count-rewriter/sql-record-dispatch: cartesian product not implemented yet" from))
          ((combined-subquery? from) from)
          ;; TODO: (error "count-rewriter/sql-record-dispatch: Combined SQL subquery not implemented yet" from))
          ((alias? from) from)
          ;; TODO: (error "count-rewriter/sql-record-dispatch: ALIAS not implemented yet" from))
          
          ((join-table-using? from) (rewrite-join-using from))
          ((table-combinator? from)
           ;; all UNION ALL
           (let ((list-of-table-combinators (table-combinator->list-of-table-combinators from)))
             (cond ((null? list-of-table-combinators) from)
                   ((all-combinators-are-union-all from)
                    (error "count-rewriter/sql-record-dispatch: UNION ALL not implemented yet" from))
                   (else from))))
          
          (else (error "count-rewriter/sql-record-dispatch: unknown type" from))))
  
  (define (rewrite-join-using from)
    
    (define (helper)

      (let ((type         (join-table-using->type from))
            (left-table   (join-table-using->left-table from))
            (right-table  (join-table-using->right-table from))
            (join-fields  (join-table-using->join-fields from)))
        (let ((left-table-have-constraint?  (constraints? left-table))
              (right-table-have-constraint? (constraints? right-table))
              (left-table-name (table->name left-table))
              (right-table-name (table->name right-table))
              (join-fields-list (map field->column join-fields)))
          (let ((decision-left?
                 (and (not right-table-have-constraint?)
                      (cardinality-one? left-table-name
                                        right-table-name
                                        join-fields-list)))
                (decision-right?
                 (and (not left-table-have-constraint?)
                      (cardinality-one? right-table-name
                                        left-table-name
                                        join-fields-list))))
            (debug
             (pp (list "helper:"
                       left-table-have-constraint? right-table-have-constraint?
                       decision-left? decision-right?)) (newline))
            (cond ((and (not decision-left?) (not decision-right?)) from)
                  ((and decision-left? (not decision-right?)) (rewrite-from left-table))
                  ((and (not decision-left?) decision-right?) (rewrite-from right-table))
                  ((and decision-left? decision-right?)
                   (rewrite-from
                    (choice left-table left-table-name
                            right-table right-table-name))))))))

    (helper))
  
  
  ;; Dispatchers

  (define (sql-record-dispatch obj)
    (debug
     (display "sql-record-dispatch:  ")
     (pp (ast->sql obj)) (newline))
    (let ((select    (sql-record->select  obj))
          (from      (sql-record->from  obj))
          (where     (sql-record->where  obj))
          (group-by  (sql-record->group-by  obj))
          (order-by  (sql-record->order-by  obj))
          (limit     (sql-record->limit  obj)))
      (if (and (eq? group-by 'empty-group)
               (eq? where 'empty-where)
               (only-count? select))
          (cond ((sql-subquery? from)
                 (let* ((content (alias->value from))
                        (content (parenthesised-expression->expr content))
                        (from      (sql-record->from  content))
                        (where     (sql-record->where  content))
                        (group-by  (sql-record->group-by  content))
                        (order-by  (sql-record->order-by  content))
                        (limit     (sql-record->limit  content)))
                   ;; In another context  we should have rewritten the
                   ;; upper select to replace the alias name but as it
                   ;; is a "count(*)" this is not needed.
                   (dispatch (sql-record select from where group-by order-by limit))))
                (else
                 (let ((new-from (rewrite-from from)))
                   (if (equal? from new-from) obj
                       (sql-record-dispatch 
                        (sql-record select new-from where
                                    group-by order-by limit))))))
          obj)))

  ;; End of count-rewriter
  )
