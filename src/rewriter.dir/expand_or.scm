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



;; Expand Query with OR to query with UNION
;; Used in order to only have AND at first-level in predicate.

(define (expand-or ast)

  (define (make-union table)
    (table-combinator-partial "UNION" table))
  
  (define (split-or obj)
    (cond
     ((predicate? obj)
      (let ((first-expr (predicate->first-expr obj))
            (rest-expr  (predicate->rest-expr obj)))
        (let loop ((rest-expr rest-expr)
                   (first-expr first-expr)
                   (and-list (list ))
                   (result '()))
          (if (null? rest-expr)
              (let ((new-predicate (predicate first-expr (reverse and-list))))
                (reverse (cons new-predicate result)))
              (let ((head (car rest-expr))
                    (rest (cdr rest-expr)))
                (let ((logic-op (partial-predicate->logic-operation head))
                      (pred     (partial-predicate->predicate head)))
                  (cond ((string-ci=? logic-op "AND")
                         (loop rest first-expr (cons head and-list) result))
                        ((string-ci=? logic-op "OR")
                         (let ((new-predicate (predicate first-expr (reverse and-list))))
                           (loop rest pred '() (cons new-predicate result))))
                        (else (error "negation-predicate: unknown logical operator:" logic-op)))))))))
     ((negation-predicate? obj) ;; TODO
      (error "negation-predicate: not implemented yet."))
     (else (list obj))))

  ;; Dispatchers
  
  (define (dispatch obj)
    (cond ((sql-record? obj) (sql-record-dispatch obj))
          ((table-combinator? obj) (table-combinator-dispatch obj))
          ((function-call? obj) (function-call-dispatch obj))
          ((parenthesised-expression? obj) (parenthesised-expression-dispatch obj))
          ((or (number? obj) (field? obj)
               (arithmetic? obj))
           obj)
          (else (error "ast-template dispatch: Unknown type" obj))))

  (define (sql-record-dispatch obj)
    (let ((select    (sql-record->select  obj))
          (from      (sql-record->from  obj))
          (where     (sql-record->where  obj))
          (group-by  (sql-record->group-by  obj))
          (order-by  (sql-record->order-by  obj))
          (limit     (sql-record->limit  obj)))
      (if (eq? where 'empty-where) obj
          (let* ((where-list (split-or where))
                 (make-sql (lambda (where) (sql-record select from where group-by order-by limit))) 
                 (sql-list (map make-sql where-list))
                 (head-sql-list (car sql-list))
                 (rest-sql-list (cdr sql-list)))
            (if (= (length sql-list) 1)
                head-sql-list
                (table-combinator
                 head-sql-list
                 (map make-union rest-sql-list)))))))

  (define (table-combinator-dispatch obj)
    (let ((table (table-combinator->table obj))
          (list-of-table-combinators (table-combinator->list-of-table-combinators obj)))
      (table-combinator
       (dispatch table)
       (map table-combinator-partial-dispatch
            list-of-table-combinators))))

  (define (table-combinator-partial-dispatch obj)
    (let ((combinator (table-combinator-partial->combinator obj))
          (table      (table-combinator-partial->table obj)))
      (table-combinator-partial combinator (dispatch table))))

  (define (function-call-dispatch obj)
    (let ((function-name (function-call->function obj))
          (arguments (function-call->arguments obj)))
      (function-call
       function-name
       (map dispatch arguments))))

  (define (parenthesised-expression-dispatch obj)
    (let ((expr (parenthesised-expression->expr obj)))
      (parenthesised-expression (dispatch expr))))
    
  ;; Main

  (dispatch ast))
             
