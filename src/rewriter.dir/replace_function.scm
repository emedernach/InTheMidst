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



;;      (export replace-functions)

(define (replace-functions ast functions-hash)

  (define (dispatch obj)
    (cond
     ((sql-record? obj) (sql-record-dispatch obj))
     ((projection? obj) (projection-dispatch obj))
     ((field? obj) obj)
     ((table-record? obj) obj)
     ((cast-expression? obj) (cast-dispatch obj))
     ((comparison? obj) (comparison-dispatch obj))
     ((cartesian-product? obj) (cartesian-product-dispatch obj))
     ((join-table-using? obj) (join-table-using-dispatch obj))
     ((join-table-on? obj) (join-table-on-dispatch obj))
     ((predicate? obj) (predicate-dispatch obj))
     ((partial-predicate? obj) (partial-predicate-dispatch obj))
     ((alias? obj)  (alias-dispatch obj))
     ((function-call? obj) (function-call-dispatch obj))
     ((between-predicate? obj) (between-predicate-dispatch obj))
     ((order-by? obj) (order-by-dispatch obj))
     ((group-by? obj) (group-by-dispatch obj))
     ((parenthesised-expression? obj)  (parenthesised-expression-dispatch obj))
     ((table-combinator? obj) (table-combinator-dispatch obj))
     ((table-combinator-partial? obj) (table-combinator-partial-dispatch obj)) 
     ((arithmetic? obj) (arithmetic-dispatch obj))
     ((partial-arithmetic? obj) (partial-arithmetic-dispatch obj))          
     ((unary-arithmetic? obj) (unary-arithmetic-dispatch obj))
     ((case-expr? obj) (case-expr-dispatch obj))
     ((case-value? obj) (case-value-dispatch obj))
     ((case-clause? obj) (case-clause-dispatch obj))
     ((else-clause? obj) (else-clause-dispatch obj))
     ((negation-predicate? obj) (negation-predicate-dispatch obj))
     ((set-predicate? obj) (set-predicate-dispatch obj))
     ((sql-limit? obj) (sql-limit-dispatch obj))
     
     ;; Constants
     ((number? obj) obj)
     ((string-token? obj) obj)
     ((symbol? obj) obj)
     ((set? obj) obj)
     ((boolean? obj) obj)
     
     (else (error "replace-functions dispatch: Unknown type" obj))))

  ;; Dispatchers

  (define (sql-record-dispatch obj)
    (let ((select    (sql-record->select  obj))
          (from      (sql-record->from  obj))
          (where     (sql-record->where  obj))
          (group-by  (sql-record->group-by  obj))
          (order-by  (sql-record->order-by  obj))
          (limit     (sql-record->limit  obj)))
      (sql-record (dispatch select)
                  (dispatch from)
                  (dispatch where)
                  (dispatch group-by)
                  (dispatch order-by)
                  (dispatch limit))))

  (define (projection-dispatch obj)
    (let ((distinct? (projection->distinct? obj))
          (fields    (projection->fields obj)))
      (projection distinct? (map dispatch fields))))

  (define (comparison-dispatch obj)
    (let ((comparator (comparison->comparator obj))
          (left-expr  (comparison->left-expr obj))
          (right-expr (comparison->right-expr obj)))
      (comparison comparator
                  (dispatch left-expr)
                  (dispatch right-expr))))

  (define (cartesian-product-dispatch obj)
    (let ((table-list (cartesian-product->tables obj)))
      (cartesian-product (map dispatch table-list))))

  (define (join-table-using-dispatch obj)
    (let ((type         (join-table-using->type obj))
          (left-table   (join-table-using->left-table obj))
          (right-table  (join-table-using->right-table obj))
          (join-fields  (join-table-using->join-fields obj)))
      (join-table-using type
                        (dispatch left-table)
                        (dispatch right-table)
                        join-fields)))

  (define (join-table-on-dispatch obj)
    (let ((type        (join-table-on->type obj))
          (left-table  (join-table-on->left-table obj))
          (right-table (join-table-on->right-table obj))
          (predicate   (join-table-on->predicate obj)))
      (join-table-on
       type
       (dispatch left-table)
       (dispatch right-table)
       (dispatch predicate))))

  (define (predicate-dispatch obj)
    (let ((first-expr (predicate->first-expr obj))
          (rest-expr  (predicate->rest-expr obj)))
      (predicate (dispatch first-expr)
                 (map dispatch rest-expr))))

  (define (partial-predicate-dispatch obj)
    (let ((logic-operation (partial-predicate->logic-operation obj))
          (predicate       (partial-predicate->predicate obj)))
      (partial-predicate logic-operation
                         (dispatch predicate))))

  (define (cast-dispatch obj)
    (let ((expr (cast-expression->expr obj))
          (type (cast-expression->type obj)))
      (cast-expression (dispatch expr) type)))
  
  (define (alias-dispatch obj)
    (let ((name (alias->name obj))
          (val  (alias->value obj)))
      (make-alias name (dispatch val))))

  (define (function-call-dispatch obj)
    (let ((function   (function-call->function obj))
          (arguments  (function-call->arguments obj)))
      (let ((arguments (map dispatch arguments))
            (fun (table-ref functions-hash function 'empty)))
        (if (eq? fun 'empty)
            (function-call function arguments)
            (apply fun arguments)))))

  (define (between-predicate-dispatch obj)
    (let ((expr (between-predicate->expr obj))
          (left-bound  (between-predicate->left-bound obj))
          (right-bound (between-predicate->right-bound obj)))
      (between-predicate (dispatch expr)
                         (dispatch left-bound)
                         (dispatch right-bound))))

  (define (order-by-dispatch obj)
    (let ((direction (order-by->direction obj))
          (fields    (order-by->fields obj)))
      (order-by direction (map dispatch fields))))

  (define (group-by-dispatch obj)
    (let ((fields  (group-by->fields obj))
          (having  (group-by->having obj)))
      (group-by (map dispatch fields)
                (dispatch having))))

  (define (parenthesised-expression-dispatch obj)
    (let ((expr  (parenthesised-expression->expr obj)))
      (parenthesised-expression (dispatch expr))))

  (define (table-combinator-dispatch obj)
    (let ((table (table-combinator->table obj))
          (list-of-table-combinators (table-combinator->list-of-table-combinators obj)))
      (table-combinator
       (dispatch table)
       (map dispatch list-of-table-combinators))))

  (define (table-combinator-partial-dispatch obj)
    (let ((combinator (table-combinator-partial->combinator obj))
          (table      (table-combinator-partial->table obj)))
      (table-combinator-partial combinator (dispatch table))))

  (define (arithmetic-dispatch obj)
    (let ((first-expr  (arithmetic->first-expr obj))
          (rest-expr   (arithmetic->rest-expr obj)))
      (arithmetic
       (dispatch first-expr)
       (map dispatch rest-expr))))

  (define (partial-arithmetic-dispatch obj)
    (let ((operation  (partial-arithmetic->operation obj))
          (expr       (partial-arithmetic->expr obj)))
      (partial-arithmetic operation (dispatch expr))))

  (define (unary-arithmetic-dispatch obj)
    (let ((unary-op   (unary-arithmetic->unary-op obj))
          (expr       (unary-arithmetic->expr obj)))
      (unary-arithmetic unary-op (dispatch expr))))

  (define (case-expr-dispatch obj)
    (let ((case-clauses-list  (case-expr->case-clauses-list obj))
          (else-clause        (case-expr->else-clause obj)))
      (case-expr (map dispatch case-clauses-list)
                 (dispatch else-clause))))

  (define (case-value-dispatch obj)
    (let ((expr               (case-value->expr obj))
          (case-clauses-list  (case-value->case-clauses-list obj))
          (else-clause        (case-value->else-clause obj)))
      (case-value (dispatch expr)
                  (map dispatch case-clauses-list)
                  (dispatch else-clause))))

  (define (case-clause-dispatch obj)
    (let ((predicate  (case-clause->predicate obj))
          (expr       (case-clause->expr obj)))
      (case-clause (dispatch predicate) (dispatch expr))))

  (define (else-clause-dispatch obj)
    (let ((expr  (else-clause->expr obj)))
      (else-clause (dispatch expr))))

  (define (negation-predicate-dispatch obj)
    (let ((predicate  (negation-predicate->predicate obj)))
      (negation-predicate (dispatch predicate))))                     

  (define (set-predicate-dispatch obj)
    (let ((expr  (set-predicate->expr obj))
          (set-of-values  (set-predicate->set-of-values obj)))
      (set-predicate (dispatch expr) set-of-values)))

  (define (sql-limit-dispatch obj)
    (let ((number (sql-limit->number obj)))
      (sql-limit number)))
  
  ;; replace_functions
  
  (dispatch ast))

;; Tests

'(let ((functions-hash (make-table)))
   (table-set! functions-hash "spatial_rectangle" spatial_rectangle)
   (AST->SQL (replace-functions
              (SQL->AST LSST_query_003)
              functions-hash)))                  

