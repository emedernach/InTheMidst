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


;; define-ast-walker is a macro for AST walker creation.  An
;; AST  walker  is  a  recursive  function  on  an  AST  and
;; returning an AST.

(define-syntax define-ast-walker
  (syntax-rules ()

    ;; TODO: Is it general enough to handle ast->SQL ?
    ;; the same for collect_tables
    ;; i.e. when we dont return an AST this macro is quite useless
    ;; How to do better then ?

    ((define-ast-walker <name>
       (<args> ...) ;; Additional arguments
       (dispatch (<predicate> <action>) ...)
       <body> ...)
         
    ;; <name> <args> ...
    ;; dispatch is a macro parameter in order to be called outside

    (define (<name> ast <args> ...)

      <body> ...
      
      (define (dispatch obj)
        (cond

         ((<predicate> obj) (<action> obj)) ...
        
         ;; Default dispatchers

         ;; Arithmetic
         ((parenthesised-expression? obj) (parenthesised-expression-dispatch obj))
         ((arithmetic? obj) (arithmetic-dispatch obj))
         ((partial-arithmetic? obj) (partial-arithmetic-dispatch obj))
         ((unary-arithmetic? obj) (unary-arithmetic-dispatch obj))
         ((cast-expression? obj) (cast-expression-dispatch obj))
         
         ;; Function
         ((function-call? obj) (function-call-dispatch obj))

         ;; Control flow
         ((case-expr? obj) (case-expr-dispatch obj))
         ((case-value? obj) (case-value-dispatch obj))
         ((case-clause? obj) (case-clauses-dispatch obj))
         ((else-clause? obj) (else-clause-dispatch obj))

         ;; Modifiers
         ((order-by? obj) (order-by-dispatch obj))
         ((sql-limit? obj) (sql-limit-dispatch obj))
         ((group-by? obj) (group-by-dispatch obj))

         ;; Predicates
         ((predicate? obj) (predicate-dispatch obj))
         ((partial-predicate? obj) (partial-predicate-dispatch obj))
         ((between-predicate? obj) (between-predicate-dispatch obj))
         ((negation-predicate? obj) (negation-predicate-dispatch obj))
         ((set-predicate? obj) (set-predicate-dispatch obj))
         ((comparison? obj) (comparison-dispatch obj))
         ((like-operator? obj) (like-operator-dispatch obj))
         ((is-null-record? obj) (is-null-dispatch obj))
         ((any-record? obj) (any-record-dispatch obj))
         ((all-record? obj) (all-record-dispatch obj))
         ((some-record? obj) (some-record-dispatch obj))
         ((exists-record? obj) (exists-record-dispatch obj))

         ;; Projection
         ((projection? obj) (projection-dispatch obj))

         ;; SQL
         ((sql-record? obj) (sql-record-dispatch obj))
            
         ;; Tables
         ((field? obj)  (field-dispatch obj))
         ((alias? obj)  (alias-dispatch obj))
         ((table-record? obj)  (table-dispatch obj))
         ((join-table-using? obj) (join-table-using-dispatch obj))
         ((join-table-on? obj) (join-table-on-dispatch obj))
         ((join-table-using-partial? obj) (join-table-using-partial-dispatch obj))
         ((join-table-on-partial? obj) (join-table-on-partial-dispatch obj))
         ((table-combinator? obj) (table-combinator-dispatch obj))
         ((table-combinator-partial? obj) (table-combinator-partial-dispatch obj))
         ((cartesian-product? obj) (cartesian-product-dispatch obj)) 

         ;; Virtual tables
         ((modulo-partition? obj) (modulo-partition-dispatch obj))
         ((distributed-query? obj) (distributed-query-dispatch obj))
         ((inner-joins? obj) (inner-joins-dispatch obj))
         
         ;; Tokens
         ((string-token? obj) obj)
         ((number? obj) obj)
         ((symbol? obj) obj)
         ((boolean? obj) obj)
         ((set? obj) obj)
         
         (else (error "ast-walk/dispatcher: Unknown type"
                      (pp (list ast obj))))))

      (define (parenthesised-expression-dispatch obj)
        (let ((expr  (parenthesised-expression->expr obj)))
          (parenthesised-expression (dispatch expr))))

      (define (arithmetic-dispatch obj)
        (let ((first-expr  (arithmetic->first-expr obj))
              (rest-expr   (arithmetic->rest-expr obj)))
          (arithmetic (dispatch first-expr)
                      (map dispatch rest-expr))))
    
      (define (partial-arithmetic-dispatch obj)
        (let ((operation  (partial-arithmetic->operation obj))
              (expr       (partial-arithmetic->expr obj)))
          (partial-arithmetic operation (dispatch expr))))

      (define (unary-arithmetic-dispatch obj)
        (let ((unary-op   (unary-arithmetic->unary-op obj))
              (expr       (unary-arithmetic->expr obj)))
          (unary-arithmetic unary-op (dispatch expr))))

      (define (cast-expression-dispatch obj)
        (let ((expr  (cast-expression->expr obj))
              (type  (cast-expression->type obj)))
          (cast-expression (dispatch expr) type)))
      
      (define (function-call-dispatch obj)
        (let ((function   (function-call->function obj))
              (arguments  (function-call->arguments obj)))
          (function-call function (map dispatch arguments))))

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

      (define (case-clauses-dispatch obj)
        (let ((predicate  (case-clause->predicate obj))
              (expr       (case-clause->expr obj)))
          (case-clause predicate (dispatch expr))))
  
      (define (else-clause-dispatch obj)
        (let ((expr  (else-clause->expr obj)))
          (else-clause (dispatch expr))))

      (define (order-by-dispatch obj)
        (if (eq? obj 'empty-order-by) obj
            (let ((direction (order-by->direction obj))
                  (fields    (order-by->fields obj)))
              (order-by direction
                        (map dispatch fields)))))

      (define (sql-limit-dispatch obj)
        (let ((number (sql-limit->number obj)))
          (sql-limit number)))

      (define (group-by-dispatch obj)
        (let ((fields (group-by->fields obj))
              (having (group-by->having obj)))
          (group-by (map dispatch fields)
                    (dispatch having))))

      (define (predicate-dispatch obj)
        (let ((first-expr (predicate->first-expr obj))
              (rest-expr  (predicate->rest-expr obj)))
          (predicate (dispatch first-expr)
                     (map dispatch rest-expr))))

      (define (partial-predicate-dispatch obj)
        (let ((logic-operation (partial-predicate->logic-operation obj))
              (predicate       (partial-predicate->predicate obj)))
          (partial-predicate logic-operation (dispatch predicate))))

      (define (between-predicate-dispatch obj)
        (let ((expr (between-predicate->expr obj))
              (left-bound  (between-predicate->left-bound obj))
              (right-bound (between-predicate->right-bound obj)))
          (between-predicate (dispatch expr)
                             (dispatch left-bound)
                             (dispatch right-bound))))

      (define (negation-predicate-dispatch obj)
        (let ((predicate  (negation-predicate->predicate obj)))
          (negation-predicate (dispatch predicate))))

      (define (set-predicate-dispatch obj)
        (let ((expr  (set-predicate->expr obj))
              (set-of-values  (set-predicate->set-of-values obj)))
          (set-predicate (dispatch expr)
                         set-of-values)))

      (define (comparison-dispatch obj)
        (let ((comparator (comparison->comparator obj))
              (left-expr  (comparison->left-expr obj))
              (right-expr (comparison->right-expr obj)))
          (comparison comparator
                      (dispatch left-expr)
                      (dispatch right-expr))))        

      (define (like-operator-dispatch obj)
        (let ((expr     (like-operator->expr obj))
              (not?     (like-operator->not? obj))
              (pattern  (like-operator->pattern obj)))
          (like-operator (dispatch expr) not? pattern)))

      (define (is-null-dispatch obj)
        (let ((field  (is-null-record->field obj))
              (not?   (is-null-record->not? obj)))
          (is-null-record (dispatch field) not?)))

      (define (any-record-dispatch obj)
        (let ((subquery (any-record->subquery obj)))
          (any-record (dispatch subquery))))
  
      (define (all-record-dispatch obj)
        (let ((subquery (all-record->subquery obj)))
          (all-record (dispatch subquery))))
  
      (define (some-record-dispatch obj)
        (let ((subquery (some-record->subquery obj)))
          (some-record (dispatch subquery))))
  
      (define (exists-record-dispatch obj)
        (let ((subquery (exists-record->subquery obj)))
          (exists-record (dispatch subquery))))

      (define (projection-dispatch obj)
        (let ((distinct? (projection->distinct? obj))
              (fields    (projection->fields obj)))
          (projection distinct? (map dispatch fields))))

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

      (define (field-dispatch obj)
        (let ((table  (field->table obj))
              (column (field->column obj)))
          (field table column)))

      (define (alias-dispatch obj)
        (let ((name (alias->name obj))
              (val  (alias->value obj)))
          (make-alias name (dispatch val))))

      (define (table-dispatch obj)
        (let ((database (table-record->database obj))
              (name (table-record->name obj)))
          (table-record database name)))

      (define (join-table-using-dispatch obj)
        (let ((type         (join-table-using->type obj))
              (left-table   (join-table-using->left-table obj))
              (right-table  (join-table-using->right-table obj))
              (join-fields  (join-table-using->join-fields obj)))
          (join-table-using type
                            (dispatch left-table)
                            (dispatch right-table)
                            (map dispatch join-fields))))

      (define (join-table-on-dispatch obj)
        (let ((type        (join-table-on->type obj))
              (left-table  (join-table-on->left-table obj))
              (right-table (join-table-on->right-table obj))
              (predicate   (join-table-on->predicate obj)))
          (join-table-on type
                         (dispatch left-table)
                         (dispatch right-table)
                         (dispatch predicate))))

      (define (join-table-using-partial-dispatch obj)
        (let ((type  (join-table-using-partial->type obj))
              (table (join-table-using-partial->table obj))
              (join-fields (join-table-using-partial->join-fields obj)))
          (join-table-using-partial type
                                    (dispatch table)
                                    (map dispatch join-fields))))

      (define (join-table-on-partial-dispatch obj)
        (let ((type      (join-table-on-partial->type obj))
              (table     (join-table-on-partial->table obj))
              (predicate (join-table-on-partial->predicate obj)))
          (join-table-on-partial type
                                 (dispatch table)
                                 (dispatch predicate))))

      (define (table-combinator-dispatch obj)
        (let ((table (table-combinator->table obj))
              (list-of-table-combinators (table-combinator->list-of-table-combinators obj)))
          (table-combinator
           (dispatch table)
           (map dispatch
                list-of-table-combinators))))

      (define (table-combinator-partial-dispatch obj)
        (let ((combinator (table-combinator-partial->combinator obj))
              (table      (table-combinator-partial->table obj)))
          (table-combinator-partial combinator (dispatch table))))
    
      (define (cartesian-product-dispatch obj)
        (let ((table-list (cartesian-product->tables obj)))
          (cartesian-product (map dispatch table-list))))

      (define (modulo-partition-dispatch obj)
        (let ((name     (modulo-partition->name obj))
              (key      (modulo-partition->key obj))
              (number   (modulo-partition->number obj))
              (template (modulo-partition->template obj)))
          obj))

      (define (distributed-query-dispatch obj)
        (let ((query  (distributed-query->query obj))
              (key    (distributed-query->key obj))
              (number (distributed-query->number obj)))
          (distributed-query (dispatch query) key number)))

      (define (inner-joins-dispatch obj)
        (let ((table-list        (inner-joins->table-list obj))
              (join-fields-list  (inner-joins->join-fields-list obj)))
          (inner-joins (map dispatch table-list)
                       join-fields-list)))
      
      ;; Main body

      (dispatch ast)))))
    