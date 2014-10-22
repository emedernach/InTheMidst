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

;;      (export
;;       collect-fields
;;       collect-fields-insert
;;       collect-fields-subset?
;;       )

;; Scan the tree and collect only fields type

;; TODO: Beware that a field could shadow another one in subqueries !

;; TODO: table->fields

;; This is not a AST walker as we collect fields as we walk the tree !
(define (collect-fields-helper ast table->fields)

  (define (dispatch obj)
    (let ((result (dispatch_ obj)))
      (if (boolean? result)
          (error "collect-fields: boolean result"
                 (list obj result))
          result)))
  
  (define (dispatch_ obj)
    (cond
     
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
     ((inner-joins? obj) (inner-join-dispatch obj))
     
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
     
     ;; Tokens
     ((string-token? obj) '())
     ((number? obj) '())
     ((symbol? obj) '())
     ((set? obj) '())
     
     (else (error "collect-fields: Unknown type"
                  (pp (list ast obj))))))

  ;; Dispatchers

  (define (parenthesised-expression-dispatch obj)
    (let ((expr  (parenthesised-expression->expr obj)))
      (dispatch expr)))

  (define (arithmetic-dispatch obj)
    (let ((first-expr  (arithmetic->first-expr obj))
          (rest-expr   (arithmetic->rest-expr obj)))
      (cons (dispatch first-expr)
            (map dispatch rest-expr))))
  
  (define (partial-arithmetic-dispatch obj)
    (let ((operation  (partial-arithmetic->operation obj))
          (expr       (partial-arithmetic->expr obj)))
      (dispatch expr)))

  (define (unary-arithmetic-dispatch obj)
    (let ((unary-op   (unary-arithmetic->unary-op obj))
          (expr       (unary-arithmetic->expr obj)))
      (dispatch expr)))

  (define (cast-expression-dispatch obj)
    (let ((expr  (cast-expression->expr obj))
          (type  (cast-expression->type obj)))
      (dispatch expr)))    
  
  (define (function-call-dispatch obj)
    (let ((function   (function-call->function obj))
          (arguments  (function-call->arguments obj)))
      (map dispatch arguments)))

  (define (case-expr-dispatch obj)
    (let ((case-clauses-list  (case-expr->case-clauses-list obj))
          (else-clause        (case-expr->else-clause obj)))
      (list (map dispatch case-clauses-list)
            (dispatch else-clause))))

  (define (case-value-dispatch obj)
    (let ((expr               (case-value->expr obj))
          (case-clauses-list  (case-value->case-clauses-list obj))
          (else-clause        (case-value->else-clause obj)))
      (list (dispatch expr)
            (map dispatch case-clauses-list)
            (dispatch else-clause))))

  (define (case-clauses-dispatch obj)
    (let ((predicate  (case-clause->predicate obj))
          (expr       (case-clause->expr obj)))
      (list (dispatch predicate) (dispatch expr))))
  
  (define (else-clause-dispatch obj)
    (let ((expr  (else-clause->expr obj)))
      (dispatch expr)))

  (define (order-by-dispatch obj)
    (if (eq? obj 'empty-order-by) '()
        (let ((direction (order-by->direction obj))
              (fields    (order-by->fields obj)))
          (map dispatch fields))))

  (define (sql-limit-dispatch obj) 
    (let ((number (sql-limit->number obj)))
      '()))

  (define (group-by-dispatch obj)
    (let ((fields (group-by->fields obj))
          (having (group-by->having obj)))
      (list (map dispatch fields)
            (map dispatch having))))

  (define (predicate-dispatch obj)
    (let ((first-expr (predicate->first-expr obj))
          (rest-expr  (predicate->rest-expr obj)))
      (cons (dispatch first-expr)
            (map dispatch rest-expr))))

  (define (partial-predicate-dispatch obj)
    (let ((logic-operation (partial-predicate->logic-operation obj))
          (predicate       (partial-predicate->predicate obj)))
      (dispatch predicate)))

  (define (between-predicate-dispatch obj)
    (let ((expr (between-predicate->expr obj))
          (left-bound  (between-predicate->left-bound obj))
          (right-bound (between-predicate->right-bound obj)))
      (list (dispatch expr)
            (dispatch left-bound)
            (dispatch right-bound))))

  (define (negation-predicate-dispatch obj)
    (let ((predicate  (negation-predicate->predicate obj)))
      (dispatch predicate)))

  (define (set-predicate-dispatch obj)
    (let ((expr  (set-predicate->expr obj))
          (set-of-values  (set-predicate->set-of-values obj)))
      (dispatch expr)))

  (define (comparison-dispatch obj)
    (let ((comparator (comparison->comparator obj))
          (left-expr  (comparison->left-expr obj))
          (right-expr (comparison->right-expr obj)))
      (list (dispatch left-expr)
            (dispatch right-expr)))) 

  (define (like-operator-dispatch obj)
    (let ((expr     (like-operator->expr obj))
          (not?     (like-operator->not? obj))
          (pattern  (like-operator->pattern obj)))
      (dispatch expr)))

  (define (is-null-dispatch obj)
    (let ((field  (is-null-record->field obj))
          (not?   (is-null-record->not? obj)))
      (dispatch field)))

  (define (any-record-dispatch obj)
    (let ((subquery (any-record->subquery obj)))
      (dispatch subquery)))
  
  (define (all-record-dispatch obj)
    (let ((subquery (all-record->subquery obj)))
      (dispatch subquery)))
  
  (define (some-record-dispatch obj)
    (let ((subquery (some-record->subquery obj)))
      (dispatch subquery)))
  
  (define (exists-record-dispatch obj)
    (let ((subquery (exists-record->subquery obj)))
      (dispatch subquery)))

  (define (projection-dispatch obj)
    (let ((distinct? (projection->distinct? obj))
          (fields    (projection->fields obj)))
      (map dispatch fields)))

  (define (sql-record-dispatch obj)
    (let ((select    (sql-record->select  obj))
          (from      (sql-record->from  obj))
          (where     (sql-record->where  obj))
          (group-by  (sql-record->group-by  obj))
          (order-by  (sql-record->order-by  obj))
          (limit     (sql-record->limit  obj)))
      (list (dispatch select)
            (dispatch from)
            (dispatch where)
            (dispatch group-by)
            (dispatch order-by)
            (dispatch limit))))

  (define (field-dispatch obj)
    (let ((table  (field->table obj))
          (column (field->column obj)))
      ;;  (field table column)))
      ;; TODO: do we have to track which database a field come from or not ?
      ;; column))
      (list obj)))

  (define (alias-dispatch obj)
    ;; We have to rename the table with the corresponding alias name
    (let ((name (alias->name obj))
          (val  (alias->value obj)))
      (let ((tmp (flatten (dispatch val))))
        ;; (display (list "DEBUG: alias-dispatch" tmp)) (newline)
        (map (lambda (f)
               (if (field? f)
                   (let ((table (field->table f))
                         (column (field->column f))) 
                     (field name column))
                   (error "collect_fields/alias-dispatch: not a field" f
                          ;; (begin (display (ast->sql ast)) f)
                          )))
             tmp))))        

  (define (table-dispatch obj)
    (let ((database (table-record->database obj))
          (name (table-record->name obj)))
      (let ((result (table->fields database name)))
        (if (eq? result #f)
            (error "collect_fields/table-dispatch: table not found in description"
                   (list database name result))
            result))))

  (define (inner-join-dispatch obj)
    (list (map dispatch (inner-joins->table-list obj))
          (apply map dispatch (inner-joins->join-fields-list obj))))
  
  (define (join-table-using-dispatch obj)
    (let ((type         (join-table-using->type obj))
          (left-table   (join-table-using->left-table obj))
          (right-table  (join-table-using->right-table obj))
          (join-fields  (join-table-using->join-fields obj)))
      (list (dispatch left-table)
            (dispatch right-table)
            (map dispatch join-fields))))

  (define (join-table-on-dispatch obj)
    (let ((type        (join-table-on->type obj))
          (left-table  (join-table-on->left-table obj))
          (right-table (join-table-on->right-table obj))
          (predicate   (join-table-on->predicate obj)))
      (list (dispatch left-table)
            (dispatch right-table)
            (dispatch predicate))))

  (define (join-table-using-partial-dispatch obj)
    (let ((type  (join-table-using-partial->type obj))
          (table (join-table-using-partial->table obj))
          (join-fields (join-table-using-partial->join-fields obj)))
      (list (dispatch table)
            (map dispatch join-fields))))

  (define (join-table-on-partial-dispatch obj)
    (let ((type      (join-table-on-partial->type obj))
          (table     (join-table-on-partial->table obj))
          (predicate (join-table-on-partial->predicate obj)))
      (list (dispatch table)
            (dispatch predicate))))

  (define (table-combinator-dispatch obj)
    (let ((table (table-combinator->table obj))
          (list-of-table-combinators (table-combinator->list-of-table-combinators obj)))
      (list (dispatch table)
            (map dispatch list-of-table-combinators))))

  (define (table-combinator-partial-dispatch obj)
    (let ((combinator (table-combinator-partial->combinator obj))
          (table      (table-combinator-partial->table obj)))
      (dispatch table)))
  
  (define (cartesian-product-dispatch obj)
    (let ((table-list (cartesian-product->tables obj)))
      (map dispatch table-list)))

  (define (modulo-partition-dispatch obj)
    (let ((name     (modulo-partition->name obj))
          (key      (modulo-partition->key obj))
          (number   (modulo-partition->number obj))
          (template (modulo-partition->template obj)))
      ;; key is not listed here as it is not used directly
      '()))

  (define (distributed-query-dispatch obj)
    (let ((query  (distributed-query->query obj))
          (key    (distributed-query->key obj))
          (number (distributed-query->number obj)))
      (list (dispatch query)
            (dispatch key))))
  
  ;; Main body

  (dispatch ast))

(define (collect-fields-insert obj cf)
  ;; cf is a list of fields
  (if (field? obj)
      (cons obj cf)
      (error "collect-fields-insert: not a field" obj)))

(define (collect-fields-subset? cf1 cf2)
  ;; cf1 and cf2 are lists of fields
  
  ;; We must take into account non-qualified fields (with unknown table)
  (define (field-compare? f1 f2)
    (define (compare x y)
      (or (and (symbol? x) (symbol? y)
               (eq? x y))
          (and (string? x) (string? y)
               (string-ci=? x y))))
    (let ((f1-table (field->table f1))
          (f2-table (field->table f2)))
      (and (or (eq? f1-table 'unknown)
               (compare f1-table f2-table))
           (let ((f1-column (field->column f1))
                 (f2-column (field->column f2)))
             (compare f1-column f2-column)))))
  
  (or (null? cf1)
      (let ((head   (car cf1))
            (others (cdr cf1)))
        (if (field? head)
            (let ((table  (field->table head))
                  (column (field->column head)))
              (and (list-member? head cf2 field-compare?)
                   (collect-fields-subset? others cf2)))
            (error "collect-fields-subset?: not a field" head)))))

;; Returns a list of fields from an AST
(define (collect-fields ast table->fields)
  (let* ((tmp (collect-fields-helper ast table->fields))
         (result (flatten tmp)))
    ;; (result (map field->column result)))
    ;; (display (list 'collect-fields ast result)) (newline)
    ;; (cond ((pair? result) (list->string-set result))
    ;;       ((null? result) (empty-string-set))
    ;;       (else (error "collect-fields: Unknown type:" result)))))
    result ))


