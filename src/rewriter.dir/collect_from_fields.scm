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



;; This file is included by collect.scm

(include "../macros.dir/macros.scm")
(include "../ast.dir/ast-macros.scm")


;; Collect FROM fields from a query.

(define (collect-from-fields ast)
  (let* ((used-tables (collect-tables ast))
         (tmp (collect-from-fields-helper ast used-tables)))
    (flatten tmp)))

(define-ast-walker collect-from-fields-helper
  (used-tables)
  (dispatch
   (sql-record?  sql-record-dispatch)
   (projection?  projection-dispatch)
   (field?  field-dispatch)
   (function-call?  function-call-dispatch)
   (alias?  alias-dispatch)
   (table-record?  table-dispatch)
   (cast-expression? cast-dispatch)
   (parenthesised-expression?  parenthesised-expression-dispatch)
   (join-table-on?  join-table-on-dispatch)
   (join-table-using?  join-table-using-dispatch)
   (table-combinator?  table-combinator-dispatch))
   
    (define (sql-record-dispatch obj)
      (let ((select    (sql-record->select  obj))
            (from      (sql-record->from  obj))
            (where     (sql-record->where  obj))
            (group-by  (sql-record->group-by  obj))
            (order-by  (sql-record->order-by  obj))
            (limit     (sql-record->limit  obj)))
        (dispatch select)))

    (define (projection-dispatch obj)
      (let ((distinct? (projection->distinct? obj))
            (fields    (projection->fields obj)))
        (map dispatch fields)))

    (define (field-dispatch obj)
      (let ((table  (field->table obj))
            (column (field->column obj)))
        (if (string=? column "*")
            ;; We should collect all fields from the tables inside the query
            (fields-from-tables used-tables)
            ;; Do we have to keep the table name or not ?
            column)))

    (define (cast-dispatch obj)
      (let ((expr (cast-expression->expr obj))
            (type (cast-expression->type obj)))
        (cast-expression (dispatch expr) type)))
  
    (define (function-call-dispatch obj)
      (let ((function   (function-call->function obj))
            (arguments  (function-call->arguments obj)))
        (map dispatch arguments)))

    (define (alias-dispatch obj)
      (let ((name (alias->name obj))
            (val  (alias->value obj)))
        name))

    (define (table-dispatch obj)
      (fields-from-tables (list obj)))

    (define (parenthesised-expression-dispatch obj)
      (let ((expr  (parenthesised-expression->expr obj)))
        (dispatch expr)))

    (define (join-table-using-dispatch obj)
      (let ((type         (join-table-using->type obj))
            (left-table   (join-table-using->left-table obj))
            (right-table  (join-table-using->right-table obj))
            (join-fields  (join-table-using->join-fields obj)))
        ;; TODO: left-table + right-table - join-fields
        ;; The issue is that we don't know here the table fields
        ;; We could use left-table union right-table if we prefix all fields by their table name.
        (union-table-fields
         (dispatch left-table)
         (dispatch right-table))))

    (define (join-table-on-dispatch obj)
      (let ((type        (join-table-on->type obj))
            (left-table  (join-table-on->left-table obj))
            (right-table (join-table-on->right-table obj))
            (predicate   (join-table-on->predicate obj)))
        (union-table-fields
         (dispatch left-table)
         (dispatch right-table))))

    (define (table-combinator-dispatch obj)
      (let ((table (table-combinator->table obj))
            (list-of-table-combinators (table-combinator->list-of-table-combinators obj)))
        ;; All combined tables must have the same fields
        (dispatch table)))

    )


  (define (collect-from-fields:test)
    (unit-test
     (equal? (collect-from-fields (SQL->AST LSST_query_001_a))
             '("taiMidPoint" "psfFlux" "psfFluxSigma"))

     (equal? (collect-from-fields (SQL->AST LSST_query_002))
             (list (fields-from-tables
                    (list (table-record 'Unknown "Object")
                          (table-record 'Unknown "_ObjectToType")
                          (table-record 'Unknown "ObjectType")))))

     (equal? (collect-from-fields (SQL->AST LSST_query_004))
             '("gFlux_PS" "rFlux_PS" "iFlux_PS" "zFlux_PS" "yFlux_PS"))

     (equal? (collect-from-fields (SQL->AST LSST_query_006))
             '("ra" "decl" "raRange" "declRange"))
           
     ))

  