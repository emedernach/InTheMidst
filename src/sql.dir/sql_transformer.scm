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



;;      (export
;;       all-expr-transformer
;;       any-expr-transformer
;;       between-predicate-transformer
;;       case-else-expr-transformer
;;       case-expr-transformer
;;       case-value-transformer
;;       case-when-expr-transformer
;;       cast-expr-transformer
;;       comparison-transformer
;;       crossmatch-transformer
;;       database-table-name-transformer
;;       exists-transformer
;;       field-expr-transformer
;;       field-name-transformer
;;       field-test-equality-transformer
;;       from-transformer
;;       group-by-transformer
;;       is-null-transformer
;;       join-table-on-transformer
;;       join-table-using-transformer
;;       like-transformer
;;       limit-transformer
;;       make-parenthesised-expression
;;       order-by-transformer
;;       predicate-transformer
;;       predicate-with-optional-negation-transformer
;;       select-field-transformer
;;       select-transformer
;;       set-predicate-transformer
;;       some-expr-transformer
;;       sql-selection-transformer
;;       sql-transformer
;;       subquery-transformer
;;       table-alias-transformer
;;       table-combinator-transformer
;;       table-name-transformer
;;       table-with-optional-join-transformer
;;       where-transformer
;;       )

(define (table-combinator-transformer combinator table)
  (table-combinator-partial combinator table))

(define (sql-transformer table list-of-table-combinators)
  (if (null? list-of-table-combinators) table
      (table-combinator table list-of-table-combinators)))

(define (sql-selection-transformer select from xmatch where group-by order-by limit)
  (let ((query (sql-record select from where group-by order-by limit)))
    (if (eq? xmatch 'empty-crossmatch) query
        (crossmatch-list xmatch query))))

(define (select-transformer distinct fields)
  (projection distinct fields))  

(define (select-field-transformer field-expr alias-name)
  (if (eq? alias-name 'no-alias)
      field-expr
      (make-alias alias-name field-expr)))

(define (from-transformer list-of-tables)
  (apply table:cartesian-product list-of-tables))

(define (field-name-transformer field-name)
  (field 'Unknown field-name))

(define (table-name-transformer table-name)
  (table-record 'Unknown table-name))

(define (database-table-name-transformer db table-name)
  (table-record db table-name))

(define (table-alias-transformer raw-table table-name)
  (make-alias table-name raw-table))

(define (join-table-using-transformer type table-name list-of-fields)
  (join-table-using-partial type table-name list-of-fields))

(define (join-table-on-transformer type table-name field-comparison)
  (join-table-on-partial type table-name field-comparison))

(define (field-test-equality-transformer left-expr right-expr)
  (comparison "=" left-expr right-expr))

(define (subquery-transformer sql table-name)
  (make-alias table-name sql))

(define (table-with-optional-join-transformer
         partial-join result-table)
  (cond ((join-table-using-partial? partial-join)
         (let ((type  (join-table-using-partial->type partial-join))
               (table (join-table-using-partial->table partial-join))
               (join-fields (join-table-using-partial->join-fields partial-join)))
           (join-table-using type result-table table join-fields)))
        ((join-table-on-partial? partial-join)
         (let ((type  (join-table-on-partial->type partial-join))
               (table (join-table-on-partial->table partial-join))
               (predicate (join-table-on-partial->predicate partial-join)))
           (join-table-on type result-table table predicate)))
        (else (error "Unknown join table type:" partial-join))))

(define (make-parenthesised-expression expr)
  (cond ((field? expr) expr)
        (else (parenthesised-expression expr))))

(define (case-expr-transformer clauses-list else-clause)
  (case-expr clauses-list else-clause))

(define (case-value-transformer expr clauses-list else-clause)
  (case-value expr clauses-list else-clause))

(define (case-when-expr-transformer pred expr)
  (case-clause pred expr))

(define (case-else-expr-transformer expr)
  (else-clause expr))

(define (comparison-transformer left-expr comparator right-expr)
  (comparison comparator left-expr right-expr))

(define (between-predicate-transformer expr not-string left-bound right-bound)
  (if (string-ci=? not-string "NOT")
      (negation-predicate (between-predicate expr left-bound right-bound))
      (between-predicate expr left-bound right-bound)))

(define (set-predicate-transformer expr not-string set-of-values)
  (if (string-ci=? not-string "NOT")
      (negation-predicate (set-predicate expr set-of-values))
      (set-predicate expr set-of-values)))

(define (predicate-with-optional-negation-transformer not-string pred)
  (if (string-ci=? not-string "NOT")
      (negation-predicate pred)
      pred))

(define (cast-expr-transformer expr type)
  (cast-expression expr type))

(define (field-expr-transformer expr partial-expr-list)
  (if (null? partial-expr-list) expr
      (arithmetic expr partial-expr-list)))

(define (predicate-transformer pred partial-pred-list)
  (if (null? partial-pred-list)
      pred
      (predicate pred partial-pred-list)))

(define (order-by-transformer field-list direction)
  (order-by direction field-list))

(define (crossmatch-transformer
         table1 ra1 decl1
         table2 ra2 decl2
         radius)
  (crossmatch
   table1 ra1 decl1
   table2 ra2 decl2
   radius))

(define (where-transformer pred) pred)

(define (limit-transformer number)
  (sql-limit number))

(define (group-by-transformer field-list having)
  (group-by field-list having))

(define (like-transformer field-expr not? str)
  (like-operator field-expr (eq? not? 'not) str))

(define (is-null-transformer field-expr not?)
  (is-null-record field-expr (eq? not? 'not)))

(define (any-expr-transformer subquery)
  (any-record subquery))

(define (all-expr-transformer subquery)
  (all-record subquery))

(define (some-expr-transformer subquery)
  (some-record subquery))

(define (exists-transformer subquery)
  (exists-record subquery))

