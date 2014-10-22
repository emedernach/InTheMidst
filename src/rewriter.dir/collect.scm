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
(include "../ast.dir/table.scm")
(include "../ast.dir/sql.scm")
(include "../ast.dir/projection.scm")
(include "../ast.dir/function.scm")

(define-record-type :columns
  (columns name fields)
  columns?
  (name    columns->name)
  (fields  columns->fields))

(define-record-type :fields-from-tables
  (fields-from-tables_ tables)
  fields-from-tables?
  (tables    fields-from-tables->tables))

(define (fields-from-tables tables)
  (if (pair? tables)
      (fields-from-tables_ tables)
      (error "fields-from-tables: not a list" tables)))

(define-record-type :union-table-fields
  (union-table-fields left-table right-table)
  union-table-fields?
  (left-table    union-table-fields->left-table)
  (right-table   union-table-fields->right-table))

(include "collect_fields.scm")
(include "collect_from_fields.scm")
(include "collect_tables.scm")

;; For example:
;; (evaluate-collected-tables
;;  (collect-tables (SQL->AST LSST_query_001_a))
;;  LSST_tables-description)

;; collected-tables = (collect-tables ast)
;; tables-description describes tables and function returning tables
;; Returns a list of columns
(define (evaluate-collected-tables collected-tables tables-description)

  ;; [COLLECT-TABLES] = (list [FROM])
  ;; [FROM] = table-record | (columns string [COLLECT-FIELDS])
  ;; 
  ;; [COLLECT-FIELDS] = (list [FIELDS])
  ;; [FIELDS] = column name without the table name |
  ;;            alias name without its value |
  ;;            function-call |
  ;;            (fields-from-tables [COLLECT-TABLES]) |
  ;;            (fields-from-tables (list table-record)) |
  ;;            (union-table-fields [TABLE] [TABLE])

  (define (evaluate-union-table-fields obj)
    (let ((left-table    (union-table-fields->left-table obj))
          (right-table   (union-table-fields->right-table obj)))
      (let ((left-table-fields  (evaluate-collected-table left-table))
            (right-table-fields (evaluate-collected-table right-table)))
        (set->list
         (set-union (list->set left-table-fields)
                    (list->set right-table-fields))))))
  
  (define (evaluate-fields-from-tables obj)
    (let* ((tables (fields-from-tables->tables obj))
           (all-columns (map evaluate-collected-table tables))
           (all-fields  (map columns->fields all-columns))
           ;; all-fields is a list of list of strings
           (all-sets    (map list->string-set all-fields))
           (union-all-sets  (apply string-set-union all-sets)))
      (string-set->list union-all-sets)))

  ;; Return a string  (field name) or a list of strings  (in case of a
  ;; function call or composite columns)
  (define (evaluate-field obj)
    (cond ((string? obj) obj) 
          ((function-call? obj) (schema:get-columns tables-description (function-call->function obj)))
          ((fields-from-tables? obj) (evaluate-fields-from-tables obj))
          ((union-table-fields? obj) (evaluate-union-table-fields obj))
          (else (error "evaluate-field: Unknown type" obj))))

  ;; Returns a columns
  (define (evaluate-columns col)
    (let ((name    (columns->name col))
          (fields  (columns->fields col)))
      (cond ((pair? fields) (columns name (flatten (map evaluate-field fields))))
            ((fields-from-tables? fields) (columns name (evaluate-fields-from-tables fields)))
            (else (error "evaluate-columns: Unknown type" fields)))))

  ;; Returns a columns
  (define (evaluate-collected-table collected-table)
    (cond ((table-record? collected-table)
           (schema:get-columns tables-description (table-record->name collected-table)))
          ((columns? collected-table) (evaluate-columns collected-table))
          (else (error "evaluate-collected-table: Unknown type" collected-table))))

  ;; (pp (list 'DEBUG collected-tables)) (newline)
  (if (pair? collected-tables)
      (map evaluate-collected-table collected-tables)
      (error "evaluate-collected-tables: not a list" collected-tables)))


(define (collect:test)
  (unit-test
   (collect-from-fields:test)
   (collect-tables:test)
   ;; (equal? (evaluate-collected-tables
   ;;          (collect-tables (SQL->AST case01_0006_transientVarObjNearGalaxy))
   ;;          LSST_tables-description)
   ;;         (let ((Object-columns (schema:get-columns LSST_tables-description "Object")))
   ;;           (list (columns "v" (columns->fields Object-columns))
   ;;                 (columns "o" (columns->fields Object-columns)))))

   ))

;; ---------------------------------------- ;;

