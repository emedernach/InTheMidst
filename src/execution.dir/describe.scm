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



;; Functions to  obtain the list of fields  with their types
;; from an AST.

(define (search-in-all-scope->field-type-list
         database-schema local-scope name)
  (let ((boolean_local-scope (scope->field-type-list local-scope name)))
    '(begin
       (display (list 'search-in-all-scope->field-type-list name boolean_local-scope))
       (newline))
    (or boolean_local-scope
        (let ((tmp (schema->table-description database-schema name)))
          ;; tmp is a table-type
          (and tmp (table-type->field-type-list tmp)))
        (error "Name not found in local scope not in global tables" name))))

(define (ast->field-type-list ast database-schema local-scope)

  (define (search-in-all-scope name)
    (or (scope->field-type-list local-scope name)
        (schema->table-description database-schema name)
        (error "Name not found in local scope not in global tables" name)))  

  ;; Dispatchers

  (define (sql-record->field-type-list ast)
    (let* ((select (sql-record->select ast))
           (from   (sql-record->from ast))
           ;; Take all field type list in from
           (from-frame (from->frame from database-schema local-scope))
           ;; Then project with select
           (field-list (projection->fields select)))
      ;; Search for field-type for all fields
      ;; Beware of '*' !
      '(begin
        (display (list 'sql-record->field-type-list
                       ;; from-frame
                       field-list))
        (newline))
      (frame:search-fields from-frame field-list)))
  
  (define (table-record->field-type-list ast)
    (let ((name (table-record->name ast)))
      (search-in-all-scope->field-type-list
       database-schema local-scope name)))
      

  (cond

   ((parenthesised-expression? ast)
    (let ((val (parenthesised-expression->expr ast)))
      (ast->field-type-list val database-schema local-scope)))

   ((table-record? ast) (table-record->field-type-list ast))

   ((alias? ast)
    (let ((val (alias->value ast)))
      (ast->field-type-list val database-schema local-scope)))

   ((sql-record? ast) (sql-record->field-type-list ast))
   
   (else (error "ast->field-type-list: Unknown type" ast))))