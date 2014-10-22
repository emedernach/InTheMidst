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
(include "framedef.scm")

;; Convert a FROM clause to a frame

;; Warning: A JOIN B, C => A, B, C

;; Warning: A table variable  may refer either to the global
;; scope  (which is the  database schema)  or the  the local
;; scope (created  with aliases  for example).  But  for SQL
;; variable we could only use local scope.

(define (from->frame from database-schema local-scope)

  (define (table->table-definition tbl)
    (cond
     ((table-record? tbl)
      (table-record->table-definition tbl))
     ((alias? tbl) (alias->table-definition tbl))
     (else (error "Unknown type:" tbl))))
  
  (define (ast-> ast)
    (ast->field-type-list ast database-schema local-scope))

  (define (table->name tbl)
    (cond
     ((table-record? tbl) (table-record->name tbl))
     ((alias? tbl) (alias->name tbl))
     (else (error "Unknown type" tbl))))
  
  (define (dispatch-from)
    (cond
     ((table-record? from) (table-record->frame from))
     ((alias? from) (alias->frame from))
     ((cartesian-product? from) (cartesian-product->frame from))
     ((inner-joins? from) (inner-joins->frame from))
     ((join-table-using? from) (join-table-using->frame from))
     ((parenthesised-expression? from)
      (let ((val (parenthesised-expression->expr from)))
        (from->frame val local-scope)))
     (else (error "Unknown type:" from))))

  ;; Dispatchers

  (define (table-record->table-definition tbl)
    (let* ((name (table-record->name tbl))
           (field-type-list
            (search-in-all-scope->field-type-list
             database-schema local-scope name)))
      (table-definition name field-type-list)))
   
  (define (table-record->frame tbl)
    (let ((td (table-record->table-definition tbl)))
      (frame (list td))))

  (define (alias->table-definition al)
    (let* ((name   (alias->name al))
           (val    (alias->value al))
           (field-type-list (ast-> val)))
      (table-definition name field-type-list)))
  
  (define (alias->frame al)
    (let ((td (alias->table-definition al)))
      (frame (list td))))

  (define (cartesian-product->frame cp)
    (let* ((table-list  (cartesian-product->tables cp))
           (table-names-list (map table->name table-list))
           (field-type-list-list  (map ast-> table-list))
           (tref-list (map table-definition
                           table-names-list
                           field-type-list-list))
           (table-definition-list tref-list))
      (frame table-definition-list)))
      
  (define (inner-joins->frame ij)
    (let* ((table-list  (inner-joins->table-list ij))
           (table-definition-list
            (map table->table-definition
                 table-list)))
      (frame table-definition-list)))
          
  (define (join-table-using->frame jtu)
    (let* ((left-table  (join-table-using->left-table  jtu))
           (right-table (join-table-using->right-table jtu))
           (left-td (table->table-definition left-table))
           (right-td (table->table-definition right-table))
           (table-definition-list (list left-td right-td)))
      (frame table-definition-list)))

  ;; Body
  
  (dispatch-from))


(define (from:test database-schema)
  (unit-test

   (let* ((frame_1
           (make-frame
            ("T1" (("a" "integer")
                   ("b" "real")
                   ("c" "real")))
            ("T2" (("d" "integer")
                   ("e" "real")))))
          (frame_2
           (make-frame
            ("T3" (("f" "bigint")
                   ("g" "real")
                   ("h" "integer")
                   ("i" "real")))))
          (myscope (make-scope))
          (myscope (scope-add-frame myscope frame_1))
          (myscope (scope-add-frame myscope frame_2))
          (sql  "SELECT * FROM Object, T2")
          (ast  (sql->ast sql))
          (from (sql-record->from ast))
          (myframe (from->frame from database-schema myscope))
          (object:field-type-list (frame:find-table myframe "Object"))
          (t2:field-type-list (frame:find-table myframe "T2")))
     (display t2:field-type-list)
     (newline)
     'Ok)

   ))
   