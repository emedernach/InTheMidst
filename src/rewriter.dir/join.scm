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

;; We replace tree of inner joins with a list using inner-join associativity.

(define-ast-walker join-associativity
  () ;; arguments
  (dispatch (join-table-using?  join-table-using-dispatch))

  ;; "SELECT * FROM <from>"
  (define (unselect ast)
    (if (sql-record? ast)
        (let ((select    (sql-record->select  ast))
              (from      (sql-record->from  ast))
              (where     (sql-record->where  ast))
              (group-by  (sql-record->group-by  ast))
              (order-by  (sql-record->order-by  ast))
              (limit     (sql-record->limit  ast)))
          (if (and (projection? select)
                   (let ((fields (projection->fields select)))
                     (and (pair? fields)
                          (null? (cdr fields))
                          (let ((f (car fields)))
                            (and (field? f)
                                 (eq? (field->table f) 'unknown)
                                 (string=? (field->column f) "*")))))
                   (eq? where 'empty-where)
                   (eq? group-by 'empty-group)
                   (eq? order-by 'empty-order-by)
                   (eq? limit 'empty-limit))
              from
              ast))
        ast))

  (define (unparenthesised ast)
    (if (parenthesised-expression? ast)
        (unselect (parenthesised-expression->expr ast))
        ast))
  
  (define (unalias ast)
    (if (alias? ast)
        (begin
          ;; (display (list "Aliases are not supported inside JOINS:" (alias->name ast)))
          ;; (newline)
          (unparenthesised (alias->value ast)))
        ast))
  
  (define (join-table-using-dispatch obj)
    (let ((type         (join-table-using->type obj))
          (left-table   (dispatch (unalias (join-table-using->left-table obj))))
          (right-table  (dispatch (unalias (join-table-using->right-table obj))))
          (join-fields  (join-table-using->join-fields obj)))
      (if (eq? type 'inner)
          (cond ((and (inner-joins? left-table)
                      (inner-joins? right-table))
                 (let* ((left-table-list (inner-joins->table-list left-table))
                        (left-table-join-fields-list (inner-joins->join-fields-list left-table))
                        (right-table-list (inner-joins->table-list right-table))
                        (right-table-join-fields-list (inner-joins->join-fields-list right-table))
                        (new-table-list (append left-table-list right-table-list))
                        (new-join-fields (append left-table-join-fields-list right-table-join-fields-list))
                        (new-join-fields (cons join-fields new-join-fields)))
                   (inner-joins new-table-list new-join-fields)))
                ((inner-joins? left-table)
                 (let ((left-table-list (inner-joins->table-list left-table))
                       (left-table-join-fields-list (inner-joins->join-fields-list left-table)))
                   (inner-joins (cons right-table left-table-list)
                                (cons join-fields left-table-join-fields-list))))
                ((inner-joins? right-table)
                 (let ((right-table-list (inner-joins->table-list right-table))
                       (right-table-join-fields-list (inner-joins->join-fields-list right-table)))
                   (inner-joins (cons right-table right-table-list)
                                (cons join-fields right-table-join-fields-list))))
                (else (inner-joins (list left-table right-table) (list join-fields))))                       
          obj)))

  ;; End of join-associativity

  )
   
;; Probleme: la  mise a  plat des inner  join ne  marche pas
;; parce qu'on perd l'information  sur les alias qui forment
;; un arbre  (par exemple ci-dessus on a  les alias Object_,
;; Object, Source_, Source)

'(define-ast-walker join-add-alias
  () ;; arguments
  (dispatch (join-table-using?  join-table-using-dispatch))

  (define (make-aliasname)
    (string-append
     "__JOIN__" 
     (number->string (gensym))))
     
  (define (join-table-using-dispatch obj)
    (let ((type         (join-table-using->type obj))
          (left   (remove-and-collect-alias (join-table-using->left-table obj)))
          (right  (remove-and-collect-alias (join-table-using->right-table obj)))
          (join-fields  (join-table-using->join-fields obj)))
      (let ((left-table    (vector-ref left 0))
            (left-aliases  (vector-ref left 1))
            (right-table   (vector-ref right 0))
            (right-aliases (vector-ref right 1)))
        (let ((new-join (join-table-using type left-table right-table join-fields)))
          (make-alias (make-aliasname) new-join)))))

  )

'(define-ast-walker join-renaming
  () ;; arguments
  (dispatch  (sql-record? sql-record-dispatch))

  (define (sql-record-dispatch ast)
    (let ((select    (sql-record->select  ast))
          (from      (sql-record->from  ast))
          (where     (sql-record->where  ast))
          (group-by  (sql-record->group-by  ast))
          (order-by  (sql-record->order-by  ast))
          (limit     (sql-record->limit  ast)))

      ;; Ajouter un alias autour du join le plus haut
      ;; Collecter tout les alias de tables dans les join et les supprimer
      ;; Remplacer dans where, group-by, order-by tout les alias par cet alias global

      ast
      
      ))
  
  ;; End of join-renaming
  )