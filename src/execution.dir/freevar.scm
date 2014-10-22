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


;; Un FROM  au niveau d'un "SELECT .."  introduit un nouveau
;; scope Toutes  les references dans ce "SELECT  .." se font
;; relativement a ce scope, sauf pour le FROM.

;; Il faut connaitre les champs de toutes les tables

;;  Le  scope d'une  sous  requete est  le  scope du  parent
;;  auquel on rajoute les tables de la sous requete.

;;  Lorsqu'on  rencontre  une variable  il  faut essayer  de
;; l'associer a une  table.  Si la variable est  de la forme
;; <table>.<var>  alors il faut  s'assurer que  <table> fait
;; partie des  tables connues dans  le scope actuel.   Si la
;; variable est  de la forme <var> alors  il faut rechercher
;; de quel table  elle fait partie dans le  scope actuel. Si
;; on  ne  trouve pas  de  table  c'est  qu'il s'agit  d'une
;; variable libre.

(define (free-variables-present? ast myschema)

  ;; We exit as soon as a free variable is detected.
  (call-with-current-continuation
   (lambda (exit)

     (let ((current-scope (make-scope)))
  
       (define-ast-walker walker
         () ;;
         (dispatch
          (join-table-using? join-table-using-dispatch)
          (field? field-dispatch)
          (sql-record?  sql-record-dispatch))

         ;; Dispatchers

         ;; We don't dispatch on join-fields
         ;; For instance:
         ;; A JOIN B USING(id)
         ;; 'id' is not bounded in scope but comes from both A and B
         
         (define (join-table-using-dispatch obj)
           (let ((type         (join-table-using->type obj))
                 (left-table   (join-table-using->left-table obj))
                 (right-table  (join-table-using->right-table obj))
                 (join-fields  (join-table-using->join-fields obj)))
             (join-table-using type
                               (dispatch left-table)
                               (dispatch right-table)
                               ;; (map dispatch join-fields))))
                               join-fields)))
         
         (define (field-dispatch obj)
           (if (bounded-in-scope? current-scope obj)
               obj (exit #t)))
           
         (define (sql-record-dispatch obj)
           (let* ((select    (sql-record->select  obj))
                  (from      (sql-record->from  obj))
                  (where     (sql-record->where  obj))
                  (group-by  (sql-record->group-by  obj))
                  (order-by  (sql-record->order-by  obj))
                  (limit     (sql-record->limit  obj))

                  ;; FROM scope is the scope of its surrounding query
                  (new-from (dispatch from))
                  (frame (from->frame from myschema current-scope))
                  (old-scope current-scope)
                  (new-scope (scope-add-frame current-scope frame)))
             
             (set! current-scope new-scope)
             (sql-record (dispatch select)
                         new-from
                         (dispatch where)
                         (dispatch group-by)
                         (dispatch order-by)
                         (dispatch limit))
             (set! current-scope old-scope)
             obj )))

       (begin (walker ast) #f )))))

(define (freevar:test database-schema)
  (unit-test
   (let ((ast (sql->ast "SELECT * FROM Object WHERE foo > 12")))
     (free-variables-present? ast database-schema))
   (let ((ast (sql->ast "SELECT * FROM Object WHERE objectid = 123")))
     (not (free-variables-present? ast database-schema)))
   (let* ((sql "SELECT * FROM Source s, Filter f WHERE s.filterId = f.filterId")
          (ast (sql->ast sql)))
     (not (free-variables-present? ast database-schema)))
   (let ((ast (sql->ast "SELECT * FROM Object o1 WHERE (SELECT o1.objectid, o2.objectid FROM Object o2 WHERE o1.htmId20 = o2.htmId20) > 10000;")))
     (not (free-variables-present? ast database-schema)))
   (let ((ast (sql->ast "SELECT * FROM Object o1, Source s, (SELECT o2.objectid FROM Object o2 WHERE o2.objectid = s.objectid) AS T1;")))
     ;; 's' in the subquery should not refer to Source s in the FROM
     (free-variables-present? ast database-schema))
   (let* ((sql "SELECT *
FROM (SELECT *
      FROM master_object_015_xyz
      WHERE latestObsTime > 51048 )
  AS __WRAPPED_INNERJOIN__8
INNER JOIN
 (SELECT * FROM master_source_015_xyz)
  AS __WRAPPED_INNERJOIN__9
USING ( objectId )" )
          (ast (sql->ast sql)))
     (not (free-variables-present? ast database-schema)))
   ))