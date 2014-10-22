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

;; A query is an external query iff:
;; - All its tables are from the same pool
;; - There is no free variables

;; Return either the common pool name of #f

(define (external-query? ast myschema)

  (define (multitable-query? obj)
    (let ((from  (sql-record->from obj)))
      (not (table-record? from))))

  (define (filtering-query? obj)

    (define-ast-walker helper
      (exit) ;;
      (dispatch (sql-record?  sql-record-dispatch))

      (define (sql-record-dispatch obj)
	(let ((select    (sql-record->select  obj))
	      (from      (sql-record->from  obj))
	      (where     (sql-record->where  obj))
	      (group-by  (sql-record->group-by  obj))
	      (order-by  (sql-record->order-by  obj))
	      (limit     (sql-record->limit  obj)))
	  (if (not (eq? where 'empty-where))
	      (exit #t)
	      (sql-record (dispatch select)
			  (dispatch from)
			  (dispatch where)
			  (dispatch group-by)
			  (dispatch order-by)
			  (dispatch limit))))))

    (call-with-current-continuation
     (lambda (exit) 
       (helper obj exit) #f)))

;;  (define (filtering-query? obj)
;;    (let ((where (sql-record->where obj)))
;;      (not (eq? where 'empty-where))))

  (if (sql-record? ast)
      ;; One table query should not be externalised
      (and ;; (multitable-query? ast)
           (filtering-query? ast)
           (let ((pool-result (same-pool? ast myschema)))
             (and pool-result
                  (not (free-variables-present? ast myschema))
                  pool-result)))
      (error "Not a SQL record" ast)))

(define (external-query:test database-schema)
  (unit-test
   (let* ((sql "SELECT * FROM Source s, Filter f WHERE s.filterId = f.filterId")
          (ast (sql->ast sql)))
     ;; Not an external query because not on the same pool (Filter is on the master)
     (not (external-query? ast database-schema)))
   (let* ((sql "SELECT toto FROM master_source_016_xyz;")
          (ast (sql->ast sql)))
     (not (external-query? ast database-schema)))
   (let* ((sql "SELECT * FROM master_object_001_xyz
WHERE (-2.5  * log(zFlux_PS) - 48.6) BETWEEN 25 AND 26
AND zFlux_PS > 0. ;")
          (ast (sql->ast sql)))
     (external-query? ast database-schema))
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
     (external-query? ast database-schema)
     ;; (same-pool? ast database-schema)
     )
   ))
