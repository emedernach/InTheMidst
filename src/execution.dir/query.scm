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

(define-record-type :query-job
  (query-job id query pool)
  query-job?
  (id     query-job->id)
  (query  query-job->query)
  (pool   query-job->pool))

;; -> New AST with placeholders and vector of external queries
(define (extract-external-queries ast myschema)

  (let ((tq (make-ticket-queue)))
  
    (define-ast-walker walker
      () ;;
      (dispatch  (sql-record?  sql-record-dispatch))

      ;; Dispatchers

      (define (sql-record-dispatch obj)
        ;; We don't extract external queries under LIMIT
        (if (sql-limit? (sql-record->limit  obj)) obj
            (let ((pool-result (external-query? obj myschema)))
              (if pool-result
                  (let* ((num (ticket-queue->number tq))

                         ;; We don't replace external tables here
                         ;; because we need the original query
                         ;; in order to obtain the table type.
                         (qjob (query-job num obj pool-result)))
                    (set! tq (ticket-queue-push tq qjob))
                    (placeholder num))
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
                                (dispatch limit))))))))

    (let ((new-ast (walker ast)))
      (list new-ast (ticket-queue->vector tq)))))
  
(define (sql->plan database-schema sql)
  (let ((ast (rewriter-ast sql)))
    (extract-external-queries ast database-schema)))

;; 
;; (define (tmp sql)
;;   (let* ((extquery (sql->plan sql))
;;          (vec (cadr extquery)))
;;     (pp (ast->sql (car extquery)))
;;     (newline)
;;     (for-each (lambda (qj)
;;                 (pp (ast->sql (query-job->query qj)))
;;                 (newline))
;;               (vector->list vec))))

(define (extract-external-queries:test database-schema)
  (unit-test
   (let* ((sql "SELECT  objectId, fluxToAbMag(zFlux_PS)
 FROM  Object 
 WHERE zFlux_PS > 0. 
 AND  fluxToAbMag(zFlux_PS) BETWEEN 25 AND 26 ;")
          (ast (rewriter-ast sql))
          (result (extract-external-queries ast database-schema))
          (ext-query-vector (cadr result)))
     ;; (pp (vector-ref ext-query-vector 0)) (newline)
     (= 18 (vector-length ext-query-vector)))
   (let* ((sql "SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51048
GROUP BY (objectId)
HAVING COUNT(sourceId) <= 2 ;")
          (ast (rewriter-ast sql))
          (result (extract-external-queries ast database-schema))
          (ext-query-vector (cadr result)))
     ;; (pp (query-job->query (vector-ref ext-query-vector 0)))
     ;; (newline)
     ;; (= 36 (vector-length ext-query-vector)))
     (pp (vector-length ext-query-vector)) (newline)
     (pp (ast->sql (query-job->query (vector-ref ext-query-vector 5))))
     (newline)
     'OK
     )))
     
