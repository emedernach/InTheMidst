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



(define-record-type :plan
  (plan sql pool)
  plan?
  (sql  plan->sql)
  (pool plan->pool))

(define-record-type :sequential-plan
  (sequential-plan_ plan-list)
  sequential-plan?
  (plan-list  sequential-plan->plan-list))

(define-syntax sequential-plan
  (syntax-rules ()
    ((sequential-plan plan ...)
     (sequential-plan_ (list plan ...)))))

(define-record-type :parallel-plan
  (parallel-plan_ plan-list)
  parallel-plan?
  (plan-list  parallel-plan->plan-list))

(define-syntax parallel-plan
  (syntax-rules ()
    ((parallel-plan plan ...)
     (parallel-plan_ (list plan ...)))))


;; (define master-pool (pool "master" "localhost" 5280 "postgres" "postgres"))

;; Query String -> planning = (list Query Cleanup)
(define (planner
         master-pool
         database-schema
         external-table->local-name
         sql result)

  (define (pool-queries
           query-job-vector definition-vector
           local-vector foreign-vector)
    (parallel-plan_
     (map (lambda (qjob)
            (let* ((id    (query-job->id qjob))
                   (query (query-job->query qjob))
                   (pool  (query-job->pool qjob))
                   (pool-name (pool->pool-name pool))
                   (local-query (replace-external-tables query external-table->local-name))
                   
                   ;; We must deanonymize columns in order to create a table !
                   (local-query (deanonymize local-query))
                   (local-sql   (ast->sql local-query))
                   (local-table-name (generate-cache-table-name))
                   (foreign-table-name (string-append "foreign_" local-table-name))
                   (local-creation-sql (create-table-as local-table-name local-sql))
                   (my-foreign-table (query-job->foreign-table
                                      database-schema 
                                      qjob
                                      local-table-name
                                      foreign-table-name))
                   (master-creation-sql (create-foreign-table my-foreign-table)))

              (if (string=? pool-name "master")
                  (begin
                    (vector-set! foreign-vector id #f)
                    (vector-set!
                     definition-vector id
                     (table-record 'unknown local-table-name))
                    (vector-set! local-vector id local-table-name)
                    ;; Just  create the  table  and  don't export  it
                    ;; locally (it is already local !)
                    (plan local-creation-sql pool))
                  (begin
                    (vector-set! foreign-vector id #t)
                    (vector-set!
                     definition-vector id
                     (table-record 'unknown foreign-table-name))
                    (vector-set! local-vector id local-table-name)
                    (sequential-plan
                     (plan local-creation-sql pool)
                     (plan master-creation-sql master-pool))))))
          (vector->list query-job-vector))))
  
  (define (final-query placeholder-query definition-vector)
    (let* ((final-ast (replace-placeholders
                       placeholder-query
                       definition-vector))
           ;; We must deanonymize columns in order to create a table !
           (final-ast (deanonymize final-ast))
           (final-query (ast->sql final-ast))
           (final-query (create-table-as result final-query)))
      (plan final-query master-pool)))
  
  (define (drop-tables query-job-vector local-vector)
    (sequential-plan_
     (map (lambda (qjob local-table-name)
            (let* ((pool  (query-job->pool qjob))
                   (sql (string-append "DROP TABLE " local-table-name)))
              (plan sql pool)))
          (vector->list query-job-vector)
          (vector->list local-vector))))
  
  (define (drop-foreign-tables definition-vector foreign-vector len)
    ;; This is  a loop  and not  a map  because we  need to
    ;; delete only  foreign tables  (cache tables  from the
    ;; master are local)
    (let loop ((index 0)
               (result '()))
      (if (< index len)
          (loop (+ index 1) 
                (let ((foreign? (vector-ref foreign-vector index)))
                  (if foreign?
                      (let* ((tr (vector-ref definition-vector index))
                             (foreign-table-name (table-record->name tr))
                             (sql (string-append "DROP FOREIGN TABLE " foreign-table-name))
                             (myplan (plan sql master-pool)))
                        (cons myplan result))
                      result)))
          (sequential-plan_ (reverse result)))))
                  
  (let* ((extquery (sql->plan database-schema sql))
         (placeholder-query (car extquery))
         (query-job-vector (cadr extquery))
         (len (vector-length query-job-vector))
         (definition-vector (make-vector len 'empty))
         (local-vector (make-vector len 'empty))
         (foreign-vector (make-vector len 'empty))
         (query-plan
          (pool-queries
           query-job-vector
           definition-vector 
	   local-vector 
	   foreign-vector))
         (final-query-plan
          (final-query placeholder-query definition-vector)))
    (list
     ;; Query 
     (sequential-plan query-plan final-query-plan)
     ;; Cleanup
     (sequential-plan
      (drop-tables query-job-vector local-vector)
      (drop-foreign-tables definition-vector foreign-vector len)))))



;; (define (plan:test)
;;   (let ((master-pool
;;          (pool "master" "localhost"
;;                5280 "postgres" "postgres")))
;;     (with-output-to-file "/tmp/plan_001.txt"
;;       (lambda () (planner
;;                   master-pool
;;                   database-schema
;;                   external-table->local-name
;;                   LSST_query_001_a
;;                   "T_LSST_query_001_a")))
;;     (with-output-to-file "/tmp/plan_003.txt"
;;       (lambda () (planner
;;                   master-pool
;;                   database-schema
;;                   external-table->local-name
;;                   LSST_query_003
;;                   "T_LSST_query_003")))))




    