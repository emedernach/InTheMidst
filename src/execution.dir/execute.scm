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



;; ----

;; (define free-result LIBPQ:PQclear)

;; TODO: Asynchronously ?

;; ----

;; The  way we  execute an  elementary plan  depends  on the
;; surrounding plan (sequential or parallel)

(define (execute myplan)

  (define (plan-dispatch.sequential myplan)
    (let ((start (time->seconds (current-time)))
          (sql  (plan->sql myplan))
          (pool (plan->pool myplan)))
      (let ((connection (connect-to-pool pool)))
        (display "Execution of ")
        (display sql)
        (let ((res (execute-sql-command connection sql)))
          (or (= res 0)
              ;; TODO: abort all other threads and do the cleanup
              (error "plan-dispatch: error executing SQL query" sql)))
        (end-connection connection)
        (display " done in : ")
        (display (- (time->seconds (current-time)) start))
        (newline)
        )))

  (define (plan-dispatch myplan)
    (let ((start (time->seconds (current-time)))
          (sql  (plan->sql myplan))
          (pool (plan->pool myplan)))
      (let ((connection (connect-to-pool pool)))
        (display "Execution of ")
        (display sql)
        (execute-asynchronous-sql-command connection sql)
        (end-connection connection)
        (display " done in : ")
        (display (- (time->seconds (current-time)) start))
        (newline)
        )))

  (define (sequential myplan)
    (let ((plan-list (sequential-plan->plan-list myplan)))
      (for-each execute plan-list)))

  (define (parallel myplan) 
    (let ((plan-list (parallel-plan->plan-list myplan)))
      (parallel-for-each execute plan-list)))

  ;; TODO: LIBPQ:PQsendQuery
  
  ;; TODO: we must execute _plans_ in parallel and not query
  ;; so  this is  more difficult that  it sounds !  and with
  ;; threads ?

  ;; TODO: use at most M connections and manage them  
  
  (cond ((plan? myplan) (plan-dispatch myplan))
        ((sequential-plan? myplan) (sequential myplan))
        ((parallel-plan? myplan) (parallel myplan))
        (else (error "execute: Unknown type" myplan))))