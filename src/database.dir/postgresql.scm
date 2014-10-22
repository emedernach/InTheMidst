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



;; Use execute-sql-select for SELECT statement

;; Use execute-sql-command for SQL commands (table creation, table drop, etc.)

;; TODO: Asynchronously 

'(define (connect-to-database)
  (connect-to-postgres))

(define (end-connection connection)
  ;; (LIBPQ:PQfinish connection)
  'end-of-connection)

;; Don't forget to clear result after usage !
(define free-result LIBPQ:PQclear)

(define (number-of-rows dbresult)
  (LIBPQ:PQntuples dbresult))

(define (connect-to-pool pool)
  (let* ((pool-name  (pool->pool-name pool))
         (hostname   (pool->hostname pool))
         (port       (number->string (pool->port pool)))
         (dbname     (pool->dbname pool))
         (user       (pool->user pool))
         (conninfo (string-append
                    "host=" hostname " port=" port
                    " dbname=" dbname " user=" user))
         (connection (LIBPQ:PQconnectdb conninfo)))
    (if (= (LIBPQ:PQstatus connection) 0)
        connection 
        (error "Database connection error" pool))))

'(define (connect-to-postgres)
   ;; (string-append "host=" host)
   ;; (string-append "dbname=" dbname)
   ;; (string-append "connect_timeout=" timeout)
   ;; (string-append "user=" user)
   ;; (string-append "password=" password)
   (let* ((conninfo "host=127.0.0.1 port=5280 dbname=postgres user=postgres")
          (connection (LIBPQ:PQconnectdb conninfo)))
     ;; (display "libPQ is thread safe : ")
     ;; (display (LIBPQ:PQisthreadsafe))
     ;; (newline)
     (if (= (LIBPQ:PQstatus connection) 0)
         connection 
         (error "Database connection error"))))

(define (run-query connection query)
  (let ((sql (rewriter query)))
    (display "Rewritten query = ") (pp sql) (newline) 
    (display "Querying database. (Please wait)") (newline)
    (let ((dbresult (time (execute-sql-select connection sql))))
      ;; (description (database-result->description dbresult))
      ;; (result      (database-result->vector dbresult)))
      dbresult)))



;; Replaced with execute-sql-select :

;; (define execute-query
;;   (let ((connection (connect-to-database))) 
;;     (lambda (query) (run-query connection query))))

(define (execute-query-with-timing-and-number-of-rows-only connection query)
  (let* ((start (time->seconds (current-time)))
         (dbresult (execute-sql-select connection query))
         (end (time->seconds (current-time))))
    (list (- end start) (number-of-rows dbresult)))) 

;; TODO:
;; - better display for tables (take into account stringth length and format)
;; - 

;; (define (display-dbresult dbresult)
  

(define (database-result->description dbresult)
  (let* ((nfields (LIBPQ:PQnfields dbresult))
         (result  (make-vector nfields 'void)))
    (let loop ((index 0))
      (if (= index nfields) result
          (let ((column-name (LIBPQ:PQfname dbresult index)))
            (vector-set! result index column-name)
            (loop (+ index 1))))))) 

(define (database-result->vector dbresult)
  (let* ((nfields (LIBPQ:PQnfields dbresult))
         (ntuples (LIBPQ:PQntuples dbresult))
         (result  (make-vector ntuples 'void)))
    (let loop ((index  0))
      (if (= index ntuples) result
          (let ((new-line (make-vector nfields 'void)))
            (let column-loop ((column 0))
              (if (= column nfields)
                  (begin
                    (vector-set! result index new-line)
                    (loop (+ index 1)))
                  (let ((content (LIBPQ:PQgetvalue dbresult index column)))
                    (vector-set! new-line column content)
                    (column-loop (+ column 1)))))))))) 

(define (database-result-vector-get-line dbresult index)
  (let* ((nfields (LIBPQ:PQnfields dbresult))
         (result (make-vector nfields 'void)))
    (let loop ((column 0))
      (if (= column nfields) result
          (let ((content (LIBPQ:PQgetvalue dbresult index column)))
            (vector-set! result column content)
            (loop (+ column 1)))))))    

(define (database-result-list-get-line dbresult index)
  (let* ((nfields (LIBPQ:PQnfields dbresult)))
    (let loop ((column 0) (result '()))
      (if (= column nfields)
          (reverse result)
          (let ((content (LIBPQ:PQgetvalue dbresult index column)))
            (loop (+ column 1)
                  (cons content result)))))))

(define (database-result-vector-map fun dbresult)
  (let* ((nfields (LIBPQ:PQnfields dbresult))
         (ntuples (LIBPQ:PQntuples dbresult))
         (result  (make-vector ntuples 'void)))
    (let loop ((index 0))
      (if (= index ntuples) result
          (let ((content (database-result-list-get-line dbresult index)))
            (vector-set! result index (apply fun content))
            (loop (+ index 1)))))))
    
(define (database-result-fold kons knil dbresult)
  (let* ((nfields (LIBPQ:PQnfields dbresult))
         (ntuples (LIBPQ:PQntuples dbresult))
         (result  (make-vector ntuples 'void)))
    (let loop ((index 0) (result knil))
      (if (= index ntuples) result
          (let ((content (database-result-list-get-line dbresult index)))
            (loop (+ index 1)
                  (kons content result)))))))
    

;;;; (define (execute-query connection query)
;;;;   (let* ((start (time->seconds (current-time)))
;;;;          (res (execute-sql-select connection query))
;;;;          (end (time->seconds (current-time)))
;;;;          (nfields (LIBPQ:PQnfields res))
;;;;          (ntuples (LIBPQ:PQntuples res)))
;;;;     (let table-field-loop ((i 0))
;;;;       (if (< i nfields)
;;;;           (begin
;;;;             (display (LIBPQ:PQfname res i))
;;;;             (display " | ")
;;;;             (table-field-loop (+ i 1)))
;;;;           (newline)))
;;;;     (let tuple-loop ((i 0))
;;;;       (if (< i ntuples)
;;;;           (begin
;;;;             (display " | ")
;;;;             (let field-loop ((j 0))
;;;;               (if (< j nfields)
;;;;                   (begin 
;;;;                     (display (LIBPQ:PQgetvalue res i j))
;;;;                     (display " | ")
;;;;                     (field-loop (+ j 1)))
;;;;                   (begin
;;;;                     (newline)
;;;;                     (tuple-loop (+ i 1))))))
;;;;           (begin
;;;;             (display (list "Time:" (- end start))) (newline)
;;;;             (display (list "#Lines:" (number-of-rows res))) (newline))))))
;;;; 
;;;; (define (execute-query_old connection query)
;;;;   (let* ((res (execute-sql-select connection query))
;;;;          (nfields (LIBPQ:PQnfields res))
;;;;          (ntuples (LIBPQ:PQntuples res)))
;;;;     (let table-field-loop ((i 0))
;;;;       (if (< i nfields)
;;;;           (begin
;;;;             (display (LIBPQ:PQfname res i))
;;;;             (display " | ")
;;;;             (table-field-loop (+ i 1)))
;;;;           (newline)))
;;;;     (let tuple-loop ((i 0))
;;;;       (if (< i ntuples)
;;;;           (begin
;;;;             (display " | ")
;;;;             (let field-loop ((j 0))
;;;;               (if (< j nfields)
;;;;                   (begin 
;;;;                     (display (LIBPQ:PQgetvalue res i j))
;;;;                     (display " | ")
;;;;                     (field-loop (+ j 1)))
;;;;                   (begin
;;;;                     (newline)
;;;;                     (tuple-loop (+ i 1))))))
;;;;           'Ok))))
  