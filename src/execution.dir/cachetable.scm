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


(define generate-cache-table-name
  (let ((count 0)
        (prefix "cache__"))
    (lambda ()
      (set! count (+ count 1))
      (let ((str (number->string count)))
        (string-append prefix str)))))

(define (create-table-as table-name query)
  (string-append
   "CREATE TABLE " table-name
   " AS " query ))

(define-record-type :foreign-table
  (foreign-table table-name pool-name foreign-name field-type-list)
  foreign-table?
  (table-name      foreign-table->table-name)
  (pool-name       foreign-table->pool-name)
  (foreign-name    foreign-table->foreign-name)
  (field-type-list foreign-table->field-type-list))

(define (query-job->foreign-table database-schema qjob table-name foreign-name)
  (let* ((query     (query-job->query qjob))
         (pool      (query-job->pool qjob))
         (pool-name (pool->pool-name pool))
         (field-type-list (ast->type database-schema query)))

    '(begin
       (display (list 'query-job->foreign-table (ast->sql query)))
       (newline))
    
    (foreign-table
     table-name
     pool-name
     foreign-name
     field-type-list))) 

(define (create-foreign-table ftable)
  (let ((table-name      (foreign-table->table-name ftable))
        (pool-name       (foreign-table->pool-name ftable))
        (foreign-name    (foreign-table->foreign-name ftable))
        (field-type-list (foreign-table->field-type-list ftable)))
    (let* (

           ;; We have to  uniquify here because an arbitrary SQL query
           ;; could have multiple fields with the same name, however a
           ;; table cannot have columns with the same name.
           
           (field-type-list (field-type-unique field-type-list))
           (types (map field-type->type field-type-list))
           (types-string (apply string-append-with ", " types)))

      '(begin (display (list 'create-foreign-table ftable)) (newline))
      
      (string-append
       "CREATE FOREIGN TABLE " foreign-name " ( "
       types-string
       " ) SERVER " pool-name
       " OPTIONS ( TABLE_NAME  '" table-name "' );"))))

(define (cachetable:test database-schema)
  (unit-test
   (let* ((sql "SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51048
GROUP BY (objectId)
HAVING COUNT(sourceId) <= 2 ;")
          (ast (rewriter-ast sql))
          (result (extract-external-queries ast database-schema))
          (ext-query-vector (cadr result))
          (query (vector-ref ext-query-vector 5)))
     (create-foreign-table
      (query-job->foreign-table
       database-schema
       query "foo" "bar")))))