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

(define-record-type :table-type
  (table-type field-type-list)
  table-type?
  (field-type-list  table-type->field-type-list))

(define-record-type :field-type
  (field-type field-name field-datatype)
  field-type?
  (field-name  field-type->field-name)
  (field-datatype  field-type->field-datatype))

(define-record-type :schema
  (schema schema-table-list)
  schema?
  (schema-table-list  schema->schema-table-list))

(define-record-type :schema-table
  (schema-table table-name description virtual? pool parameters)
  schema-table?
  (table-name  schema-table->table-name)
  (description  schema-table->description)
  (virtual?  schema-table->virtual?)
  (pool  schema-table->pool)
  (parameters  schema-table->parameters))

(define (schema->table-description myschema table-name)
  (let loop ((schema-table-list (schema->schema-table-list myschema)))
    (and (not (null? schema-table-list))
         (let ((head (car schema-table-list))
               (rest (cdr schema-table-list)))
           (let ((head:table-name (schema-table->table-name head)))
             (if (string-ci=? head:table-name table-name)
                 (schema-table->description head)
                 (loop rest)))))))

(define-record-type :pool
  (pool pool-name hostname port dbname user)
  pool?
  (pool-name  pool->pool-name)
  (hostname   pool->hostname)
  (port       pool->port)
  (dbname     pool->dbname)
  (user       pool->user))

(define (pool=? pool_a pool_b)
  (if (and (pool? pool_a)
           (pool? pool_b))
      (and (string=? (pool->pool-name pool_a)
                     (pool->pool-name pool_b))
           (string=? (pool->hostname pool_a)
                     (pool->hostname pool_b))
           (= (pool->port pool_a)
              (pool->port pool_b)))
      (error "Not a pool"
             (list pool_a pool_b))))

;; An arbitrary  SQL query  could have multiple  fields with
;; the same name. Problem: this does not keep the ordering.
(define (field-type-unique field-type-list)
  
  (define (string-downcase str)
    (let* ((mylist (string->list str))
           (mylist (map char-downcase mylist)))
      (list->string mylist)))
  
  (let ((mytable (make-table)))
    (let loop ((field-type-list field-type-list))
      (if (null? field-type-list)
          (map (lambda (l) (apply field-type l))
               (table->list mytable))
          (let ((head (car field-type-list))
                (rest (cdr field-type-list)))
            (let ((field-name      (field-type->field-name head))
                  (field-datatype  (field-type->field-datatype head)))
              (let ((field-name-downcase (string-downcase field-name)))
                (table-set! mytable
                            field-name-downcase
                            (list field-datatype))
                (loop rest))))))))
  
;; A virtual table is a  table which exists for the end user
;; but not  physically.  For instance "Object"  is a virtual
;; table.   pool is  #f  is  the table  is  not an  external
;; table.  parameters   describes  the  way   the  table  is
;; partitionned (or is #f).

(define (table->pool myschema table-name)
  (let loop ((schema-table-list  (schema->schema-table-list myschema)))
    (and (not (null? schema-table-list))
         (let ((head (car schema-table-list))
               (rest (cdr schema-table-list)))
           (let ((head:table-name (schema-table->table-name head)))
             (if (string-ci=? head:table-name table-name)
                 (schema-table->pool head)
                 (loop rest)))))))





;; 
;; '(define (tabletypes:test)
;;    (unit-test
;;     (let ((pool1  (pool "pool1" "localhost"  1234 "postgres" "postgres"))
;;           (pool2  (pool "pool2" "localhost"  1235 "postgres" "postgres"))
;;           (pool2_ (pool "pool2" "localhost"  1235 "postgres" "postgres"))
;;           (pool3  (pool "pool2" "other.host" 1235 "postgres" "postgres")))
;;       (and (not (pool=? pool1 pool2))
;;            (pool=? pool2 pool2_)
;;            (not (pool=? pool2 pool3))))
;; 
;;     (let* ((source-result (schema->table-description database-schema "Source"))
;;            (filter-result (schema->table-description database-schema "Filter"))
;;            (field-type-list (table-type->field-type-list filter-result)))
;;       (and 
;;        (equal? source-result source-description)
;;        (equal? filter-result filter-description)
;;        (= 4 (length field-type-list))))
;; 
;;     (let ((pool2  (pool "pool2" "localhost" 5282 "postgres" "postgres")))
;;       (and 
;;        (not (table->pool database-schema "Source"))
;;        (pool=? pool2 (table->pool database-schema "master_object_007_xyz"))))
;;    
;;     ))