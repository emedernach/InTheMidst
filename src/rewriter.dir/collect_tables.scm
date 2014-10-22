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



;; This file is included by collect.scm

;; Collect tables from the FROM clause of a query,
;; do  not proceed  subqueries as  tables  used in
;; subqueries  cannot  be  accessed by  the  query
;; above. However we have to proceed aliases.

;; Returns  a list  of table-record,  for  each we
;; have to  lookup the field names  in an external
;; table description, or  a columns structure with
;; a description of field names.

(define (collect-tables ast)

  (define (dispatch obj)
    (cond 
     ((sql-record? obj) (sql-record-dispatch obj))
     ((join-table-using? obj) (join-table-using-dispatch obj))
     ((join-table-on? obj) (join-table-on-dispatch obj))
     ((table-record? obj)  (table-dispatch obj))
     ((cartesian-product? obj) (cartesian-product-dispatch obj))
     ((alias? obj)  (alias-dispatch obj))
     ((parenthesised-expression? obj)  (parenthesised-expression-dispatch obj))
     ((table-combinator? obj) (table-combinator-dispatch obj))
     ((predicate? obj) '())
     ((field? obj) '())
     (else (error "collect-tables: Unknown type" obj))))

  ;;

  (define (sql-record-dispatch obj)
    (let ((select    (sql-record->select  obj))
          (from      (sql-record->from  obj))
          (where     (sql-record->where  obj))
          (group-by  (sql-record->group-by  obj))
          (order-by  (sql-record->order-by  obj))
          (limit     (sql-record->limit  obj)))
      (list (dispatch from))))

  (define (join-table-using-dispatch obj)
    (let ((type         (join-table-using->type obj))
          (left-table   (join-table-using->left-table obj))
          (right-table  (join-table-using->right-table obj))
          (join-fields  (join-table-using->join-fields obj)))
      (list (dispatch left-table)
            (dispatch right-table))))

  (define (join-table-on-dispatch obj)
    (let ((type        (join-table-on->type obj))
          (left-table  (join-table-on->left-table obj))
          (right-table (join-table-on->right-table obj))
          (predicate   (join-table-on->predicate obj)))
      (list (dispatch left-table)
            (dispatch right-table))))
  
  (define (table-dispatch obj) (list obj))

  (define (cartesian-product-dispatch obj)
    (let ((table-list (cartesian-product->tables obj)))
      (map dispatch table-list)))

  (define (alias-dispatch obj)
    (let ((name (alias->name obj))
          (val  (alias->value obj)))
      
      ;; TODO: do we have to dispatch on val ?

      ;; REMARK: to access a field from an aliased
      ;; table we should  prefix it with the alias
      ;; name. Conclusion: these tables must never
      ;; be  ambiguously used. EXCEPT  THAT the id
      ;; field used to join tables need this.

      ;; TODO: Well, in  case of a sql subquery we
      ;;  need to collect  fields from  the select
      ;; statement (renaming).

      ;;   TODO:  In fact  all that matter  for an
      ;;  alias  is the list of  the table fields,
      ;;  does the value must be instead a list of
      ;;  fields ? Better use another structure !
      
      (columns name (collect-from-fields val))))

  (define (parenthesised-expression-dispatch obj)
    (let ((expr  (parenthesised-expression->expr obj)))
      (dispatch expr)))

  (define (table-combinator-dispatch obj)
    (let ((table (table-combinator->table obj))
          (list-of-table-combinators (table-combinator->list-of-table-combinators obj)))
      (dispatch table)))

  ;;
  
  (flatten (dispatch ast)))

;; 

(define (collect-tables:test)
  (unit-test
   (equal? (collect-tables (SQL->AST LSST_query_001_a))
           (list (table-record 'Unknown "Source")
                 (table-record 'Unknown "Filter")))

   (equal? (collect-tables (SQL->AST LSST_query_002))
           (list (table-record 'Unknown "Object")
                 (table-record 'Unknown "_ObjectToType")
                 (table-record 'Unknown "ObjectType")))

   (equal? (collect-tables (SQL->AST LSST_query_003))
           (list (table-record 'Unknown "Object")))   

   (equal? (collect-tables (SQL->AST LSST_query_013))
           (list (columns "o1" (fields-from-tables (list (table-record 'Unknown "Object"))))
                 (columns "o2" (fields-from-tables (list (table-record 'Unknown "Object"))))))

   (equal? 
    (let ((str "SELECT G.objectId, COUNT(N.NeighborObjID)  AS pop
FROM Galaxy AS G
JOIN Neighbors AS N ON  (G.objectId = N.objectId) 
JOIN Galaxy AS U
ON (U.objectId = N.neighborObjId)  
JOIN photoZ AS Gpz ON (G.objectId
= Gpz.objectId)  
JOIN photoZ AS  Npz ON (U.objectId  = Npz.objectId)"))
      (collect-tables (SQL->AST str)))
    (list (columns "G"   (fields-from-tables (list (table-record 'Unknown "Galaxy"))))
          (columns "N"   (fields-from-tables (list (table-record 'Unknown "Neighbors"))))
          (columns "U"   (fields-from-tables (list (table-record 'Unknown "Galaxy"))))
          (columns "Gpz" (fields-from-tables (list (table-record 'Unknown "photoZ"))))
          (columns "Npz" (fields-from-tables (list (table-record 'Unknown "photoZ"))))))

   (equal? (let ((str "SELECT objID
FROM (SELECT objID, ra, dec FROM Geometry 
      WHERE ra BETWEEN 250 AND 270 AND dec > 50 ) 
JOIN (SELECT objID, g FROM Luminosity) USING (objID) 
JOIN (SELECT objID, rho FROM Galaxy_others) USING (objID) 
WHERE (g+rho) between 23 and 25 ;"))
             (collect-tables (SQL->AST str)))
           (list (table-record 'Unknown "Geometry")
                 (table-record 'Unknown "Luminosity")
                 (table-record 'Unknown "Galaxy_others")))

   (equal? (collect-tables (SQL->AST "SELECT * FROM (A JOIN B ON A.a_id =B.b_id) AS AB ;"))
           (list
            (columns
             "AB"
             (union-table-fields
              (fields-from-tables (list (table-record 'Unknown "A")))
              (fields-from-tables (list (table-record 'Unknown "B")))))))

   
   (equal? (let ((str "SELECT ABCs.a_id, *
FROM (A JOIN B ON A.a_id =B.b_id 
   JOIN C on B.b_id=C.c_id) AS ABCs 
LEFT JOIN (X JOIN Y on X.x_id=Y.y_id 
   JOIN Z on Y.y_id=Z.z_id) AS XYZs on ABCs.a_id=XYZs.y_id ;"))
             (collect-tables (SQL->AST str)))
           (list (columns
                  "ABCs"
                  (union-table-fields
                   (union-table-fields
                    (fields-from-tables (list (table-record 'Unknown "A")))
                    (fields-from-tables (list (table-record 'Unknown "B"))))
                   (fields-from-tables (list (table-record 'Unknown "C")))))
                 (columns
                  "XYZs"
                  (union-table-fields
                   (union-table-fields
                    (fields-from-tables (list (table-record 'Unknown "X")))
                    (fields-from-tables (list (table-record 'Unknown "Y"))))
                   (fields-from-tables (list (table-record 'Unknown "Z")))))))
           
   (equal? (let ((str "SELECT *
FROM ( SELECT id, a AS a_12, b AS b_12 FROM T1, T2 WHERE T1.id = T2.id AND T2.c > 45.6) AS T12
JOIN ( SELECT id, a, b, c FROM T3 ) AS T3_
USING (id) ;"))
     (collect-tables (SQL->AST str)))
           (list (columns "T12"  (list "id" "a_12" "b_12"))
                 (columns "T3_"  (list "id" "a" "b" "c"))))
                 
                 
   
   (equal? (let ((str "SELECT * 
FROM (SELECT * FROM master_table_1 WHERE id BETWEEN  1 AND 10
  UNION ALL SELECT * FROM master_table_2 WHERE id BETWEEN 11 AND 20
  UNION ALL SELECT * FROM master_table_3 WHERE id BETWEEN 21 AND 30
  UNION ALL SELECT * FROM master_table_4 WHERE id BETWEEN 31 AND 40
  ) AS master_table
WHERE id=12 ;"))
             (collect-tables (SQL->AST str)))
           (list (columns
                  "master_table"
                  (list
                   (fields-from-tables 
                    (list (table-record 'Unknown "master_table_1")))))))

   ;; subquery: case02_3003_query_025
   ;; The subquery "inherits" the tables from its parent.
   
   (equal? (collect-tables (SQL->AST case02_3003_query_025))
           (list (columns "o1" (fields-from-tables (list (table-record 'Unknown "Object"))))
                 (columns "o2" (fields-from-tables (list (table-record 'Unknown "Object"))))))

   ;; Here 'c' and 'd' fields could be from T1 or T2.
   ;; For instance 'c'  is from T2  and 'd' from T1
   (equal? (let ((str "SELECT id, a
FROM T1
WHERE ( SELECT b FROM T2 WHERE c=d) > 123 ;"))
              (collect-tables (SQL->AST str)))
           (list (table-record 'Unknown "T1")))
   
   ))
