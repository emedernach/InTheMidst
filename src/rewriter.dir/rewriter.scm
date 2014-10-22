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

(define (explain-rewriter sql)
  (string-append
   "EXPLAIN ANALYZE  "
   (AST->SQL (rewriter-ast sql))))

(define (explain-rewriter_strategy_2 sql)
  (string-append
   "EXPLAIN ANALYZE  "
   (AST->SQL (rewriter-ast_strategy_2 sql))))

;; sql is a SQL string 
(define (rewriter sql)
  (AST->SQL (rewriter-ast sql)))

(define (rewriter-ast sql)
  (display "Transforming AST.") (newline)
  (let* ((default-database "LSST")
         (Object-table (table-record 'Unknown "Object"))        
         (Object_-table (table-record 'Unknown "Object_"))        
         (Source-table (table-record 'Unknown "Source"))
         (Source_-table (table-record 'Unknown "Source_"))

         (Object-virtual-table FDW_Object)
         (Source-virtual-table FDW_Source)
         
         (tables-description FDW_tables-description)
         (table->fields FDW_table->fields)
         (cardinality-one? FDW_cardinality-one?)
         (choice FDW_choice)
      
         ;; Replace all virtual tables

         (ast (begin (display "Parsing SQL.") (newline) (SQL->AST sql)))
         
         ;; Crossmatch are replaced by spatial joins
         (ast (crossmatch-rewriter ast))

         (ast (join-validation ast table->fields))

         ;; Functions are replaced and computed if needed
         (ast (replace-functions ast functions-hash))
         
         ;; We expand OR into UNION in order to have only AND (conjunctive queries)
         (ast (expand-or ast))
         
         ;; We replace Virtual tables by their definition
         
         (ast (replace-table ast default-database Object-table Object-virtual-table))
         (ast (replace-table ast default-database Source-table Source-virtual-table))
         
         ;; count(*) transformation
         (ast (count-rewriter ast cardinality-one? choice))
         
         ;; Field extraction and dispatch
         ;; Notice that we now have only conjunctive predicates (series of AND)
         ;; ex: Geometry extraction in order to call UDF
         (ast (distribute-predicate ast table->fields tables-description))
        
         ;; Join associativity
         
         ;;  TODO:  In order  to transform/move tables  inside a
         ;; join we have  to remove aliases around joined tables
         ;; and  search/replace all of these  aliases usage. One
         ;; idea is  to name the inner join,  remove all aliases
         ;; and replace them with  the name of the joined table.
         ;;  However what happen  if 2  tables share  some field
         ;; names  ?  Does "(..)  AS  A JOIN (..)  AS  B .. A.f"
         ;;  select the  fields from  A  ? Yes,  so this  global
         ;;  renaming idea  is not  valid. The  problem  is that
         ;;  because of transformation  the original  tables are
         ;; not there anymore and we cannot refer to it not name
         ;; it.

         ;; We could  impose that  when A  JOIN B  there  is no
         ;; common fields  between A  and B  other  that fields
         ;; listed in USING ?

         ;; Or we could say that we don't support aliases inside
         ;; JOINs: if needed user  could create an alias for the
         ;; whole join instead.

         ;; The  whole join alias  works as an umbrella  for all
         ;; aliases inside it. What is the scope of aliases ?

         (ast (join-associativity ast))
         
         (ast (distributed-query-fusion ast))
         
         (ast (inner-join-unfolding ast table->fields))
         
         (ast (distribute-predicate ast table->fields tables-description))
         
         (ast (replace-all-partition-tables ast FDW_modulo-partitions))
         
         )

    ast))

(define (rewriter-ast_strategy_2 sql)
  (let* ((default-database "LSST")
         (Object-table (table-record 'Unknown "Object"))        
         (Object_-table (table-record 'Unknown "Object_"))        
         (Source-table (table-record 'Unknown "Source"))
         (Source_-table (table-record 'Unknown "Source_"))

         (Object-virtual-table (sql->ast FDW_Object))
         (Object-virtual-table (replace-table Object-virtual-table
                                              default-database
                                              Object_-table
                                              ;; (sql->ast FDW_Object_)))
                                              FDW_Object_))
                                              
         (Source-virtual-table (sql->ast FDW_Source))
         (Source-virtual-table (replace-table Source-virtual-table
                                              default-database
                                              Source_-table
                                              ;; (sql->ast FDW_Source_))))
                                              FDW_Source_)))
                                              
    (let ((tables-description FDW_tables-description)
          (table->fields FDW_table->fields)
          (cardinality-one? FDW_cardinality-one?)
          (choice FDW_choice))
      
      ;; Replace all virtual tables

      (display "Parsing SQL.") (newline)
      (let ((ast (SQL->AST sql)))
        (display "Transforming AST.") (newline)
        (let* (;; Functions are replaced and computed if needed
               (ast (replace-functions ast functions-hash))
               
               ;; We expand OR into UNION in order to have only AND (conjunctive queries)
               (ast (expand-or ast))

               ;; We replace Virtual tables by their definition
               
               (ast (replace-table ast default-database Object-table Object-virtual-table))
               (ast (replace-table ast default-database Source-table Source-virtual-table))

               ;; Join associativity
               (ast (join-associativity ast)) ;; mais BUG !!

               ;; (ast (join-associativity_new ast)) ne marche pas du tout ...
               
               ;; Replace all partition tables (Strategy 1)
               ;; (ast (replace-all-partition-tables ast FDW_modulo-partitions))
               
               ;; Field extraction and dispatch
               ;; Notice that we now have only conjunctive predicates (series of AND)
               ;; ex: Geometry extraction in order to call UDF
               (ast (distribute-predicate ast table->fields tables-description))

               ;; We need to disambiguate all fields
               ;; (ast (disambiguation ast tables-description))

               ;; count(*) transformation
               (ast (count-rewriter ast cardinality-one? choice))

               ;; Replace all partition tables (Strategy 2)
               (ast (replace-all-partition-tables ast FDW_modulo-partitions))
               
               ;; TODO: eliminate useless JOIN (ie Geometry without constraints / cardinality)

               )
          
          ;; (debug
          ;; (begin (display (ast->sql ast)) (newline) 0)
          ast)))))

;; > (rewriter "SELECT * FROM (SELECT * FROM Object Where gFlux_PS > 0.) AS o WHERE o.ra_PS > 12. ;")
;; "SELECT * FROM (SELECT * FROM (SELECT * FROM master_object_000 WHERE objectId  % 4 = 0 UNION ALL SELECT * FROM master_object_001 WHERE objectId  % 4 = 1 UNION ALL SELECT * FROM master_object_002 WHERE objectId  % 4 = 2 UNION ALL SELECT * FROM master_object_003 WHERE objectId  % 4 = 3) AS Object WHERE Object.ra_PS > 12.  AND Object.gFlux_PS > 0.) AS o ;"

;; > (foo "SELECT * FROM Object JOIN Source USING (objectid) WHERE ra_PS = 123. ;")  
;; "SELECT * FROM (SELECT * FROM Object WHERE Object.ra_PS = 123.) AS Object INNER JOIN Source USING ( objectid ) ;"

;; > (foo SHV1_250NodeTestPlan)
;; "SELECT count(*) FROM (SELECT * FROM Object AS o1 WHERE o1.decl_PS BETWEEN -5. AND 5.  AND o1.ra_PS BETWEEN 0. AND 5.) AS o1, Object AS o2 WHERE qserv_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < .1   ;"

;; > (AST->SQL (distribute-predicate (SQL->AST LSST_query_bonus_001) LSST_table->fields LSST_tables-description))
;; "SELECT COUNT(*) FROM (SELECT * FROM LSST.Object AS o1 WHERE qserv_areaspec_box(-11, -11, -1, -1)) AS o1, (SELECT * FROM LSST.Object AS o2 WHERE qserv_areaspec_box(-11, -11, -1, -1)) AS o2 WHERE scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < .1   ;"
