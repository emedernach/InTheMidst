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
(include "../ast.dir/arithmetic.scm")
(include "../ast.dir/token.scm")
(include "../ast.dir/sql.scm")
(include "../ast.dir/modifier.scm")
(include "../ast.dir/predicate.scm")
(include "../ast.dir/projection.scm")
(include "../ast.dir/function.scm")
(include "../ast.dir/logic.scm")

;;      (export AST->SQL sql_rewriter:test))

;; returns a SQL string from an AST
(define (ast->sql ast)

  (define (dispatch obj)
    (cond ((string-token? obj) (string-token->string obj))
          ((sql-record? obj) (sql-record->SQL obj))
          ((table-combinator? obj) (table-combinator->SQL obj))
          ((table-combinator-partial? obj) (table-combinator-partial->SQL obj))
          ((projection? obj) (projection->SQL obj))
          ((field? obj)  (field->SQL obj))
          ((cast-expression? obj) (cast->SQL obj))
          ((table-record? obj)  (table->SQL obj))
          ((alias? obj)  (alias->SQL obj))
          ((parenthesised-expression? obj)  (parenthesised-expression->SQL obj))
          ((cartesian-product? obj) (cartesian-product->SQL obj))
          ((join-table-using? obj) (join-table-using->SQL obj))
          ((join-table-on? obj) (join-table-on->SQL obj))
          ((predicate? obj) (predicate->SQL obj))
          ((partial-predicate? obj) (partial-predicate->SQL obj))
          ((negation-predicate? obj) (negation-predicate->SQL obj))
          ((set-predicate? obj) (set-predicate->SQL obj))
          ((comparison? obj) (comparison->SQL obj))
          ((function-call? obj) (function-call->SQL obj))
          ((case-expr? obj) (case-expr->SQL obj))
          ((case-value? obj) (case-value->SQL obj))
          ((set? obj) (set->SQL obj))
          ((like-operator? obj) (like-operator->SQL obj))
          ((is-null-record? obj) (is-null->SQL obj))
          ((any-record? obj) (any-record->SQL obj))
          ((all-record? obj) (all-record->SQL obj))
          ((some-record? obj) (some-record->SQL obj))
          ((exists-record? obj) (exists-record->SQL obj))
          ((between-predicate? obj) (between-predicate->SQL obj))
          ((arithmetic? obj) (arithmetic->SQL obj))
          ((partial-arithmetic? obj) (partial-arithmetic->SQL obj))          
          ((number? obj) (number->string obj))
          ((unary-arithmetic? obj) (unary-arithmetic->SQL obj))
          
          ;; Virtual tables (but should not be there anymore at this stage
          ((modulo-partition? obj) (dispatch (modulo-partition->ast obj)))
          ((distributed-query? obj) (distributed-query-dispatch obj))

          ((placeholder? obj) (dispatch-placeholder obj))
          
          (else (error "AST->SQL: Unknown type" obj))))

  (define (dispatch-placeholder obj)
    (let ((id (placeholder->id obj)))
      (list "[" (number->string id) "]")))
  
  (define (table-combinator->SQL obj)
    (let ((table (table-combinator->table obj))
          (list-of-table-combinators (table-combinator->list-of-table-combinators obj)))
      (list (dispatch table)
            (map dispatch
                 list-of-table-combinators))))

  (define (table-combinator-partial->SQL obj)
    (let ((combinator (table-combinator-partial->combinator obj))
          (table      (table-combinator-partial->table obj)))
      (list " " combinator " " (dispatch table))))
  
  (define (sql-record->SQL obj)
    (let ((select    (sql-record->select  obj))
          (from      (sql-record->from  obj))
          (where     (sql-record->where  obj))
          (group-by  (sql-record->group-by  obj))
          (order-by  (sql-record->order-by  obj))
          (limit     (sql-record->limit  obj)))
      (let ((select-str (projection->SQL select))
            (from-str (from->SQL from))
            (where-str (where->SQL where))
            (group-by-str (group-by->SQL group-by))
            (order-by-str (order-by->SQL order-by))
            (limit-str (limit->SQL limit)))
        (let* ((sql-list (list select-str from-str
                               where-str group-by-str
                               order-by-str limit-str))
               (sql-list (keep (lambda (s)
                                 (not (and (string? s)
                                           (string=? s ""))))
                               sql-list)))
          (list-interpose " " sql-list)))))

  (define (projection->SQL obj)
    (let ((distinct? (projection->distinct? obj))
          (fields    (projection->fields obj)))
      (list
       (if distinct? "SELECT DISTINCT " "SELECT ")
       (list-interpose ", " (map dispatch fields)))))          

  (define (from->SQL obj)
    (if (eq? obj 'empty-from) ""
        (list "FROM " (dispatch obj))))

  (define (where->SQL obj)
    (if (eq? obj 'empty-where) ""
        (list "WHERE " (dispatch obj))))

  (define (group-by->SQL obj)
    (if (eq? obj 'empty-group) ""
        (list
         "GROUP BY "
         (let ((fields  (group-by->fields obj))
               (having  (group-by->having obj)))
           (list-interpose
            " " (list
                 (list-interpose ", " (map dispatch fields))
                 (if (eq? having #t) ""
                     (list-interpose
                      " " (list "HAVING" (dispatch having))))))))))

  (define (order-by->SQL obj)
    (if (eq? obj 'empty-order-by) ""
        (list
         "ORDER BY "
         (let ((direction (order-by->direction obj))
               (fields    (order-by->fields obj)))
           (list
            (list-interpose ", " (map dispatch fields))
            " " direction )))))
  
  (define (parenthesised-expression->SQL obj)
    (let ((expr  (parenthesised-expression->expr obj)))
      (list "(" (dispatch expr) ")")))

  (define (limit->SQL obj)
    (if (eq? obj 'empty-limit) ""
        (list
         "LIMIT "
         (let ((number (sql-limit->number obj)))
           (number->string number)))))

  (define (cast->SQL obj)
    (let ((expr (cast-expression->expr obj))
          (type (cast-expression->type obj)))
      (list "CAST" "( " (dispatch expr) " AS " type " )")))
  
  (define (field->SQL obj)
    (let ((table  (field->table obj))
          (column (field->column obj)))
      (if (eq? table 'Unknown)
          column
          (list table "." column))))

  (define (table->SQL obj)
    (let ((database (table-record->database obj))
          (name     (table-record->name obj)))
      (if (eq? database 'Unknown)
          name
          (list database "." name))))

  (define (alias->SQL obj)
    (let ((name (alias->name obj))
          (val  (alias->value obj)))
      (list-interpose
       " "
       (list (dispatch val) "AS" name))))

  (define (cartesian-product->SQL obj)
    (let ((table-list (cartesian-product->tables obj)))
      (list-interpose ", " (map dispatch table-list))))
  
  (define (join-table-using->SQL_old obj)
    (let ((type         (join-table-using->type obj))
          (left-table   (join-table-using->left-table obj))
          (right-table  (join-table-using->right-table obj))
          (join-fields  (join-table-using->join-fields obj)))
      (list-interpose
       " "
       (list
        (dispatch left-table)
        (case type
          ((INNER inner) "INNER")
          ((LEFT left)  "LEFT")
          ((RIGHT right) "RIGHT")
          ((FULL full)  "FULL")
          (else (error "join-table-using->SQL: Unknown join type" type)))
        "JOIN" (dispatch right-table) 
        "USING"
        "(" (list-interpose "," (map dispatch join-fields)) ")"))))

  (define (join-table-using->SQL obj)
    (let ((type         (join-table-using->type obj))
          (left-table   (join-table-using->left-table obj))
          (right-table  (join-table-using->right-table obj))
          (join-fields  (join-table-using->join-fields obj)))
      (list-interpose
       " "
       (list
        (dispatch left-table)
        (case type
          ((INNER inner) "INNER")
          ((LEFT left)  "LEFT")
          ((RIGHT right) "RIGHT")
          ((FULL full)  "FULL")
          (else (error "join-table-using->SQL: Unknown join type" type)))
        "JOIN"
        (if (or (table-record? right-table)
                (alias? right-table))
            (let ((right-table (dispatch right-table)))
              right-table)
            (let ((right-table (dispatch right-table)))
              (list-interpose " " (list "(" right-table ")" "AS" "__"))))
        "USING"
        "(" (list-interpose "," (map dispatch join-fields)) ")"))))

  (define (join-table-on->SQL obj)
    (let ((type        (join-table-on->type obj))
          (left-table  (join-table-on->left-table obj))
          (right-table (join-table-on->right-table obj))
          (predicate   (join-table-on->predicate obj)))
      (list-interpose
       " "
       (list
        (dispatch left-table)
        (case type
          ((INNER inner) "INNER")
          ((LEFT left)  "LEFT")
          ((RIGHT right) "RIGHT")
          ((FULL full)  "FULL")
          (else (error "join-table-using->SQL: Unknown join type" type)))
        "JOIN"
        (dispatch right-table)
        "ON" "(" (dispatch predicate) ")" ))))
  
  (define (predicate->SQL obj)
    (let ((first-expr (predicate->first-expr obj))
          (rest-expr  (predicate->rest-expr obj)))
      (list
       (dispatch first-expr)
       "  " (list-interpose
             " " (map dispatch rest-expr)))))

  (define (negation-predicate->SQL obj)
    (let ((predicate  (negation-predicate->predicate obj)))
      (list "NOT" " " (dispatch predicate))))

  (define (set-predicate->SQL obj)
    (let ((expr  (set-predicate->expr obj))
          (set-of-values  (set-predicate->set-of-values obj)))
      (list-interpose
       " " (list (dispatch expr) "IN"
                 ;; set-of-values = a set or a subquery
                 (dispatch set-of-values)))))

  (define (set->SQL obj)
    (let* ((obj-list-dispatched (map dispatch (set->list obj)))
           (str (apply string-append-with ", " obj-list-dispatched)))
      (list-interpose " " (list "(" str ")"))))
  
  (define (partial-predicate->SQL obj)
    (let ((logic-operation (partial-predicate->logic-operation obj))
          (predicate       (partial-predicate->predicate obj)))
      (list logic-operation " " (dispatch predicate))))
  
  (define (comparison->SQL obj)
    (let ((comparator (comparison->comparator obj))
          (left-expr  (comparison->left-expr obj))
          (right-expr (comparison->right-expr obj)))
      (list-interpose
       " "
       (list
        (dispatch left-expr)
        comparator
        (dispatch right-expr)))))

  (define (function-call->SQL obj)
    (let ((function   (function-call->function obj))
          (arguments  (function-call->arguments obj)))
      (list function "(" (list-interpose ", " (map dispatch arguments)) ")" )))

  (define (between-predicate->SQL obj)
    (let ((expr (between-predicate->expr obj))
          (left-bound  (between-predicate->left-bound obj))
          (right-bound (between-predicate->right-bound obj)))
      (list-interpose
       " "
       (list
        (dispatch expr)
        "BETWEEN"
        (dispatch left-bound)
        "AND"
        (dispatch right-bound)))))

  (define (like-operator->SQL obj)
    (let ((expr     (like-operator->expr obj))
          (not?     (like-operator->not? obj))
          (pattern  (like-operator->pattern obj)))
      (if not?
          (list-interpose " " (list (dispatch expr) "NOT" "LIKE" (dispatch pattern)))
          (list-interpose " " (list (dispatch expr) "LIKE" (dispatch pattern))))))

  (define (is-null->SQL obj)
    (let ((field  (is-null-record->field obj))
          (not?   (is-null-record->not? obj)))
      (if not?
          (list-interpose " " (list (dispatch field) "IS" "NOT" "NULL"))
          (list-interpose " " (list (dispatch field) "IS" "NULL")))))
  
  (define (arithmetic->SQL obj)
    (let ((first-expr  (arithmetic->first-expr obj))
          (rest-expr   (arithmetic->rest-expr obj)))
      (list
       (dispatch first-expr)
       "  " (list-interpose " " (map dispatch rest-expr)))))

  (define (partial-arithmetic->SQL obj)
    (let ((operation  (partial-arithmetic->operation obj))
          (expr       (partial-arithmetic->expr obj)))
      (list (symbol->string operation) " " (dispatch expr))))

  (define (unary-arithmetic->SQL obj)
    (let ((unary-op   (unary-arithmetic->unary-op obj))
          (expr       (unary-arithmetic->expr obj)))
      (list "-" " " (dispatch expr))))
  
  (define (case-expr->SQL obj)
    (let ((case-clauses-list  (case-expr->case-clauses-list obj))
          (else-clause        (case-expr->else-clause obj)))
      (list-interpose
       " "
       (list
        "CASE"
        (list-interpose " " (map case-clauses->SQL case-clauses-list))
        (if (eq? else-clause 'no-else) "" (else-clause->SQL else-clause))))))

  (define (case-value->SQL obj)
    (let ((expr               (case-value->expr obj))
          (case-clauses-list  (case-value->case-clauses-list obj))
          (else-clause        (case-value->else-clause obj)))
      (list-interpose
       " "
       (list
        "CASE" (dispatch expr)
        (list-interpose " " (map case-clauses->SQL case-clauses-list))
        (if (eq? else-clause 'no-else) "" (else-clause->SQL else-clause))))))

  (define (case-clauses->SQL obj)
    (let ((predicate  (case-clause->predicate obj))
          (expr       (case-clause->expr obj)))
      ;; (list-interpose " " (list "WHEN" "(" (dispatch predicate) ")" "THEN" (dispatch expr)))))
      (list-interpose " " (list "WHEN" (dispatch predicate) "THEN" (dispatch expr)))))
  
  (define (else-clause->SQL obj)
    (let ((expr  (else-clause->expr obj)))
      (list-interpose " " (list "ELSE" (dispatch expr)))))

  (define (any-record->SQL obj)
    (let ((subquery (any-record->subquery obj)))
      (list-interpose " " (list "ANY" "(" (dispatch subquery) ")"))))
  
  (define (all-record->SQL obj)
    (let ((subquery (all-record->subquery obj)))
      (list-interpose " " (list "ALL" "(" (dispatch subquery) ")"))))
  
  (define (some-record->SQL obj)
    (let ((subquery (some-record->subquery obj)))
      (list-interpose " " (list "SOME" "(" (dispatch subquery) ")"))))
  
  (define (exists-record->SQL obj)
    (let ((subquery (exists-record->subquery obj)))
      (list-interpose " " (list "EXISTS" "(" (dispatch subquery) ")"))))

  (define (distributed-query-dispatch obj)
    (let ((query  (distributed-query->query obj))
          (key    (distributed-query->key obj))
          (number (distributed-query->number obj)))
      (list-interpose
       " " (list "DISTRIBUTED(" (dispatch query) ","
                 key "," (number->string number) ")"))))

  
  ;; AST->SQL body
  (let* ((result (list (dispatch ast) " ;"))
         (result (flatten result))
         (len (length result)))
    ;; beware that result could be a long list !
    (if (< len 4000)
        (apply string-append result)
        (string-append-list result)
        )))

;; (define my_ast (SQL->AST LSST_query_001_b))
;; (AST->SQL my_ast)
(define (AST->SQL:test)
  (unit-test
   (string=? (AST->SQL (SQL->AST "SELECT * FROM T;"))
             "SELECT * FROM T ;")
   (string=? (AST->SQL (SQL->AST "SELECT * FROM T1, T2;"))
             "SELECT * FROM T1, T2 ;")
   (string=? (AST->SQL (SQL->AST "SELECT a FROM T;"))
             "SELECT a FROM T ;")
   (string=? (AST->SQL (SQL->AST "SELECT a,b,c FROM T;"))
             "SELECT a, b, c FROM T ;")
   (string=? (AST->SQL (SQL->AST "SELECT T1.a FROM T1,T2;"))
             "SELECT T1.a FROM T1, T2 ;")
   (string=? (AST->SQL (SQL->AST "SELECT T1.a,T2.b,c FROM T1,T2;"))
             "SELECT T1.a, T2.b, c FROM T1, T2 ;")
   (string=? (AST->SQL (SQL->AST "SELECT (a + b)/(c - d) ;"))
             "SELECT (a  + b)  / (c  - d) ;")
   (string=? (AST->SQL (SQL->AST "SELECT - a FROM T WHERE - b * c < 12.3;"))
             "SELECT - a FROM T WHERE - b  * c < 12.3 ;")
   (string=? (AST->SQL (SQL->AST "(SELECT * FROM T1) UNION ALL (SELECT * FROM T2 INTERSECT SELECT * FROM T3)"))
             "(SELECT * FROM T1) UNION ALL (SELECT * FROM T2 INTERSECT SELECT * FROM T3) ;")
   (string=? (AST->SQL (SQL->AST "SELECT * FROM T1 UNION ALL SELECT * FROM T2 UNION ALL SELECT * FROM T3 ;"))
             "SELECT * FROM T1 UNION ALL SELECT * FROM T2 UNION ALL SELECT * FROM T3 ;")
   (string=? (AST->SQL (SQL->AST "SELECT * FROM (SELECT * FROM T2 INTERSECT SELECT * FROM T3) AS DUMMY")) 
             "SELECT * FROM (SELECT * FROM T2 INTERSECT SELECT * FROM T3) AS DUMMY ;")
   (string=? (AST->SQL (SQL->AST "SELECT taiMidPoint, psfFlux, psfFluxSigma
FROM   Source
JOIN   Filter USING (filterId)
WHERE  objectId = :objectId
   AND filterName = :filterName"))
             "SELECT taiMidPoint, psfFlux, psfFluxSigma FROM Source INNER JOIN Filter USING ( filterId ) WHERE objectId = :objectId  AND filterName = :filterName ;")
   (string=? (AST->SQL (SQL->AST "SELECT *
FROM   Object
JOIN   _ObjectToType USING(objectId)
JOIN   ObjectType USING (typeId)
WHERE  description = 'Supernova'
  AND  variability > 0.8
  AND  probability > 0.8 ;"))
             (cond-expand
              (guile
               "SELECT * FROM Object INNER JOIN _ObjectToType USING ( objectId ) INNER JOIN ObjectType USING ( typeId ) WHERE description = 'Supernova'  AND variability > 0.8 AND probability > 0.8 ;")
              (gambit
               "SELECT * FROM Object INNER JOIN _ObjectToType USING ( objectId ) INNER JOIN ObjectType USING ( typeId ) WHERE description = 'Supernova'  AND variability > .8 AND probability > .8 ;")))
   (string=? (AST->SQL (SQL->AST "SELECT  *
FROM    Object
WHERE   areaSpec_box(:raMin, :declMin, :raMax, :declMax)
   AND  zMag      BETWEEN :zMin  AND :zMax
   AND  gMag-rMag BETWEEN :grMin AND :grMax
   AND  iMag-zMag BETWEEN :izMin AND :izMax"))
             "SELECT * FROM Object WHERE areaSpec_box(:raMin, :declMin, :raMax, :declMax)  AND zMag BETWEEN :zMin AND :zMax AND gMag  - rMag BETWEEN :grMin AND :grMax AND iMag  - zMag BETWEEN :izMin AND :izMax ;")
   (string=? (AST->SQL (SQL->AST "SELECT s.ra, s.decl, o.raRange, o.declRange
FROM   Object o
JOIN   Source s USING (objectId)
WHERE  o.objectId = :objectId
AND    o.latestObsTime = s.taiMidPoint;"))
             "SELECT s.ra, s.decl, o.raRange, o.declRange FROM Object AS o INNER JOIN Source AS s USING ( objectId ) WHERE o.objectId = :objectId  AND o.latestObsTime = s.taiMidPoint ;")
   (string=? (AST->SQL (SQL->AST "SELECT objectId, taiMidPoint, fluxToAbMag(psfMag)
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
WHERE  areaSpec_box(:raMin, :declMin, :raMax, :declMax)
  AND  filterName = 'u'
  AND  variability BETWEEN :varMin AND :varMax
ORDER BY objectId, taiMidPoint ASC"))
             "SELECT objectId, taiMidPoint, fluxToAbMag(psfMag) FROM Source INNER JOIN Object USING ( objectId ) INNER JOIN Filter USING ( filterId ) WHERE areaSpec_box(:raMin, :declMin, :raMax, :declMax)  AND filterName = 'u' AND variability BETWEEN :varMin AND :varMax ORDER BY objectId, taiMidPoint ASC ;")
   (string=? (AST->SQL (SQL->AST "SELECT objectId
FROM   Object
WHERE  areaSpec(:raMin, :declMin, :raMax, :declMax)
AND    variability > 0.8"))
             (cond-expand
              (guile
               "SELECT objectId FROM Object WHERE areaSpec(:raMin, :declMin, :raMax, :declMax)  AND variability > 0.8 ;")
              (gambit
               "SELECT objectId FROM Object WHERE areaSpec(:raMin, :declMin, :raMax, :declMax)  AND variability > .8 ;")))
   (string=? (AST->SQL (SQL->AST "SELECT  COUNT(*)                                               AS totalCount,
        SUM(CASE WHEN (typeId=3) THEN 1 ELSE 0 END)            AS galaxyCount,
        SUM(CASE WHEN (typeId=6) THEN 1 ELSE 0 END)            AS starCount,
        SUM(CASE WHEN (typeId NOT IN (3,6)) THEN 1 ELSE 0 END) AS otherCount
FROM    Object
JOIN    _Object2Type USING(objectId)
WHERE  (uMag-gMag > 2.0 OR uMag > 22.3) 
   AND iMag BETWEEN 0 AND 19 
   AND gMag - rMag > 1.0 
   AND ( (rMag-iMag < 0.08 + 0.42 * (gMag-rMag - 0.96)) OR (gMag-rMag > 2.26 ) )
   AND iMag-zMag < 0.25"))
             (cond-expand
              (guile
               "SELECT COUNT(*) AS totalCount, SUM(CASE WHEN ( typeId = 3 ) THEN 1 ELSE 0) AS galaxyCount, SUM(CASE WHEN ( typeId = 6 ) THEN 1 ELSE 0) AS starCount, SUM(CASE WHEN ( NOT typeId IN ( 6, 3 ) ) THEN 1 ELSE 0) AS otherCount FROM Object INNER JOIN _Object2Type USING ( objectId ) WHERE uMag  - gMag > 2.0  OR uMag > 22.3  AND iMag BETWEEN 0 AND 19 AND gMag  - rMag > 1.0 AND rMag  - iMag < 0.08  + 0.42 * (gMag  - rMag - 0.96)  OR gMag  - rMag > 2.26 AND iMag  - zMag < 0.25 ;")
              (gambit
               "SELECT COUNT(*) AS totalCount, SUM(CASE WHEN (typeId = 3) THEN 1 ELSE 0) AS galaxyCount, SUM(CASE WHEN (typeId = 6) THEN 1 ELSE 0) AS starCount, SUM(CASE WHEN (NOT typeId IN ( 3, 6 )) THEN 1 ELSE 0) AS otherCount FROM Object INNER JOIN _Object2Type USING ( objectId ) WHERE (uMag  - gMag > 2.  OR uMag > 22.3)  AND iMag BETWEEN 0 AND 19 AND gMag  - rMag > 1. AND ((rMag  - iMag < .08  + .42 * (gMag  - rMag - .96))  OR (gMag  - rMag > 2.26)) AND iMag  - zMag < .25 ;")))
   (string=? (AST->SQL (SQL->AST "SELECT objectId
FROM   Object
JOIN   DIASource USING(objectId)
WHERE  latestObsTime > :time
GROUP BY (objectId)
HAVING COUNT(objectId) = 1 ;"))
             "SELECT objectId FROM Object INNER JOIN DIASource USING ( objectId ) WHERE latestObsTime > :time GROUP BY objectId HAVING COUNT(objectId) = 1 ;")
   (string=? (AST->SQL (SQL->AST "SELECT  objectId,
	rowC,colC,rowV,colV,rowVErr,colVErr,
	flags,
	psfMag_u,psfMag_g,psfMag_r,psfMag_i,psfMag_z,
	psfMagErr_u,psfMagErr_g,psfMagErr_r,psfMagErr_i,psfMagErr_z
FROM   MovingObject
WHERE  rowvErr > 0 and colvErr> 0 
  AND ((rowV * rowV) / (rowVErr * rowVErr) + (colV * colV) / (colVErr * colVErr) > 4)
;"))
             "SELECT objectId, rowC, colC, rowV, colV, rowVErr, colVErr, flags, psfMag_u, psfMag_g, psfMag_r, psfMag_i, psfMag_z, psfMagErr_u, psfMagErr_g, psfMagErr_r, psfMagErr_i, psfMagErr_z FROM MovingObject WHERE rowvErr > 0  and colvErr > 0 AND ((rowV  * rowV)  / (rowVErr  * rowVErr) + (colV  * colV) / (colVErr  * colVErr) > 4) ;")
   (string=? (AST->SQL (SQL->AST "SELECT  G.objectId,
        G.uMag,G.gMag,G.rMag,G.iMag,G.zMag,G.yMag 
FROM    Galaxy G
JOIN    _Source2Object M1 ON (G.objectId = M1.objectId)
JOIN    _Source2Object M2 ON (M1.sourceId = M2.sourceId)
JOIN    Star S ON (M2.objectId = S.objectId); "))
             "SELECT G.objectId, G.uMag, G.gMag, G.rMag, G.iMag, G.zMag, G.yMag FROM Galaxy AS G INNER JOIN _Source2Object AS M1 ON ( G.objectId = M1.objectId ) INNER JOIN _Source2Object AS M2 ON ( M1.sourceId = M2.sourceId ) INNER JOIN Star AS S ON ( M2.objectId = S.objectId ) ;")
   (string=? (AST->SQL (SQL->AST "SELECT 
          o.ra, o.decl, o.flags, o.type, o.objid,
          o.psfMag_g, o.psfMag_r, o.psfMag_i, o.gMag, o.rMag, o.iMag, 
          o.petroRad_r, 
          o.q_g, o.q_r, o.q_i, 
          o.u_g, o.u_r, o.u_i, 
          o.mE1_r, o.mE2_r, o.mRrCc_r, o.mCr4_r, 
          o.isoA_r, o.isoB_r, o.isoAGrad_r, o.isoBGrad_r, o.isoPhi_r, 
          n.distance, p.r, p.g
FROM      Object as o 
LEFT JOIN Neighbors as n on o.objid=n.objid 
JOIN      Object as p ON (p.objId = n.neighborObjId)
WHERE     (o.ra > 120) and (o.ra < 240) 
    AND   (o.r > 16.) and (o.r<21.0) 
    AND   n.neighborObjId = (
               SELECT nn.neighborObjId
               FROM   Neighbors nn
               JOIN   Object pp ON (nn.neighborObjId = pp.objectId)
               WHERE  nn.objectId = o.objectId 
               ORDER BY pp.r
               LIMIT 1
                          )
LIMIT 100 ;"))
             (cond-expand
              (guile
               "SELECT o.ra, o.decl, o.flags, o.type, o.objid, o.psfMag_g, o.psfMag_r, o.psfMag_i, o.gMag, o.rMag, o.iMag, o.petroRad_r, o.q_g, o.q_r, o.q_i, o.u_g, o.u_r, o.u_i, o.mE1_r, o.mE2_r, o.mRrCc_r, o.mCr4_r, o.isoA_r, o.isoB_r, o.isoAGrad_r, o.isoBGrad_r, o.isoPhi_r, n.distance, p.r, p.g FROM Object AS o LEFT JOIN Neighbors AS n ON ( o.objid = n.objid ) INNER JOIN Object AS p ON ( p.objId = n.neighborObjId ) WHERE o.ra > 120  and o.ra < 240 AND o.r > 16.0 and o.r < 21.0 AND n.neighborObjId = (SELECT nn.neighborObjId FROM Neighbors AS nn INNER JOIN Object AS pp ON ( nn.neighborObjId = pp.objectId ) WHERE nn.objectId = o.objectId ORDER BY pp.r ASC LIMIT 1) LIMIT 100 ;")
              (gambit
               "SELECT o.ra, o.decl, o.flags, o.type, o.objid, o.psfMag_g, o.psfMag_r, o.psfMag_i, o.gMag, o.rMag, o.iMag, o.petroRad_r, o.q_g, o.q_r, o.q_i, o.u_g, o.u_r, o.u_i, o.mE1_r, o.mE2_r, o.mRrCc_r, o.mCr4_r, o.isoA_r, o.isoB_r, o.isoAGrad_r, o.isoBGrad_r, o.isoPhi_r, n.distance, p.r, p.g FROM Object AS o LEFT JOIN Neighbors AS n ON ( o.objid = n.objid ) INNER JOIN Object AS p ON ( p.objId = n.neighborObjId ) WHERE (o.ra > 120)  and (o.ra < 240) AND (o.r > 16.) and (o.r < 21.) AND n.neighborObjId = (SELECT nn.neighborObjId FROM Neighbors AS nn INNER JOIN Object AS pp ON ( nn.neighborObjId = pp.objectId ) WHERE nn.objectId = o.objectId ORDER BY pp.r ASC LIMIT 1) LIMIT 100 ;")))

   ))

(define (sql_rewriter:test)
  (AST->SQL:test))

;; (AST->SQL (SQL->AST LSST_query_029))

