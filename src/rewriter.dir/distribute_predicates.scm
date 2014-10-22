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
(include "../sql.dir/schema-macros.scm")

(define distribute_predicates::debug #f)

;; (define (rename-predicate_debug ast name ast2 table-description)
;;   (let ((result (rename-predicate_orig ast name ast2 table-description)))
;;     (display (list 'rename-predicate (ast->sql ast))) (newline)
;;     (display (list 'rename-predicate name)) (newline)
;;     (display (list 'rename-predicate (ast->sql ast2))) (newline)
;;     (display (list 'result (ast->sql result))) (newline)
;;     (newline)
;;     result))

  
;; ---------------------------------------- ;;

;; (rename-predicate ast (in fact it is a predicate) name ast2 description)
(define-ast-walker rename-predicate
  ()
  (dispatch (field? field-dispatch))

  ;; Dispatchers
  
  (define (field-dispatch obj)
    (let ((table  (field->table obj))
          (column (field->column obj)))
      (field 'unknown column)))

  ;; End of rename-predicate
  )
  

;; WHERE must be a series of AND predicates
(define (where->and-list where)

  (define (partial-predicate->list partial-pred)
    (let ((log-op (partial-predicate->logic-operation partial-pred))
          (pred   (partial-predicate->predicate partial-pred)))
      (if (string-ci=? log-op "AND") pred
          (error "where->and-list: WHERE contains logical operation different from AND" (pp where)))))
  
  (cond ((eq? where 'empty-where) '())
        ((predicate? where)
         (let ((first-expr  (predicate->first-expr where))
               (rest-expr   (predicate->rest-expr where)))
           (cons first-expr (map partial-predicate->list rest-expr))))
        ((or (comparison? where)
             (between-predicate? where)
             (negation-predicate? where)
             (set-predicate? where)
             (like-operator? where)
             (is-null-record? where)
             (any-record? where)
             (all-record? where)
             (some-record? where)
             (exists-record? where)
             (parenthesised-expression? where)
             (function-call? where))
         (list where))
        (else (error "where->and-list: Unknown type" where))))


;; ;; Returns a boolean  indicating whether the predicate could
;; ;; be distributed to the table.

;; (define (distributable? pred table)
;;   (let ((used-fields (collect-fields pred table->fields))
;;         (set-fields-list (list->set (table->fields table))))
;;     (list-subset? used-fields set-of-fields-from-table)))
    
(define-ast-walker distribute-predicate
  (table->fields description) ;; arguments
  (dispatch (sql-record?  sql-record-dispatch))

  ;; Utils

  ;; Idea: Try to distribute each pred inside pred-list
  ;; if it fails keep pred for non-distributable list
  ;; else update with a new FROM and continue on next pred

  (define (distribute-predicates-into-tables from pred-list)

    (define (display-list mylist)
      (for-each
       (lambda (p)
         (display (ast->sql p))
         (newline))
       mylist))
    
    (let predicate-loop ((pred-list pred-list)
                         (from from)
                         (non-distributable '()))
      
      (and distribute_predicates::debug 
           (display 'predicate-loop) (newline)
           (display 'pred-list) (newline)
           (display-list pred-list) (newline)
           (display 'from) (newline)
           ;; (display (ast->sql from)) (newline)
           (display from) (newline)
           (display 'non-distributable) (newline)
           (display-list non-distributable) (newline)
           (newline))
      
      (if (null? pred-list)
          (vector
           
           ;; New FROM
           ;; We must be able to reconstruct even if it is complex
           ;; Idea: create an abstract view of the composition of tables
           ;; and replace each abstract table name with corresponding
           ;; table definition taking into account the distribution.
           
           from
         
           ;; New WHERE
           (if (null? non-distributable) 'empty-where
               (apply and-predicate (reverse non-distributable))))
        
          (let ((pred (car pred-list))
                (others (cdr pred-list)))

            (let* ((tmp (distribute-predicate-in-from-clause
                         from pred table->fields description))
                   (new-from (vector-ref tmp 0))
                   (success? (vector-ref tmp 1)))

              (if success?
                  (predicate-loop others new-from non-distributable)
                  (predicate-loop others from (cons pred non-distributable))
                  ))))))

  ;; Dispatchers
  
  (define (sql-record-dispatch obj)

    (let ((select    (dispatch (sql-record->select obj)))
          (from      (dispatch (sql-record->from   obj)))
          (where     (dispatch (sql-record->where  obj)))
          (group-by  (sql-record->group-by  obj))
          (order-by  (sql-record->order-by  obj))
          (limit     (sql-record->limit  obj)))

      (let ((pred-list (where->and-list where)))

        ;; Now we scan the FROM clause for predicate distribution
        ;; For instance "A UNION B" we should distribute on both A and B
        ;; whereas "A JOIN B" we should select either A B or nothing        
        
        (let* ((distribution (distribute-predicates-into-tables from pred-list))

               ;;  We keep only non distributable predicates
               ;;  in  the new-where but  the computation of
               ;;   distributability  has  to be  done  only
               ;;  once. So this function should return many
               ;;  values (here in a vector)
            
               (new-from  (vector-ref distribution 0))
               (new-where (vector-ref distribution 1)))

          (sql-record select
                      new-from new-where
                      group-by order-by limit)))))

  ;; End of distribute-predicate
  )

;; ---------------------------------------- ;;

;; Returns a vector with a new FROM and a success? boolean
(define (distribute-predicate-in-from-clause
         from pred table->fields description)

  ;; recursive scan on FROM with peculiarities !

  ;;  'from' represents a  table, so  it must  have fields
  ;; each part of it must also have fields then it depends
  ;; on how we combine parts

  (define (get-fields t)
    (list t (collect-fields t table->fields)))
      
  ;; -------- ;;
    
  ;; Nothing to do when there is only one table.
  (define (table-dispatch) (vector from #f))

  ;; We have to rename the predicate fields if it uses the
  ;; alias name when we distribute it into the subquery.
  (define (sql-subquery-dispatch)
    ;; (display (list 'sql-subquery-dispatch (AST->SQL from))) (newline)
    (let* ((name  (alias->name from))
           (content (alias->value from))
           (query (parenthesised-expression->expr content))
           (used-fields  (collect-fields pred table->fields))
           (table-fields (collect-fields from table->fields)))
      (cond ((sql-record? query)
             (if (collect-fields-subset? used-fields table-fields)
                 (let* ((new-pred  (rename-predicate pred))
                        (new-query (insert-predicate-in-sql-record
                                    query new-pred
                                    table->fields
                                    description
                                    )))
                   ;; (pp (list 'sql-subquery-dispatch pred new-pred)) (newline)
                   (vector (make-subquery new-query name) #t))
                 (vector from #f)))
            (else (error "sql-subquery-dispatch: unknown type" query)))))
            

  (define (combined-subquery-dispatch)
    (let* ((name  (alias->name from))
           (content (alias->value from))
           (query (parenthesised-expression->expr content)))
      ;; Here query is a table-combinator
      ;; Remark that we cannot combine tables without identical schemas
      ;; Therefore
      (let ((table  (table-combinator->table query))
            (others (table-combinator->list-of-table-combinators query)))
        (let ((used-fields  (collect-fields pred table->fields))
              (table-fields (collect-fields from table->fields)))
          (if (collect-fields-subset? used-fields table-fields)
              (vector
               (make-subquery 
                (table-combinator
                 (insert-predicate-in-sql-record
                  table
                  (rename-predicate pred)
                  table->fields description)
                   
                 (map (lambda (x)
                        (let* ((combinator (table-combinator-partial->combinator x))
                               (table      (table-combinator-partial->table x))
                               (new-pred   (rename-predicate pred)))
                          (cond ((sql-record? table)
                                 (table-combinator-partial
                                  combinator 
                                  (insert-predicate-in-sql-record
                                   table new-pred
                                   table->fields
                                   description)))
                                (else (error "combined-subquery-dispatch: unknown type" table)))))
                      others))
                name)
               #t)
              (vector from #f))))))

  (define (distributed-query-dispatch val new-pred)
    '(begin (display (list 'distributed-query-dispatch val new-pred)) (newline))
    ;; returns a vector with new val and a boolean (= if successfull)
    (let* ((query  (distributed-query->query val))
           (key    (distributed-query->key val))
           (number (distributed-query->number val))
           (used-fields  (collect-fields pred  table->fields))
           (table-fields (collect-fields query table->fields))
           (table-fields (collect-fields-insert key table-fields)))
      '(begin
         (display (list 'used-fields used-fields)) (newline)
         (display (list 'table-fields table-fields)) (newline))
      (cond ((sql-record? query)
             (if (collect-fields-subset? used-fields table-fields)
                 (let* ((new-query (insert-predicate-in-sql-record
                                    query new-pred
                                    table->fields
                                    description))
                        (new-distributed-query (distributed-query new-query key number))
                        (new-from new-distributed-query))
                   (vector new-from #t))
                 (vector from #f)))
            ;; ((table-record? query)
            (else (error "distributed-query-dispatch: unknown type" query)))))

  (define (alias-dispatch)
    '(begin (display (list "alias-dispatch: " from pred)) (newline))
    (let ((name (alias->name from))
          (val  (alias->value from)))
      (cond ((table-record? val) (vector from #f))
            ((parenthesised-expression? val)
             (let ((expr (parenthesised-expression->expr val)))
               (cond
                ((distributed-query? expr)
                 (let ((query  (distributed-query->query expr))
                       (key    (distributed-query->key expr))
                       (number (distributed-query->number expr))
                       (used-fields  (collect-fields pred table->fields))
                       (table-fields (collect-fields from table->fields)))
                   (if (collect-fields-subset? used-fields table-fields)
                       (let* ((new-pred  (rename-predicate pred))
                              (new-expr (insert-predicate-in-sql-record
                                         query new-pred
                                         table->fields
                                         description))
                              (new-expr (distributed-query new-expr key number))
                              (new-expr (parenthesised-expression new-expr))
                              (new-expr (make-alias name new-expr)))
                         (vector new-expr #t))
                       (vector from #f))))
                ((sql-record? expr)
                 (let ((used-fields  (collect-fields pred table->fields))
                       (table-fields (collect-fields from table->fields)))
                   (if (collect-fields-subset? used-fields table-fields)
                       (let* ((new-pred (rename-predicate pred))
                              (new-expr (insert-predicate-in-sql-record
                                         expr new-pred
                                         table->fields
                                         description))
                              (new-expr (parenthesised-expression new-expr))
                              (new-expr (make-alias name new-expr)))
                         (vector new-expr #t))
                       (vector from #f))))
                (else (error "alias-dispatch: Unknown type" expr)))))
            (else (error "distribute-predicates-into-tables/alias: Unknown type" val)))))
    
  (define (alias-dispatch.old)
    '(begin (display (list "alias-dispatch: " from pred)) (newline))
    (let ((name (alias->name from))
          (val  (alias->value from)))
      (cond ((table-record? val) (vector from #f))
            ((parenthesised-expression? val)
             (let* ((expr (parenthesised-expression->expr val))
                    (new-pred (rename-predicate pred)))
               ;; ( <expr> ) AS <name> .. <pred>
               (let* ((tmp (if (distributed-query? expr)
                               (distributed-query-dispatch expr new-pred)
                               (distribute-predicate-in-from-clause expr new-pred)))
                      (new-from (vector-ref tmp 0))
                      (success? (vector-ref tmp 1))

                      (new-from
                       (if (alias? new-from)
                           (let ((new-name (alias->name new-from)))
                             (if (string-ci=? name new-name)
                                 new-from
                                 (error "alias-dispatch: wrong renaming: " new-from)))
                           (let ((new-from (parenthesised-expression new-from)))
                             (make-alias name new-from)))))
                 (vector new-from success?))))
            (else (error "distribute-predicates-into-tables/alias: Unknown type" val)))))

  (define (cartesian-dispatch)

    '(begin (pp (list 'cartesian-dispatch from pred)) (newline))
      
    (let ((table-list (cartesian-product->tables from))
          (used-fields (collect-fields pred table->fields)))
      (let ((alist (map get-fields table-list))
            (bool #f))
        (let* ((fun (lambda (p)
                      (let ((table (car p))
                            (table-fields (cadr p)))
                        (if (collect-fields-subset? used-fields table-fields)
                            (let ((new-pred (rename-predicate pred)))
                              (set! bool #t)
                              '(begin
                                 (display (list 'cartesian-dispatch "found")) (newline)
                                 (display (list 'insert-predicate table new-pred)) (newline))
                              (insert-predicate
                               table new-pred
                               table->fields
                               description))
                            table))))
               (new-alist (map fun alist))
               (result (cartesian-product new-alist)))

          '(begin (pp (list 'cartesian-dispatch " => " result bool)) (newline))
            
          (vector result bool )))))
    
  (define (join-using-dispatch) ;; from pred
      
    (let ((type         (join-table-using->type from))
          (left-table   (join-table-using->left-table from))
          (right-table  (join-table-using->right-table from))
          (join-fields  (join-table-using->join-fields from))
          (used-fields  (collect-fields pred table->fields)))
      (let* ((left-table-fields  (collect-fields left-table  table->fields))
             (right-table-fields (collect-fields right-table table->fields))

             (left-boolean  #f)
             (right-boolean #f)
               
             (new-left-table
              (if (collect-fields-subset? used-fields left-table-fields)
                  (begin ;; let ((new-pred (rename-predicate pred 
                    (set! left-boolean #t)
                    (insert-predicate
                     left-table pred
                     table->fields
                     description))
                  left-table))
               
             (new-right-table
              (if (collect-fields-subset? used-fields right-table-fields)
                  (begin
                    (set! right-boolean #t)
                    (insert-predicate
                     right-table pred
                     table->fields
                     description))
                  right-table)))
          
        (if (or left-boolean right-boolean)
            (vector (join-table-using type new-left-table new-right-table join-fields) #t)
            (vector (join-table-using type left-table right-table join-fields) #f)))))

  (define (join-on-dispatch)

    (let ((type        (join-table-on->type from))
          (left-table  (join-table-on->left-table from))
          (right-table (join-table-on->right-table from))
          (predicate   (join-table-on->predicate from)))
      (let* ((used-fields  (collect-fields pred table->fields))
             (left-table-fields  (collect-fields left-table  table->fields))
             (right-table-fields (collect-fields right-table table->fields))

             (left-boolean  #f)
             (right-boolean #f)
               
             (new-left-table
              (if (collect-fields-subset? used-fields left-table-fields)
                  (begin ;; let ((new-pred (rename-predicate pred 
                    (set! left-boolean #t)
                    (insert-predicate
                     left-table pred
                     table->fields
                     description))
                  left-table))
               
             (new-right-table
              (if (collect-fields-subset? used-fields right-table-fields)
                  (begin
                    (set! right-boolean #t)
                    (insert-predicate
                     right-table pred
                     table->fields
                     description))
                  right-table)))

        (if (or left-boolean right-boolean)
            (vector (join-table-on type new-left-table new-right-table predicate) #t)
            (vector (join-table-on type left-table right-table predicate) #f)))))
        

  (define (parenthesised-expression-dispatch)
    (let* ((expr (parenthesised-expression->expr from))
           (tmp (distribute-predicate-in-from-clause expr pred))
           (new-from (vector-ref tmp 0))
           (success? (vector-ref tmp 1)))
      (vector (parenthesised-expression new-from) success?)))             

  ;; It really depends on how the FROM is composed
  (cond ((table-record? from) (table-dispatch))
        ((sql-subquery? from)  (sql-subquery-dispatch))
        ((combined-subquery? from)  (combined-subquery-dispatch))
        ((alias? from) (alias-dispatch))
        ((cartesian-product? from) (cartesian-dispatch))
        ((join-table-using? from)  (join-using-dispatch))
        ((join-table-on? from) (join-on-dispatch))
        ((parenthesised-expression? from) (parenthesised-expression-dispatch))
        ((distributed-query? from) (distributed-query-dispatch from pred))
        (else (error "distribute-predicates-in-from-clause: Unknown type" from))))

(define (wrap-table-with-predicate table pred)
  (let ((name (cond ((table-record? table) (table-record->name table))
                    ((alias? table) (alias->name table))
                    ;; TODO: we should avoid duplicate table names !
                    (else "__"))))
    (make-subquery
     (sql-record
      (projection #f (list (field 'unknown "*")))
      (parenthesised-expression table)
      pred
      'empty-group
      'empty-order-by
      'empty-limit)
     name)))

;; ---------------------------------------- ;;


;; First pass on FROM clause to extract tables with their contents
;; Second pass on WHERE clause to partition it according to table contents
;; Third pass : reconstruct a FROM and a WHERE clause
               

;; for instance pred uses x and a and
;; tables are (a b x) (c d e) (f y)
;; Then pred is distributable on the first table

;; for instance pred uses x and y and
;; tables are (a b x) (c d e) (f y)
;; Then pred is not distributable

;; for instance pred uses u and
;; tables are (a b x) (c d e) (f y)
;; Then u is from another context (because of subqueries)

;; Warning: a  predicate distributed  on an UNION  should be
;; distributed on all union members.

;; therefore the  idea is to  first collect the  and-list of
;; all  predicates   and  then  scan  all   FROM  tables  to
;; distribute.

;; TODO: But this means that I have to create a special type
;; for Union tables along many servers.



;; A field meaning depends on the surrounding context (subquery)

;; We must be able to extract fields used from a predicate expression

;; Tables  in FROM  have to  be rewritten  according  to the
;; predicates  used  in  WHERE  => not  a  direct  recursive
;; transformation

;; Therefore we need  to work in the "SELECT  .. " level: at
;; sql-record level
  
;; We should split a series of AND predicate expressions

;; A predicate using fields from many table cannot be distributed

;; SELECT .. FROM <composition of T1 T2 ...>
;; WHERE Pred1 AND Pred2 AND ...

;; =>

;; SELECT .. FROM <composition of (T1 with predicates) ...>
;; WHERE <non distributable predicates>

;; We should be  able to list all fields  from a table (even
;; for a composite table as JOIN or Cartesian product)

;; TODO: add the description parameter  
(define (distribute-predicate:test)
  (unit-test
   
   (string=?
    (ast->sql 
     (insert-predicate
      (make-alias "S1" (table-record 'unknown "master_object_001_xyz"))
      (comparison ">" (field 'unknown "gFlux_PS") 0.)
      FDW_table->fields
      FDW_tables-description))
    "(SELECT * FROM master_object_001_xyz WHERE gFlux_PS > 0.) AS S1 ;")

   (string=?
    (ast->sql 
     (insert-predicate
      (make-alias "S1" (table-record 'unknown "master_object_001_xyz"))
      (comparison ">" (field 'unknown "gFlux_PS") 0.)
      FDW_table->fields
      FDW_tables-description))
    "(SELECT * FROM master_object_001_xyz WHERE gFlux_PS > 0.) AS S1 ;")
   
   (let ((table->fields
          (alist->function
           (('unknown "T1") '("id" "a" "b"))
           (('unknown "T2") '("id" "c" "d" "e")))))
    
     (list
     
      (AST->SQL
       (distribute-predicate
        (SQL->AST "SELECT * FROM T1,T2 WHERE a = 123 AND b > c ;")
        table->fields ))

      (AST->SQL
       (distribute-predicate
        (SQL->AST "SELECT * FROM T1, T2 WHERE a = 123 AND b > a ;")
        table->fields ))
   
      (AST->SQL
       (distribute-predicate
        (SQL->AST "SELECT * FROM T1 JOIN T2 USING (id) WHERE a = 123 AND b > a ;")
        table->fields ))

      (AST->SQL
       (distribute-predicate
        (SQL->AST "SELECT Source.objectId FROM Source INNER JOIN Object USING ( objectId ) WHERE Object.ra_PS BETWEEN 3 AND 4  AND Object.decl_PS BETWEEN -3 AND -2 ;")
        LSST_table->fields
        LSST_tables-description))
     
      ))))



;; > (foo "SELECT * FROM (SELECT * FROM Object WHERE ra_PS = 1.) AS O WHERE O.decl_PS > 4.;")
;; "SELECT * FROM (SELECT * FROM Object WHERE Object.decl_PS > 4.  AND Object.ra_PS = 1.) AS O ;"

;; > (foo "SELECT * FROM (SELECT * FROM Object WHERE ra_PS = 1. UNION SELECT * FROM Object WHERE ra_PS = 2.) as O WHERE decl_PS = 3.;")
;; "SELECT * FROM (SELECT * FROM Object WHERE Object.decl_PS = 3.  AND Object.ra_PS = 1. UNION SELECT * FROM Object WHERE Object.decl_PS = 3.  AND Object.ra_PS = 2.) AS O ;"

;; > (AST->SQL (distribute-predicate (SQL->AST "SELECT COUNT(*) FROM LSST.Object o1, LSST.Object o2 WHERE foo(o1.ra_PS,o1.decl_PS) > 123 ;") LSST_table->fields LSST_tables-description))
;; "SELECT COUNT(*) FROM (SELECT * FROM LSST.Object AS o1 WHERE foo(o1.ra_PS, o1.decl_PS) > 123) AS o1, LSST.Object AS o2 ;"
;; > (AST->SQL (distribute-predicate (SQL->AST "SELECT COUNT(*) FROM LSST.Object o1, LSST.Object o2 WHERE foo(o1.ra_PS,o2.decl_PS) > 123 ;") LSST_table->fields LSST_tables-description))
;; "SELECT COUNT(*) FROM LSST.Object AS o1, LSST.Object AS o2 WHERE foo(o1.ra_PS, o2.decl_PS) > 123   ;"

;; > (AST->SQL (distribute-predicate (SQL->AST  LSST_query_bonus_011 ) LSST_table->fields LSST_tables-description))
;; "SELECT o1.objectId AS objId1, o2.objectId AS objId2, scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) AS distance FROM (SELECT * FROM Object AS o1 WHERE o1.decl_PS BETWEEN .01 AND .03  AND o1.ra_PS BETWEEN 0 AND .02) AS o1, (SELECT * FROM Object AS o2 WHERE o2.decl_PS BETWEEN .01 AND .03  AND o2.ra_PS BETWEEN 0 AND .02) AS o2 WHERE o1.objectId <> o2.objectId   ;"

;; > (foo LSST_query_001_a)
;; "SELECT Source.taiMidPoint, Source.psfFlux, Source.psfFluxSigma FROM (SELECT * FROM Source WHERE Source.objectId = 430209694171177) AS Source INNER JOIN (SELECT * FROM Filter WHERE Filter.filterName = 'i') AS Filter USING ( filterId ) ;"


