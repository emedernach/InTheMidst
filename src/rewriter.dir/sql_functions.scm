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



;;      (export functions-hash spatial-join)

(define (angular-distance ra1 decl1 ra2 decl2)

  ;; Haversine formula

  ;; degrees(
  ;;  2*asin(sqrt(
  ;;   power(sin(radians((decl2 - decl1)/2)), 2) +
  ;;   power(sin(radians((ra2 - ra1)/2)), 2) *
  ;;    cos(radians(decl1)) * cos(radians(decl2)) 
  ;;  ))) ;

  (define (diff_mean a b)
    (arithmetic
     (parenthesised-expression
      (arithmetic a (list (partial-arithmetic '- b))))
     (list (partial-arithmetic '/ 2))))

  (let* ((delta-decl (diff_mean decl2 decl1))
         (delta-ra   (diff_mean ra2 ra1))

         (radians-delta-decl (function-call "radians" (list delta-decl)))
         (radians-delta-ra   (function-call "radians" (list delta-ra)))

         (sin-radians-delta-decl (function-call "sin" (list radians-delta-decl)))
         (sin-radians-delta-ra   (function-call "sin" (list radians-delta-ra)))

         (square-sin-radians-delta-decl (function-call "power" (list sin-radians-delta-decl 2)))
         (square-sin-radians-delta-ra   (function-call "power" (list sin-radians-delta-ra   2)))
         
         (radians-decl1     (function-call "radians" (list decl1)))
         (radians-decl2     (function-call "radians" (list decl2)))
         (cos-radians-decl1 (function-call "cos" (list radians-decl1)))
         (cos-radians-decl2 (function-call "cos" (list radians-decl2)))

         (product (arithmetic square-sin-radians-delta-ra
                              (list 
                               (partial-arithmetic '* cos-radians-decl1)
                               (partial-arithmetic '* cos-radians-decl2))))

         (sum (arithmetic square-sin-radians-delta-decl
                          (list
                           (partial-arithmetic '+ product))))

         (sqrt-sum (function-call "sqrt" (list sum)))
         (asin-sqrt-sum (function-call "asin" (list sqrt-sum)))
         (double-asin-sqrt-sum (arithmetic asin-sqrt-sum
                                           (list
                                            (partial-arithmetic '* 2))))
         (result (function-call "degrees" (list double-asin-sqrt-sum))))
    result))

(define (cone-search ra1 decl1 ra2 decl2 radius)

  ;; CREATE FUNCTION conesearch(
  ;;   ra1 double precision,
  ;;   decl1 double precision,
  ;;   ra2 double precision,
  ;;   decl2 double precision,
  ;;   radius double precision)
  ;; RETURNS  boolean AS $$
  ;; SELECT cos(radians(ra1))*cos(radians(decl1))
  ;;     BETWEEN cos(radians(ra2))*cos(radians(decl2)) - 2*sin(radians(radius/2))
  ;;         AND cos(radians(ra2))*cos(radians(decl2)) + 2*sin(radians(radius/2))
  ;;     AND sin(radians(ra1))*cos(radians(decl1))
  ;;     BETWEEN sin(radians(ra2))*cos(radians(decl2)) - 2*sin(radians(radius/2))
  ;;         AND sin(radians(ra2))*cos(radians(decl2)) + 2*sin(radians(radius/2))
  ;;     AND sin(radians(decl1))
  ;;     BETWEEN sin(radians(decl2)) - 2*sin(radians(radius/2))
  ;;         AND sin(radians(decl2)) + 2*sin(radians(radius/2))
  ;;     AND
  ;;     degrees(2*asin(sqrt(sin(radians((decl2 - decl1)/2))^2 +
  ;;             sin(radians((ra2 - ra1)/2))^2 * (cos(radians((decl2 + decl1)/2))^2 -
  ;;             sin(radians((decl2 - decl1)/2))^2)))) <= radius
  ;; $$ language sql immutable ;

  (let* (

         (half-radius (arithmetic
                       radius
                       (list
                        (partial-arithmetic '/ 2))))

         (radians-half-radius (function-call "radians" (list half-radius)))
         (sin-radians-half-radius (function-call "sin" (list radians-half-radius)))
         (double-sin-radians-half-radius (arithmetic
                                          sin-radians-half-radius
                                          (list (partial-arithmetic '* 2))))
         
         
         (radians-ra1  (function-call "radians" (list ra1)))
         (radians-ra2  (function-call "radians" (list ra2)))
         (radians-decl1  (function-call "radians" (list decl1)))
         (radians-decl2  (function-call "radians" (list decl2)))

         (cos-radians-ra1   (function-call "cos" (list radians-ra1)))
         (sin-radians-ra1   (function-call "sin" (list radians-ra1)))
         (cos-radians-ra2   (function-call "cos" (list radians-ra2)))
         (sin-radians-ra2   (function-call "sin" (list radians-ra2)))
         (cos-radians-decl1 (function-call "cos" (list radians-decl1)))
         (sin-radians-decl1 (function-call "sin" (list radians-decl1)))
         (cos-radians-decl2 (function-call "cos" (list radians-decl2)))
         (sin-radians-decl2 (function-call "sin" (list radians-decl2)))

         (product_1  (arithmetic
                      cos-radians-ra1
                      (list (partial-arithmetic '* cos-radians-decl1))))
         (product_2  (arithmetic
                      cos-radians-ra2
                      (list (partial-arithmetic '* cos-radians-decl2))))
         (product_2-minus-epsilon (arithmetic
                                   product_2
                                   (list (partial-arithmetic '- double-sin-radians-half-radius))))
         (product_2-plus-epsilon (arithmetic
                                  product_2
                                  (list (partial-arithmetic '+ double-sin-radians-half-radius))))

         (predicate_1 (between-predicate
                       product_1
                       product_2-minus-epsilon
                       product_2-plus-epsilon))
         
         (product_3  (arithmetic
                      sin-radians-ra1
                      (list (partial-arithmetic '* cos-radians-decl1))))
         (product_4  (arithmetic
                      sin-radians-ra2
                      (list (partial-arithmetic '* cos-radians-decl2))))
         (product_4-minus-epsilon (arithmetic
                                   product_4
                                   (list (partial-arithmetic '- double-sin-radians-half-radius))))
         (product_4-plus-epsilon (arithmetic
                                  product_4
                                  (list (partial-arithmetic '+ double-sin-radians-half-radius))))
         (predicate_2 (between-predicate
                       product_3
                       product_4-minus-epsilon
                       product_4-plus-epsilon))

         (sin-radians-decl2-minus-epsilon
          (arithmetic
           sin-radians-decl2
           (list (partial-arithmetic '- double-sin-radians-half-radius))))
         (sin-radians-decl2-plus-epsilon
          (arithmetic
           sin-radians-decl2
           (list (partial-arithmetic '+ double-sin-radians-half-radius))))
         (predicate_3 (between-predicate
                       sin-radians-decl1
                       sin-radians-decl2-minus-epsilon
                       sin-radians-decl2-plus-epsilon))
         
         (haversine (angular-distance ra1 decl1 ra2 decl2))
         (distance-pred (comparison "<=" haversine radius))

         (result (and-predicate
                  predicate_1
                  predicate_2
                  predicate_3
                  distance-pred)))
    result))


(define (insert-into-where pred where)
  (cond ((eq? where 'empty-where) pred)
        ((predicate? where)
         (let ((first-expr (predicate->first-expr where))
               (rest-expr  (predicate->rest-expr where)))
           (predicate
            pred
            (cons (partial-predicate "AND" first-expr)
                  rest-expr))))
        (else
         (predicate
          pred
          (list (partial-predicate "AND" where))))))


;; chunk-boundary-list comes from the description file
;; table-name_1 table-name_2
;; ra decl from table 1 ? radius
(define (distributed-spatial-join
         chunk-boundary-list
         query
         table-name_1  ra_1 decl_1
         table-name_2  ra_2 decl_2
         radius
         name->partition
         table->boundary
         cone-search-predicate)    
  
  (define (make-boundary-query)

    ;; Avec les tables de bordures
    ;;  
    ;;  SELECT *
    ;;  FROM BoundaryT1 AS T1,
    ;;       BoundaryT2 AS T2,
    ;;       T3, ...
    ;;  WHERE cone_search(T1.ra1, T1.decl1, T2.ra2, T2.decl2, radius)
    ;;    AND boundary(T1, radius)
    ;;    AND boundary(T2, 2 * radius)
    ;;    AND <PREDICATS>

    ;; Oui mais si les tables T1 et T2 ne sont pas Object ou Source ?

    ;; Dans ce cas il faut  se rabattre sur l'expression ou la bordure
    ;; est ramenee  sur le  master.  C'est  ce que  fait spatial-join
    ;; maintenant.

    ;; Donc  ici on  a forcement des  tables partitionnees,  donc pour
    ;; chaque table il existe une table Boundary correspondante sur le
    ;; master.

    ;; ----

    ;; Faut-il ajouter le predicat boundary(T1, radius) dans T1 ?

    ;; (SELECT * FROM BoundaryT1 WHERE boundary(BoundaryT1, radius)) AS T1

    ;; Le risque est qu'il cherche a materialiser la sous requete ?

    ;; 

    (let ((select    (sql-record->select query))
          (from      (sql-record->from query))
          (where     (sql-record->where query))
          (group-by  (sql-record->group-by query))
          (order-by  (sql-record->order-by query))
          (limit     (sql-record->limit query)))

      ;;     -- On ramene les bordures sur la master
      ;;     SELECT *
      ;;     FROM boundary(T1, radius) AS T1,
      ;;          boundary(T2, 2 * radius) AS T2,
      ;;          T3, ...
      ;;     WHERE cone_search(T1.ra1, T1.decl1, T2.ra2, T2.decl2, radius)
      ;;       AND <PREDICATS>

      (define (boundary
               table-name ra decl radius
               chunkid
               xmin xmax
               ymin ymax
               zmin zmax)
        (within-limit-xyz
         table-name ra decl
         xmin xmax
         ymin ymax
         zmin zmax
         radius)) 


      ;; TODO: create a  UNION of bounded chunks (Note: this
      ;; is  NOT a  distributed query as  the filter  is not
      ;; identical on all chunks !)

      ;; 'make-bound' creates  a  template function  (which
      ;; take a table as  parameter and produces an union of
      ;; bounded chunks)  for  a distributed  table with  a
      ;; boundary.

      (define (make-bound ra decl radius)

        (define (make-where new-name)
          (apply
           and-predicate 
           (map (lambda (chunk)
                  (let* ((pred-list (apply boundary
                                           new-name
                                           ra decl radius
                                           chunk))
                         (pred (apply and-predicate pred-list))
                         (pred (parenthesised-expression pred))
                         (pred (negation-predicate pred)))
                    pred))
                chunk-boundary-list)))
        
        ;; Object O1 =>
        ;; (SELECT * FROM ObjectBoundary WHERE ...) AS O1
        
        (lambda (tbl) ;; tbl = 'Object' for instance
          (let* ((name-boundary-tbl (table->boundary tbl))
                 (boundary-tbl (table-record 'unknown name-boundary-tbl))
                 (star (projection #f (list (field 'unknown "*"))))
                 (where (make-where name-boundary-tbl)))
            (parenthesised-expression
             (sql-record
              star boundary-tbl where
              'empty-group
              'empty-order-by
              'empty-limit))))
        
        ;; (define (make-bounded-chunk
        ;;          database template
        ;;          chunkid
        ;;          xmin xmax
        ;;          ymin ymax
        ;;          zmin zmax)
        ;;   (let* ((new-name  (template chunkid))
        ;;          (new-table (table-record database new-name))
        ;;          (star (projection #f (list (field 'unknown "*"))))
        ;;          (pred-list (boundary new-name ra decl radius
        ;;                               chunkid
        ;;                               xmin xmax
        ;;                               ymin ymax
        ;;                               zmin zmax))
        ;;          (pred  (apply and-predicate pred-list))
        ;;          (pred  (parenthesised-expression pred))
        ;;          (where (negation-predicate pred)))
        ;;     (sql-record
        ;;      star new-table where
        ;;      'empty-group
        ;;      'empty-order-by
        ;;      'empty-limit)))

        ;; (lambda (tbl) ;; tbl = 'Object' for instance
        ;;    (let* ((database  (table-record->database tbl))
        ;;           (name      (table-record->name tbl))
        ;;           (partition (name->partition name)))
        ;;      (let ((name     (modulo-partition->name partition))
        ;;            (key      (modulo-partition->key partition))
        ;;            (number   (modulo-partition->number partition))
        ;;            (template (modulo-partition->template partition)))
        ;;        (parenthesised-expression
        ;;         (apply
        ;;          union-all
        ;;          (map (lambda (chunk)
        ;;                 (apply make-bounded-chunk database template chunk))
        ;;               chunk-boundary-list))))))

        )
      
      (define (make-bound.old ra decl radius)
        (lambda (tbl)
          (let* ((database   (table-record->database tbl))
                 (table-name (table-record->name tbl))
                 (star (projection #f (list (field 'unknown "*"))))
                 (pred-list-list
                  (map (lambda (chunk-boundary)
                         (apply boundary table-name ra decl radius chunk-boundary))
                       chunk-boundary-list))
                 (pred-list
                  (map (lambda (pred-list)
                         (apply and-predicate pred-list))
                       pred-list-list))
                 (pred-list (map parenthesised-expression pred-list))
                 (pred-list (map negation-predicate pred-list))
                 (where (apply and-predicate pred-list)))

            (parenthesised-expression 
             (sql-record
              star tbl where
              'empty-group
              'empty-order-by
              'empty-limit)))))

      ;; c'est ici qu'il faut remplacer avec les tables Boundary si necessaire

      ;;  SELECT *
      ;;  FROM (SELECT * FROM BoundaryT1 WHERE boundary(BoundaryT1, radius)) AS T1,
      ;;       (SELECT * FROM BoundaryT2 WHERE boundary(BoundaryT2, 2 * radius)) AS T2,
      ;;       T3, ...
      ;;  WHERE cone_search(T1.ra1, T1.decl1, T2.ra2, T2.decl2, radius)
      ;;    AND <PREDICATS>

      ;; We use a template function for make-bound because of this:
      ;; SELECT .. FROM Foo Bar ...
      ;; If we want to replace Bar we must work with Foo
      
      (let* ((from (replace-table-with
                    from
                    table-name_1
                    (make-bound ra_1 decl_1 radius)))
             (from (replace-table-with
                    from
                    table-name_2
                    (make-bound ra_2 decl_2 (* radius 2))))
             (new-from from)

             (new-where
              (insert-into-where
               cone-search-predicate
               where)))

        ;; (pp (list (ast->sql where)
        ;;           (ast->sql new-where)))
        ;; (newline)
        
        (sql-record
         select from
         new-where group-by
         order-by limit))))
  

  ;; BUG here "AND AND ..."
  (define (insert-list-into-where pred-list where)
    (cond ((eq? where 'empty-where)
           (let ((head (car pred-list))
                 (rest (cdr pred-list)))
             (predicate
              head
              (map (lambda (p) (partial-predicate "AND" p))
                   rest))))
          
          ((predicate? where)           
           (let* ((head (car pred-list))
                  (rest (cdr pred-list))
                  (rest (map (lambda (p) (partial-predicate "AND" p)) rest))
                  (first-expr (predicate->first-expr where))
                  (rest-expr  (predicate->rest-expr where))
                  (partial-first-expr (partial-predicate "AND" first-expr))
                  (expr-list (cons partial-first-expr rest-expr))
                  (new-list (append rest expr-list))
                  (result (predicate head new-list)))
             result))

          ((or (between-predicate? where)
               (negation-predicate? where)
               (set-predicate? where)
               (comparison? where)
               (like-operator? where)
               (is-null-record? where)
               (any-record? where)
               (all-record? where)
               (some-record? where)
               (exists-record? where))
           (predicate
            where
            (map (lambda (p) (partial-predicate "AND" p))
                 pred-list)))
          
          (else
           (error "insert-list-into-where: Unknown type" where)
           )))

  (define (within-limit-xyz
           table-name ra decl
           xmin xmax
           ymin ymax
           zmin zmax
           radius)

    (define (check-bounds amin amax)
      (or (not (number? amin))
          (not (number? amax))
          (<= amin amax)
          (error "within-limit-xyz: RADIUS is too big => (> min max)"
                 (list amin amax radius))))
    
    ;; x = cos(ra)*cos(decl) ;
    ;; y = sin(ra)*cos(decl) ;
    ;; z = sin(decl) ;

    '(begin (display (list 'within-limit-xyz "First check")) (newline))
    (and
     (check-bounds xmin xmax)
     (check-bounds ymin ymax)       
     (check-bounds zmin zmax))
    
    (let* ((ra   (function-call "radians" (list (field table-name ra))))
           (decl (function-call "radians" (list (field table-name decl))))
           (length (* 2 (sin (/ radius 2.))))
           
           (xmin (+ xmin length))
           (ymin (+ ymin length))
           (zmin (+ zmin length))
           (xmax (- xmax length))
           (ymax (- ymax length))
           (zmax (- zmax length))

           (cos-ra (function-call "cos" (list ra)))
           (sin-ra (function-call "sin" (list ra)))
           (cos-decl (function-call "cos" (list decl)))
           (sin-decl (function-call "sin" (list decl)))

           (X (arithmetic cos-ra (list (partial-arithmetic '* cos-decl))))
           (Y (arithmetic sin-ra (list (partial-arithmetic '* cos-decl))))
           (Z sin-decl))

      '(begin (display (list 'within-limit-xyz "Second check")) (newline))
      (and
       (check-bounds xmin xmax)
       (check-bounds ymin ymax)       
       (check-bounds zmin zmax)
       
       (let ((X-pred (between-predicate X xmin xmax))
             (Y-pred (between-predicate Y ymin ymax))
             (Z-pred (between-predicate Z zmin zmax)))
         (list X-pred Y-pred Z-pred)))))

  (define (union-all tbl . tbl-list)
    (table-combinator
     tbl
     (map (lambda (tbl)
            (table-combinator-partial
             "UNION ALL" tbl))
          tbl-list)))

  ;; We need to obtain the chunk from the table name and its chunkid
  ;; table-name may be an alias name for a distributed table (here it should be a distributed table)
  (define (replace-table-with-chunk from table-name chunkid)
    
    (define (replacement tbl)
      (let* ((database  (table-record->database tbl))
             (name      (table-record->name tbl))
             (partition (name->partition name)))
        ;; (pp (list 'replace-with-chunk tbl name partition)) (newline)
        (let ((name     (modulo-partition->name partition))
              (key      (modulo-partition->key partition))
              (number   (modulo-partition->number partition))
              (template (modulo-partition->template partition)))
          (let ((new-name (template chunkid)))
            (table-record database new-name)))))
    
    (replace-table-with from table-name replacement))

  (define (make-chunk-query chunkid xmin xmax ymin ymax zmin zmax)
    (let ((select    (sql-record->select query))
          (from      (sql-record->from query))
          (where     (sql-record->where query))
          (group-by  (sql-record->group-by query))
          (order-by  (sql-record->order-by query))
          (limit     (sql-record->limit query)))

      ;;     SELECT ...
      ;;     FROM Chunk T1, Chunk T2, T3, ...
      ;;     WHERE cone_search(T1.ra1, T1.decl1, T2.ra2, T2.decl2, radius)
      ;;       AND within_limit(T1, radius)
      ;;       AND <PREDICATS>

      (let* (
             ;;  We must  replace both distributed tables by
             ;; their chunks (and only these tables)

             (from (replace-table-with-chunk from table-name_1 chunkid))
             (from (replace-table-with-chunk from table-name_2 chunkid))
             (new-from from)

             (new-where
              (insert-list-into-where 
               (cons
                cone-search-predicate
                (within-limit-xyz
                 table-name_1 ra_1 decl_1
                 xmin xmax
                 ymin ymax
                 zmin zmax
                 radius))
               where))

             (inside-query 
              (sql-record
               select new-from
               new-where group-by
               order-by limit)))

        inside-query)))

  ;; TODO: don't forget to retrieve borders to the master !
  
  ;;     SELECT ...
  ;;     FROM boundary(T1, radius) AS T1,
  ;;          boundary(T2, 2 * radius) AS T2,
  ;;          T3, ...
  ;;     WHERE cone_search(T1.ra1, T1.decl1, T2.ra2, T2.decl2, radius)
  ;;       AND <PREDICATS>

  ;; Test with boundary only
  ;; (make-boundary-query)


  (let* ((bquery (make-boundary-query)) ;; Correct
         (chunk-query-list
          (map (lambda (chunk)
                 (apply make-chunk-query chunk))
               chunk-boundary-list))
         (result (apply union-all (cons bquery chunk-query-list))))

    result ))

(define (find-table table-name from)

  (define (find-table-in-list table-list)
    (let loop ((table-list table-list))
      (and (not (null? table-list))
           (let ((head (car table-list))
                 (rest (cdr table-list)))
             (or (find-table table-name head)
                 (loop rest)))))) 
  
  (cond
   ((table-record? from)
    (and (string-ci=?
          table-name
          (table-record->name from))
         from))
   ((alias? from)
    (and (string-ci=?
          table-name
          (alias->name from))
         (alias->value from)))
   ((cartesian-product? from)
    (find-table-in-list (cartesian-product->tables from)))
   ((inner-joins? from)
    (find-table-in-list (inner-joins->table-list from)))
   ((join-table-using? from)
    (let ((left-table  (join-table-using->left-table from))
          (right-table (join-table-using->right-table from)))
      (find-table-in-list (list left-table right-table))))
   (else (error "unknown type:" from))))

;; This must be  table names (strings) used in the  FROM clause of the
;; query. radius  must be a constant  (for now).  query must  be a sql
;; query.
(define (spatial-join
         table-name_1 ra_1 decl_1
         table-name_2 ra_2 decl_2
         radius
         query
         chunk-boundary-list
         name->partition
         table->boundary)
  
  ;; ----------------------------------------

  ;; tables must be present only once in the FROM

  ;; Beware if T1 or T2 is not a distributed table 

  ;; So we need to have access to the description of distributed tables

  ;; We need to have chunk boundaries

  ;; A global variable would be BAD

  ;; So we have to pass it and create spatial-join as a closure around it !

  (let* ((T1.ra1   (field table-name_1 ra_1))
         (T1.decl1 (field table-name_1 decl_1))
         (T2.ra2   (field table-name_2 ra_2))
         (T2.decl2 (field table-name_2 decl_2))
         (cone-search-predicate
          (cone-search
           T1.ra1 T1.decl1
           T2.ra2 T2.decl2
           radius)))
    
    ;; (cone-search-predicate
    ;;  (function-call
    ;;   "cone_search"
    ;;   (list T1.ra1 T1.decl1
    ;;         T2.ra2 T2.decl2
    ;;         radius)))
    

    ;; !!! WARNING !!!
    ;; select * from T,T does not work
    ;; so we have to name the tables:
    ;; select * from T T_1, T T_2
    ;; Then pass T_1 and T_2 as parameter
    ;; But in that case we cannot know by name alone if a table is partitionned or not
    ;; We must search inside the FROM clause for the corresponding table
    ;; and find there if it is partitionned or not.

    ;; something like (find-table table-name from-clause)
    ;; then partitionned-table? on the result
    
    (let ((select    (sql-record->select query))
          (from      (sql-record->from query))
          (where     (sql-record->where query))
          (group-by  (sql-record->group-by query))
          (order-by  (sql-record->order-by query))
          (limit     (sql-record->limit query)))

      (let ((table1 (find-table table-name_1 from))
            (table2 (find-table table-name_2 from)))
        (if (not (and table1 table2))
            (error "spatial-join: tables not found in FROM clause."
                   (list
                    (cond ((not table1) table-name_1)
                          ((not table2) table-name_2))
                    from))
            
            (let ((table1-partitionned? (partitionned-table? table1))
                  (table2-partitionned? (partitionned-table? table2)))
              (cond ((and table1-partitionned?
                          table2-partitionned?)
                     (distributed-spatial-join
                      chunk-boundary-list
                      query
                      table-name_1 ra_1 decl_1
                      table-name_2 ra_2 decl_2
                      radius
                      name->partition
                      table->boundary
                      cone-search-predicate))
                    
                    (else
                     (let ((new-where
                            (insert-into-where
                             cone-search-predicate
                             where)))
                       (sql-record
                        select from
                        new-where group-by
                        order-by limit))))))))))


(define functions-hash

  (let ()
    
    ;; TODO:
    ;; - 

    ;; The problem with function like
    ;; areaSpec_box(:raMin, :declMin, :raMax, :declMax)
    ;; is that we don't know on which table it is used
    ;; because it is implicit !

    ;; Therefore   we  provide   variants   for  these
    ;; functions (see VARIANT file)

    ;; Ideally  we must  move this  function predicate
    ;; into  the Geometry  table and  then  replace it
    ;; with   corresponding  library  call.   Here  we
    ;; replace it by its raw mathematical definition.
    (define (spatial-rectangle ra decl raMin declMin raMax declMax)

      (define (real-modulo a b)
        (let ((div (/ a b)))
          (* b (- div (fltruncate div)))))
      
      ;; 0. <= ra <= 360.  
      (define (circle_interval_ra angle min_angle max_angle)
        (if (and (number? min_angle) (number? max_angle))
            (let ((min_angle (real-modulo min_angle 360.))
                  (max_angle (real-modulo max_angle 360.)))
              (if (<= min_angle max_angle)
                  ;; angle BETWEEN min_angle AND max_angle
                  (between-predicate angle min_angle max_angle)
                  
                  ;; angle BETWEEN 0. AND max_angle OR angle BETWEEN min_angle AND 360.
                  (binary-predicate
                   "OR"
                   (between-predicate angle min_angle 360.)
                   (between-predicate angle 0. max_angle))))

            ;;    ((min_angle <= max_angle) AND angle BETWEEN min_angle AND raMAX)
            ;; OR ((min_angle > max_angle) AND (angle BETWEEN 0. AND max_angle OR angle BETWEEN min_angle AND 360.))
            (binary-predicate
             "OR"
             (binary-predicate
              "AND"
              (comparison "<=" min_angle max_angle)
              (between-predicate angle min_angle max_angle))
             (and-predicate
              (comparison ">" min_angle max_angle)
              (between-predicate angle 0. max_angle)
              (between-predicate angle min_angle 360.)))))

      ;; -90. <= decl <= 90. It is not circular 
      (define (circle_interval_decl angle min_angle max_angle)
        (between-predicate angle min_angle max_angle))
      
      (binary-predicate
       "AND"
       (circle_interval_ra ra raMin raMax)
       (circle_interval_decl decl declMin declMax)))

    (define (fluxToAbMag flux)
      (cond ((number? flux) (- (* -2.5 (/ (log flux) (log 10.))) 48.6))
            (else
             (parenthesised-expression 
              (arithmetic
               -2.5
               (list (partial-arithmetic '* (function-call "log" (list flux)))
                     (partial-arithmetic '- 48.6)))))))





    
    ;; 
    ;;              
    ;;         (let ((select    (sql-record->select query))
    ;;               (from      (sql-record->from query))
    ;;               (where     (sql-record->where query))
    ;;               (group-by  (sql-record->group-by query))
    ;;               (order-by  (sql-record->order-by query))
    ;;               (limit     (sql-record->limit query)))
    ;; 
    ;;           (map
    ;;            (lambda (chunk)
    ;;              ;; Pour chaque chunk il faut remplacer T1 et T2 dans le FROM
    ;;              ;; inside-predicate depend des bords du chunk en question
    ;;              ;; Ensuite faire l'UNION ALL de tout ces chunks 
    ;;              (let* ((new-from (replace-table-with-chunk ... from))
    ;;                     (new-where (predicate
    ;;                                 cone-search-predicate
    ;;                                 (partial-predicate "AND" inside-predicate)
    ;;                                 (partial-predicate "AND" where)))
    ;;                     (chunk-query 
    ;;                      (distributed-query 
    ;;                       (sql-record select
    ;;                                   new-from
    ;;                                   new-where
    ;;                                   group-by
    ;;                                   order-by
    ;;                                   limit)
    ;;                       key number)))
    ;;                ...
    ;;                ))             
    ;;            chunk-list)
    ;; 
    ;;           query
    ;;                     
    ;;           )))
    
    (let ((functions-hash (make-table)))
      (table-set! functions-hash "spatial_rectangle" spatial-rectangle)
      (table-set! functions-hash "fluxToAbMag" fluxToAbMag)
      (table-set! functions-hash "angular_distance" angular-distance)
      (table-set! functions-hash "cone_search" cone-search)
      ;; (table-set! functions-hash "SpatialJoin" spatial-join)
      functions-hash)))

