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

;; Find if an AST uses tables from the same pool and returns
;; it else #f

(define (same-pool? ast myschema)

  (define (myschema:table->pool tbl)
    (let ((table-name (table-record->name tbl)))
      (table->pool myschema table-name)))
  
  (define (extract-all-tables)
    (let ((tq (make-ticket-queue)))
      
      (define-ast-walker walker
        () ;;
        (dispatch  (table-record? table-dispatch))

        ;; Dispatchers

        (define (table-dispatch obj)
          (set! tq (ticket-queue-push tq obj))
          obj))
      
      (let ((new-ast (walker ast)))
        (vector->list (ticket-queue->vector tq)))))

  (let* ((tables (extract-all-tables))
         (pools  (map myschema:table->pool tables)))
    (and (not (null? pools))
         (let ((first-pool (car pools))
               (pools (cdr pools)))
           (and first-pool
                (let loop ((pools pools))
                  (or (null? pools)
                      (let ((head (car pools))
                            (rest (cdr pools)))
                        (and (pool=? first-pool head)
                             (loop rest)))))
                first-pool)))))

(define (same-pool:test database-schema)
  (unit-test
   (let ((ast (rewriter-ast LSST_query_013)))
     (not (same-pool? ast database-schema)))
   (let ((ast (sql->ast "SELECT * FROM master_object_017_xyz, master_source_017_xyz")))
     (same-pool? ast database-schema))
   (let ((ast (sql->ast "SELECT * FROM master_object_012_xyz JOIN master_source_001_xyz USING (objectid)")))
     (same-pool? ast database-schema))
   (let ((ast (sql->ast "SELECT S1.objectId AS s1, S2.objectId AS s2
FROM (SELECT * FROM ObjectBoundary WHERE (-2.5  * log(iFlux_PS) - 48.6)  - (-2.5  * log(zFlux_PS) - 48.6) > .4  AND (-2.5  * log(rFlux_PS) - 48.6)  - (-2.5  * log(iFlux_PS) - 48.6) > .4 AND (-2.5  * log(gFlux_PS) - 48.6)  - (-2.5  * log(rFlux_PS) - 48.6) < .7 AND zFlux_PS > 0. AND iFlux_PS > 0. AND rFlux_PS > 0. AND gFlux_PS > 0. AND NOT (cos(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN .9527779991067244 AND .9972220008932755  AND sin(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN -.04722200089327554 AND -.002777999106724463 AND sin(radians(ObjectBoundary.decl_PS)) BETWEEN -.14722200089327553 AND -.10277799910672447) AND NOT (cos(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN .9527779991067244 AND .9972220008932755  AND sin(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN -.04722200089327554 AND -.002777999106724463 AND sin(radians(ObjectBoundary.decl_PS)) BETWEEN -.09722200089327554 AND -.05277799910672447) AND NOT (cos(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN .9527779991067244 AND .9972220008932755 )) AS S1,
(SELECT * FROM ObjectBoundary WHERE NOT (cos(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN .9555559928537978 AND .9944440071462022  AND sin(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN -.04444400714620223 AND -.005555992853797774 AND sin(radians(ObjectBoundary.decl_PS)) BETWEEN -.14444400714620223 AND -.10555599285379778)  AND NOT (cos(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN .9555559928537978 AND .9944440071462022  AND sin(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN -.04444400714620223 AND -.005555992853797774 AND sin(radians(ObjectBoundary.decl_PS)) BETWEEN -.09444400714620223 AND -.055555992853797774) AND NOT (cos(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN .9555559928537978 AND .9944440071462022  AND sin(radians(ObjectBoundary.ra_PS))  * cos(radians(ObjectBoundary.decl_PS)) BETWEEN -.04444400714620223 AND -.005555992853797774 )) AS S2
WHERE cos(radians(S1.ra_PS))  * cos(radians(S1.decl_PS)) BETWEEN cos(radians(S2.ra_PS))  * cos(radians(S2.decl_PS))  - sin(radians(.002778  / 2))  * 2 AND cos(radians(S2.ra_PS))  * cos(radians(S2.decl_PS))  + sin(radians(.002778  / 2))  * 2  AND sin(radians(S1.ra_PS))  * cos(radians(S1.decl_PS)) BETWEEN sin(radians(S2.ra_PS))  * cos(radians(S2.decl_PS))  - sin(radians(.002778  / 2))  * 2 AND sin(radians(S2.ra_PS))  * cos(radians(S2.decl_PS))  + sin(radians(.002778  / 2))  * 2 AND sin(radians(S1.decl_PS)) BETWEEN sin(radians(S2.decl_PS))  - sin(radians(.002778  / 2))  * 2 AND sin(radians(S2.decl_PS))  + sin(radians(.002778  / 2))  * 2 AND degrees(asin(sqrt(power(sin(radians((S2.decl_PS  - S1.decl_PS)  / 2)), 2)  + power(sin(radians((S2.ra_PS  - S1.ra_PS)  / 2)), 2)  * cos(radians(S1.decl_PS)) * cos(radians(S2.decl_PS))))  * 2) <= .002778 ;" )))
     (same-pool? ast database-schema))
   ))
