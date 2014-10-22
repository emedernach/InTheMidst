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



;; Scheme SQL

(define master-pool (pool "master" "localhost" 5280 "postgres" "postgres"))

(define (query-execute sql output)
  (let* ((myplan (planner
                  master-pool
                  database-schema
                  external-table->local-name
                  sql output))
         (query-plan (car myplan))
         (cleanup-plan (cadr myplan)))
    (time (execute query-plan))
    (time (execute cleanup-plan))
    ;; cleanup-plan
    ))

;; 10 arc sec = 1/360
;; count = 206145
(define Perf_Q3_crossmatch
  "SELECT  S1.objectId AS s1, 
        S2.objectId AS s2
FROM    Object S1,                                   
        Object S2
CROSSMATCH (S1, ra_PS, decl_PS)
       AND (S2, ra_PS, decl_PS)
WITH RADIUS .002778
WHERE    S1.gFlux_PS > 0.
   AND   S1.rFlux_PS > 0.
   AND   S1.iFlux_PS > 0.
   AND   S1.zFlux_PS > 0.
   AND   S1.objectId <> S2.objectId 
   AND   fluxToAbMag(S1.gFlux_PS)- fluxToAbMag(S1.rFlux_PS) <  0.7
   AND   fluxToAbMag(S1.rFlux_PS)- fluxToAbMag(S1.iFlux_PS) >  0.4
   AND   fluxToAbMag(S1.iFlux_PS)- fluxToAbMag(S1.zFlux_PS) >  0.4 ;" )

(pp (planner
     master-pool
     database-schema
     external-table->local-name
     Perf_Q3_crossmatch "T_Perf_Q3_crossmatch"))
(newline)

(time (query-execute Perf_Q3_crossmatch "T_Perf_Q3_crossmatch"))

;; Does not work because column "objectid" specified more than once
(define crossmatch_query_bug
  "SELECT * FROM Object O, Source S
CROSSMATCH (O, ra_PS, decl_PS)
AND (S, ra, decl)
WITH RADIUS 0.0003
WHERE O.objectid <> S.objectid ;")

(define crossmatch_query
  "SELECT O.objectid as oid, S.objectid as soid
FROM Object O, Source S
CROSSMATCH (O, ra_PS, decl_PS)
AND (S, ra, decl)
WITH RADIUS 0.0003
WHERE O.objectid <> S.objectid ;")

(time (query-execute crossmatch_query "T_crossmatch"))

;; ---------------------------------------- ;;

;; TODO:  this query  coud  be parallelised  because of  the
;; "GROUP BY (objectId)"

(define Perf_Q4_adapted ;; Ok
  "SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51048
GROUP BY (objectId)
HAVING COUNT(sourceId) <= 2 ;")

(pp (planner
     master-pool
     database-schema
     external-table->local-name
     Perf_Q4_adapted "T_Q4"))
(newline)

(time (query-execute Perf_Q4_adapted "T_Q4"))

;; ---------------------------------------- ;;

(define Perf_Q5_adapted 
  "SELECT ROUND(CAST(fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) AS numeric),0) AS GR, 
          ROUND(CAST(fluxToAbMag(rFlux_PS) - fluxToAbMag(iFlux_PS) AS numeric),0) AS RI, 
          ROUND(CAST(fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) AS numeric),0) AS IZ, 
          ROUND(CAST(fluxToAbMag(zFlux_PS) - fluxToAbMag(yFlux_PS) AS numeric),0) AS ZY,
          COUNT(*) AS pop
FROM    Object
WHERE    gFlux_PS > 0.
   AND   rFlux_PS > 0.
   AND   iFlux_PS > 0.
   AND   zFlux_PS > 0.
   AND   yFlux_PS > 0.
   AND  (fluxToAbMag(gFlux_PS) + fluxToAbMag(rFlux_PS) +
         fluxToAbMag(iFlux_PS) + fluxToAbMag(zFlux_PS) +
         fluxToAbMag(yFlux_PS)) < 150
GROUP BY GR, RI, IZ, ZY
ORDER BY pop ;")

(pp (planner
     master-pool
     database-schema
     external-table->local-name
     Perf_Q5_adapted "T_Q5"))
(newline)

(time (query-execute Perf_Q5_adapted "T_Q5"))

;;

(define Query022_crossmatch
  "SELECT o1.objectId AS objId1, 
          o2.objectId AS objId2
  FROM   Object o1, 
         Object o2
  CROSSMATCH (o1, ra_PS, decl_PS)
         AND (o2, ra_PS, decl_PS)
  WITH RADIUS 0.005
  WHERE  o1.ra_PS BETWEEN 0.5 AND 1.2
    AND  o1.decl_PS BETWEEN 2.8 AND 3.7
    AND  o1.objectId <> o2.objectId ;")

(planner
 master-pool
 database-schema
 external-table->local-name
 Query022_crossmatch "T_22")

(time (query-execute Query022_crossmatch "T_22"))

;; 

(define function_query
  "SELECT angular_distance(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) AS distance
FROM Object o1, Object o2
WHERE o1.objectid = 383841227243996
  AND o2.objectid = 439508298372785 ;")

