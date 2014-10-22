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

;; ---------------------------------------- ;;

(define LSST_query_001_a ;; Ok
  "SELECT taiMidPoint, psfFlux, psfFluxSigma
FROM   Source
JOIN   Filter USING (filterId)
WHERE  objectId = 430209694171177
   AND filterName = 'i'")

(query-execute LSST_query_001_a "T_LSST_001_a")

(define LSST_query_001_b ;; Ok
  "SELECT taiMidPoint, ra, decl
FROM   Source
JOIN   Filter USING (filterId)
WHERE  objectId = 430209694172154
   AND filterName = 'i'")

(query-execute LSST_query_001_b "T_LSST_001_b")

(define LSST_query_001_c ;; Ok
  "SELECT taiMidPoint, psfFlux, psfFluxSigma, ra, decl
FROM   Source
JOIN   Filter USING (filterId)
WHERE  objectId = 430209694172015
   AND filterName = 'g'")

(query-execute LSST_query_001_c "T_LSST_001_c")

(define LSST_query_003
  "SELECT  *
FROM    Object
WHERE   spatial_rectangle(ra_PS, decl_PS, 0.5, 2.8, 1.2, 3.7)
   AND  gFlux_PS > 0
   AND  rFlux_PS > 0
   AND  iFlux_PS > 0
   AND  zFlux_PS > 0
   AND  fluxToAbMag(zFlux_PS)      BETWEEN 8.2  AND 24.5
   AND  fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) BETWEEN -0.24 AND 0.35
   AND  fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) BETWEEN -0.25 AND -0.15 ;")

(query-execute LSST_query_003 "T_LSST_003")

(define LSST_query_004
  "SELECT fluxToAbMag(gFlux_PS), 
       fluxToAbMag(rFlux_PS),
       fluxToAbMag(iFlux_PS),
       fluxToAbMag(zFlux_PS),
       fluxToAbMag(yFlux_PS)
FROM   Object 
WHERE  gFlux_PS > 0
  AND  rFlux_PS > 0 
  AND  iFlux_PS > 0 
  AND  zFlux_PS > 0 
  AND  yFlux_PS > 0 
  AND  (objectId  % 100 )= 42 ;")

(query-execute LSST_query_004 "T_LSST_004")

(define LSST_query_006_adapted_a
  "SELECT ra, decl, raRange, declRange
FROM   Object 
JOIN   Source USING (objectId)
WHERE  objectId = 383854112153684
AND    latestObsTime = taiMidPoint;
")

(query-execute LSST_query_006_adapted_a "T_LSST_006_a")

(define LSST_query_006_adapted_b
  "SELECT objectId, sourceId, ra, decl, raRange, declRange
FROM   Object 
JOIN   Source USING (objectId)
WHERE  latestObsTime = taiMidPoint
LIMIT 10;")

(query-execute LSST_query_006_adapted_b "T_LSST_006_b")


(define LSST_query_008
  "SELECT objectId
FROM   Object
WHERE  spatial_rectangle(ra_PS, decl_PS, 1.5, 3.8, 2.2, 4.7)
AND    zFlux_PS > 0
AND    fluxToAbMag(zFlux_PS) BETWEEN 8.2  AND 24.5 ;")

(query-execute LSST_query_008 "T_LSST_008")

(define LSST_query_009
  "SELECT *
FROM   Object
WHERE  objectId = 417861663196448 ;")

(query-execute LSST_query_009 "T_LSST_009")

(define LSST_query_014
  "SELECT objectId
FROM   Object
WHERE  spatial_rectangle(ra_PS, decl_PS, -0.5, -5.4, 0.5, -4.8)
AND    iFlux_PS > 0
AND    zFlux_PS > 0
AND    fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) BETWEEN 0.4 AND 0.8 ; ")

(query-execute LSST_query_014 "T_LSST_014")

(define LSST_query_015_adapted
  "SELECT v.objectId, v.ra_PS, v.decl_PS
FROM   Object v, Object o
WHERE  o.objectId = 448781132759061
   AND cone_search(v.ra_PS, v.decl_PS, o.ra_PS, o.decl_PS, 1.);")

(query-execute LSST_query_015_adapted "T_LSST_015")

(define LSST_query_015_bis
  "SELECT v.objectId, v.ra, v.decl
FROM   Source v, Object o
WHERE  o.objectId = 448781132759061
   AND cone_search(v.ra, v.decl, o.ra_PS, o.decl_PS, 1.)
")

(query-execute LSST_query_015_bis "T_LSST_015_b")

;; Vide pour PT12
;;
;; (define LSST_query_019
;;   "SELECT objectId
;; FROM   Object
;; JOIN   Source USING(objectId)
;; WHERE  latestObsTime > 51113
;; GROUP BY (objectId)
;; HAVING COUNT(objectId) = 1 ;")
;; 
;; (query-execute LSST_query_019 "T_LSST_019")

(define LSST_query_019_a
  "SELECT objectId, COUNT(*) AS c
FROM ( SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51113 ) as ids
GROUP BY (objectId) ;")

(query-execute LSST_query_019_a "T_LSST_019_a")

(define LSST_query_019_b
  "SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51113
GROUP BY (objectId)
HAVING COUNT(objectId) < 5 ;")

(query-execute LSST_query_019_b "T_LSST_019_b")


(define LSST_query_022
  "SELECT o1.objectId AS objId1, 
          o2.objectId AS objId2,
          spherical_distance(o1.ra_PS, o1.decl_PS,
                             o2.ra_PS, o2.decl_PS) AS distance
  FROM   Object o1, 
         Object o2
  WHERE  spatial_rectangle(o1.ra_PS, o1.decl_PS, 0.5, 2.8, 1.2, 3.7)
    AND  spherical_distance(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 0.05
    AND  o1.objectId <> o2.objectId ;")

(query-execute LSST_query_022 "T_LSST_022")

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
   AND   fluxToAbMag(S1.gFlux_PS)- fluxToAbMag(S1.rFlux_PS) <  0.7
   AND   fluxToAbMag(S1.rFlux_PS)- fluxToAbMag(S1.iFlux_PS) >  0.4
   AND   fluxToAbMag(S1.iFlux_PS)- fluxToAbMag(S1.zFlux_PS) >  0.4 ;")

(time (query-execute Perf_Q3_crossmatch "T_Petasky_Q3_crossmatch"))

;; La  derniere phase  peut se  decouper aussi:  l'union  des requetes
;; demande d'attendre  mais la  1ere requete se  fait sur le  master !
;; est-il possible de l'executer en  parallele (donc de voir le master
;; comme un pool) ? Ok maintenant

