
;; Query repertoire for Petasky

;; TODO:
;; - Funtion generating queries taking <objectid> as parameter

;; qserv_angSep Unit: degre

;; PT12:
;; 0. <= ra_PS <= 5.2
;; -6.81 <= decl_PS <= 7.1
;; fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) [-6.39 , 8.32]
;; r - i [-5.61 , 5.97]
;; i - z [-9.79 , 7.24]
;; z - y [-9.75 , 10.0]

;; Filter distribution :
;; postgres=# select filterid,count(*) from Source_002 group by filterid ;
;;  filterid |  count   
;; ----------+----------
;;         1 |  4565170
;;         2 | 13096746
;;         3 | 18236702
;;         4 |  3699295
;;         5 |  1054360
;; (5 rows)

;; postgres=# select * from Filter ;
;;  filterid | filtername | photclam | photbw 
;; ----------+------------+----------+--------
;;         0 | u          |        0 |      0
;;         1 | g          |        0 |      0
;;         2 | r          |        0 |      0
;;         3 | i          |        0 |      0
;;         4 | z          |        0 |      0
;;         5 | y          |        0 |      0
;;         6 | w          |        0 |      0
;;         7 | V          |        0 |      0
;;       -99 | DD         |        0 |      0
;; (9 rows)
;; 

;; Ref:
;; https://dev.lsstcorp.org/trac/raw-attachment/ticket/3027/LDM-148%20(2).docx.

;;  100 simultaneous low-volume queries against up to 1 square degree in the catalog; response time 10s; resulting data set: 0.5 GB
;;  20 simultaneous high-volume queries against all objects in the catalog; response time 1 hr; resulting data set: 6 GB


;; Ref:
;; https://dev.lsstcorp.org/trac/wiki/DataManagementDescription

;; The main object and source catalogs, as well as the images, are the primary outputs of LSST for end-user astronomers.
;; Queries of these databases need to be controlled to limit resource consumption. It is likely that both number of rows and machine time will be limited.
;; 300 simultaneous low-volume queries (involving a limited sky area) are required to be executed within 10 sec. 10 simultaneous high-volume queries (involving scanning most of the sky) must be completed within an hour.
;; The object catalog will be partitioned spatially. It contains averages of the attributes of each object. The source catalog contains information about each detection of an object, providing history, and will have many more rows than the object table.
;; Separate tables may be created containing scientifically interesting derived columns. An example of this is !ObjectPhotoZ, with photometric redshifts. Joins from these tables back to the main catalogs are quite likely. 


;; Ref:
;; https://dev.lsstcorp.org/trac/wiki/db/ScalableArch

;; The low volume queries  require under 10 second response.
;; High volume  queries require under an  hour response, and
;; the super-high volume queries are expected to be answered
;; in  under 1 week.   We expect  a typical  load seen  in a
;; single  data  access center  will  be  equivalent to  ~50
;; simultaneous low-volume, 20  high-volume and 1 super-high
;; volume queries in flight at any given time.


;; Ref:
;; https://dev.lsstcorp.org/trac/wiki/DC3bDataServingRequirements

;; Number of Named Users 	<10
;; Number of Concurrent Queries 	<5
;; Number of Concurrent File Downloads 	<5
;; Level of technical competence 	High (able to write their own SQL queries)
;; Level of access needed 	read-only
;; Response time for Low volume queries 	<1 minute
;; Response time for High volume queries 	<1 hour (for object table scans)
;; <4 hours (for source/forcedsource table scans)
;; Response time for Superhigh volume queries 	<10 hours
;; Availability 	Available > 18 hrs/day (e.g. 6AM-12midnight CT)
;; No more than three consecutive days per outage
;; No more than 1 outage every two weeks
;; Available until 31-Dec-2011
;; (See comments below. -MikeF)
;; Image Retrieval Latency 	Many images will be on tape, so retrieval delays will occur.
;; (This may be more of an education issue. -MikeF)


;; -- Perf/Q1 (db/queries/029):
;; non retenue car pas de table Neighbors
;; 
;; (define Perf_Q1_adapted ;; A valider
;; "SELECT o.ra_PS, o.decl_PS, o.flags, o.objectid,
;;        o.psfMag_g, o.psfMag_r, o.psfMag_i, o.gMag, o.rMag, o.iMag, 
;;        o.petroRad_r, 
;;        o.q_g, o.q_r, o.q_i, 
;;        o.u_g, o.u_r, o.u_i, 
;;        o.mE1_r, o.mE2_r, o.mRrCc_r, o.mCr4_r, 
;;        o.isoA_r, o.isoB_r, o.isoAGrad_r, o.isoBGrad_r, o.isoPhi_r, 
;;        n.distance, p.r, p.g
;; FROM      Object as o 
;; LEFT JOIN Neighbors as n ON (o.objid = n.objid) 
;; JOIN      Object as p ON (p.objId = n.neighborObjId)
;; WHERE     (o.ra > 120.) AND (o.ra < 240.) 
;;     AND   (o.r > 16.)  AND (o.r < 21.) 
;;     AND   n.neighborObjId = 
;;              ( SELECT nn.neighborObjId
;;                FROM   Neighbors nn
;;                JOIN   Object pp ON (nn.neighborObjId = pp.objectId)
;;                WHERE  nn.objectId = o.objectId 
;;                ORDER BY pp.r
;;                LIMIT 1 )
;; LIMIT 100 ;")


;; -- Perf/Q2 (db/queries/025):
;; raRange and declRange are NULL on PT12
;; iauId is NULL on PT12
(define Perf_Q2_adapted ;; TODO: A verifier si non vide (mais requete tres longue !) et utilise densite (optimisation: a calculer a part)
  "SELECT DISTINCT o1.objectId, o1.ra_PS, o1.decl_PS
FROM   Object o1, Object o2
WHERE  ABS(o2.ra_PS   - o1.ra_PS  ) < 0.1/(2*COS(RADIANS(o1.decl_PS)))
   AND ABS(o2.decl_PS - o1.decl_PS) < 0.05
   AND ( SELECT COUNT(o3.objectId)
         FROM   Object o3
         WHERE  o1.objectId <> o3.objectId
           AND  ABS(o1.ra_PS   - o3.ra_PS  ) < 0.1/COS(RADIANS(o3.decl_PS))
           AND  ABS(o1.decl_PS - o3.decl_PS) < 0.1 ) > 10000 ; ")

;; -- Perf/Q3 (db/queries/043):
;; -- 0.05 a changer
(define Perf_Q3_adapted ;; TODO: A verifer si non vide (mais requete tres longue !)
  "SELECT  S1.objectId AS s1, 
        S2.objectId AS s2
FROM    Object S1,                                   
        Object S2                                    
WHERE   spDist(S1.ra_PS, S1.decl_PS, S2.ra_PS, S2.decl_PS) < .05 
   AND   S1.gFlux_PS > 0.
   AND   S1.rFlux_PS > 0.
   AND   S1.iFlux_PS > 0.
   AND   S1.zFlux_PS > 0.
   AND   fluxToAbMag(S1.gFlux_PS)- fluxToAbMag(S1.rFlux_PS) <  0.7
   AND   fluxToAbMag(S1.rFlux_PS)- fluxToAbMag(S1.iFlux_PS) >  0.4
   AND   fluxToAbMag(S1.iFlux_PS)- fluxToAbMag(S1.zFlux_PS) >  0.4 ;" )

;; 1 arc sec = 1/3600 degre
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

;; Does not work because column "objectid" specified more than once
(define crossmatch_query_1
  "SELECT * FROM Object O, Source S
CROSSMATCH (O, ra_PS, decl_PS)
AND (S, ra, decl)
WITH RADIUS 0.0003
WHERE O.objectid <> S.objectid ;")

(define crossmatch_query_2
  "SELECT O.objectid as oid, S.sourceid as sid
FROM Object O, Source S
CROSSMATCH (O, ra_PS, decl_PS)
AND (S, ra, decl)
WITH RADIUS 0.0003
WHERE O.objectid <> S.objectid ;")


;; -- Perf/Q4 (db/queries/019):
;; (define Perf_Q4_original ;; A valider ;; y a t-il une erreur ie est-ce plutot COUNT(sourceId) = 1 ?
;;   "SELECT objectId
;; FROM   Object
;; JOIN   Source USING(objectId)
;; WHERE  latestObsTime > 51048
;; GROUP BY (objectId)
;; HAVING COUNT(objectId) = 1 ;")

(define Perf_Q4_adapted ;; Ok
  "SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51048
GROUP BY (objectId)
HAVING COUNT(sourceId) <= 2 ;")

;; -- Perf/Q5 (db/queries/036): Ok
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

;; Irrealiste, doit-on vraiment la garder ?
(define Perf_Q6_adapted
  "SELECT * FROM Source ;")

;; Perf query adapted to PT12
(define Perf_Q7_adapted ;; Ok
  "SELECT  objectId
FROM    Object
WHERE    gFlux_PS > 0.
   AND   rFlux_PS > 0.
   AND   iFlux_PS > 0.
   AND   zFlux_PS > 0.
   AND   fluxToAbMag(gFlux_PS)- fluxToAbMag(rFlux_PS) <  0.1
   AND   fluxToAbMag(rFlux_PS)- fluxToAbMag(iFlux_PS) > -0.8
   AND   fluxToAbMag(iFlux_PS)- fluxToAbMag(zFlux_PS) <  1.4 
; ")

;; Perf/Q8 (db/queries/007):
(define Perf_Q8_adapted ;; Ok
  "SELECT objectId, taiMidPoint, psfFlux
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
WHERE  ra_PS BETWEEN 0.5 AND 2.2
  AND  decl_PS BETWEEN -0.5 AND 3.7
  AND  filterName = 'g'
ORDER BY objectId, taiMidPoint ASC ;")

(define Query022 ;; Ok (mais est-ce que 0.005 trop grand ?)

;;   postgres=# CREATE TEMPORARY TABLE pp AS SELECT * FROM (SELECT * FROM master_object_000 WHERE objectId  % 4 = 0 UNION ALL SELECT * FROM master_object_001 WHERE objectId  % 4 = 1 UNION ALL SELECT * FROM master_object_002 WHERE objectId  % 4 = 2 UNION ALL SELECT * FROM master_object_003 WHERE objectId  % 4 = 3) AS o1 WHERE o1.ra_PS BETWEEN .5 AND 1.2  AND o1.decl_PS BETWEEN 2.8 AND 3.7 ;
;; SELECT 44313
;; Time: 6722.405 ms
;; postgres=# SELECT COUNT(*) FROM pp ;
;;  count 
;; -------
;;  44313
;; (1 row)
;; 
;; Time: 34.529 ms

;;   CREATE TEMPORARY TABLE oo AS select  o1.objectId AS objId1, 
;;           o2.objectId AS objId2
;; FROM pp o1, pp o2 
;; WHERE abs(o1.ra_PS - o2.ra_PS) + abs(o1.decl_PS - o2.decl_PS) <= 0.05 AND o1.objectid <> o2.objectid ;
  
  "SELECT o1.objectId AS objId1, 
          o2.objectId AS objId2,
          spDist(o1.ra_PS, o1.decl_PS,
                 o2.ra_PS, o2.decl_PS) AS distance
  FROM   Object o1, 
         Object o2
  WHERE  o1.ra_PS BETWEEN 0.5 AND 1.2
    AND  o1.decl_PS BETWEEN 2.8 AND 3.7
    AND  spDist(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 0.005
    AND  o1.objectId <> o2.objectId ;")

(define Query022_crossmatch
  "SELECT o1.objectId AS objId1, 
          o2.objectId AS objId2,
          angular_distance(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) AS distance
  FROM   Object o1, 
         Object o2
  CROSSMATCH (o1, ra_PS, decl_PS)
         AND (o2, ra_PS, decl_PS)
  WITH RADIUS 0.005
  WHERE  o1.ra_PS BETWEEN 0.5 AND 1.2
    AND  o1.decl_PS BETWEEN 2.8 AND 3.7
    AND  o1.objectId <> o2.objectId ;")


;; Ref:
;; https://dev.lsstcorp.org/trac/wiki/db/Qserv/250NodeTestPlan

;;; Run the following queries all at the same time:
;;;
;;;    2 LV1 queries, each with different objectId
;;;    2 LV2 queries, each with different objectId
;;;    3 LV3 queries, two of them for regions that overlap
;;;    1 HV2 query 

;; Similar to Standard Query 009
(define LV1_250NodeTestPlan ;; Ok
  "SELECT * FROM Object WHERE objectId = 383841227243996 ;")

(define LV1_250NodeTestPlan_a ;; Ok
  "SELECT * FROM Source WHERE sourceId = 29808210533745462 ;")

(define LV1_250NodeTestPlan_b ;; Ok
  "SELECT * FROM Source WHERE objectId = 439508298372785 ;")

(define LV2_250NodeTestPlan ;; Ok
  "SELECT taiMidPoint, fluxToAbMag(psfFlux), fluxToAbMag(psfFluxSigma), ra, decl 
FROM   Source
WHERE  objectId = 383841227244506 ;")

;; Similar to Standard Query 003
(define LV3_250NodeTestPlan ;; Ok
  "SELECT count(*) 
FROM   Object 
WHERE  ra_PS between 1 and 2
   AND decl_PS between 3 and 4
   AND gFlux_PS > 0.
   AND rFlux_PS > 0.
   AND iFlux_PS > 0.
   AND zFlux_PS > 0.
   AND fluxToAbMag(zFlux_PS) BETWEEN 21 AND 21.5
   AND fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) BETWEEN 0.3 AND 0.4
   AND fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) BETWEEN 0.1 AND 0.12 ;")

(define HV1_250NodeTestPlan ;; Ok
  "SELECT COUNT(*) FROM Object ;")

;; Similar to HV3_IN2P3_300
;; (define HV2_250NodeTestPlan ;; Ok
;;   "SELECT objectId, 
;;        ra_PS, decl_PS, 
;;        uFlux_PS, gFlux_PS, rFlux_PS, iFlux_PS, zFlux_PS, yFlux_PS
;; FROM   Object
;; WHERE iFlux_PS > 0.
;;  AND  zFlux_PS > 0.
;;  AND  fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) > 4 ;")

;; Per chunk density
;; chunkId is NULL on PT12
(define HV3_250NodeTestPlan ;; Ok
  "SELECT count(*) AS n, AVG(ra_PS), AVG(decl_PS), htmId20
FROM Object
GROUP BY htmId20 ;")

;; Near Neighbors over 100 sq deg.
(define SHV1_250NodeTestPlan ;; Ok sauf que qserv_areaspec_box(-5,-5,5,-5) trop grand pour PT12
  "SELECT count(*) 
FROM   Object o1, Object o2 
WHERE  o1.ra_PS BETWEEN 0. AND 5.
  AND  o1.decl_PS BETWEEN -5. AND 5.
  AND  qserv_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 0.1 ;")

;; Find objects that moved above certain threshold. This query involves an Object-Source join over the entire sky
(define SHV2_250NodeTestPlan ;; Ok (0.0045 has been replaced with 0.0005)
  "SELECT o.objectId, s.sourceId, s.ra, s.decl, o.ra_PS, o.decl_PS
FROM   Object o, Source s
WHERE  o.objectId = s.objectId 
   AND spDist(s.ra, s.decl, o.ra_PS, o.decl_PS) > 0.0005 ;")

;; Ref:
;; https://dev.lsstcorp.org/trac/wiki/db/Qserv/in2p3_300

;; (define LV1_IN2P3_300
;;  "SELECT * FROM Object WHERE objectId = <id> ;")

(define LV2_IN2P3_300 ;; Ok
  "SELECT  count(*)
FROM Object
WHERE ra_PS BETWEEN 1 AND 2
 AND  decl_PS BETWEEN 3 AND 4 
 AND  zFlux_PS > 0 
 AND  fluxToAbMag(zFlux_PS) BETWEEN 21 AND 21.5 ;")

(define LV3_IN2P3_300 ;; Ok
  "SELECT count(*) FROM Object 
WHERE   ra_PS BETWEEN 1 AND 2
AND     decl_PS BETWEEN 3 AND 4
AND   gFlux_PS > 0.
AND   rFlux_PS > 0.
AND   iFlux_PS > 0.
AND   zFlux_PS > 0.
AND	 fluxToAbMag(zFlux_PS) BETWEEN 21 AND 21.5
AND	 fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) BETWEEN 0.3 AND 0.4
AND	 fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) BETWEEN 0.1 AND 0.12 ;")

(define LV4_IN2P3_300 ;; Ok
  "SELECT s.ra, s.decl 
FROM Object o 
JOIN Source s USING (objectId)
WHERE o.objectId = 396210733061753
AND o.latestObsTime = s.taiMidPoint ;")

(define LV4_IN2P3_300_a ;; Ok
  "SELECT ra, decl 
FROM Object 
JOIN Source USING (objectId)
WHERE objectId = 396210733061753
AND latestObsTime = taiMidPoint ;")

;; Similar to LV3_IN2P3_300
;; (define HV1_IN2P3_300 ;; Ok
;;   "SELECT COUNT(*) FROM Object ;")

;; Full scans
(define HV2_IN2P3_300 ;; Ok
  "SELECT COUNT(*) FROM Object WHERE gFlux_PS > 1e-25 ;")

;; Similar to Standard Query 037
(define HV3_IN2P3_300 ;; Ok
  "SELECT  objectId, ra_PS, decl_PS, uFlux_PS, gFlux_PS,
	   rFlux_PS,iFlux_PS, zFlux_PS, yFlux_PS
	FROM   Object
	WHERE  iFlux_PS > 0.
 AND   zFlux_PS > 0. 
 AND   fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) > 4 ;")

(define HV3_IN2P3_300_a ;; Ok
  "SELECT  objectId, uFlux_PS, gFlux_PS,
	   rFlux_PS,iFlux_PS, zFlux_PS, yFlux_PS
	FROM   Object
	WHERE  iFlux_PS > 0.
 AND   zFlux_PS > 0. 
 AND   fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) > 4 ;")

(define HV4_IN2P3_300 ;; Ok
  "SELECT  objectId, ra_PS, decl_PS, fluxToAbMag(zFlux_PS)
	FROM  Object 
	WHERE zFlux_PS > 0. 
 AND  fluxToAbMag(zFlux_PS) BETWEEN 25 AND 26 ;")

(define HV4_IN2P3_300_a ;; Ok
  "SELECT  objectId, fluxToAbMag(zFlux_PS)
	FROM  Object 
	WHERE zFlux_PS > 0. 
 AND  fluxToAbMag(zFlux_PS) BETWEEN 25 AND 26 ;")

(define HV5_IN2P3_300 ;; Ok mais gros volume !
  "SELECT  objectId
FROM  Source
JOIN  Object USING(objectId) 
WHERE  ra_PS BETWEEN  3 AND  4
AND decl_PS BETWEEN -3 AND -2 ;")

(define HV6_IN2P3_300 ;; Ok mais gros volume !
  "SELECT  COUNT(*) FROM Object o1, Object o2 
WHERE o1.ra_PS BETWEEN  0 AND  5
  AND o1.decl_PS BETWEEN -7 AND  7 
AND spDist(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 0.1 ;")

(define HV6_IN2P3_300_adapted
  "SELECT  COUNT(*) FROM Object o1, Object o2 
WHERE o1.ra_PS BETWEEN  0 AND  5
  AND o1.decl_PS BETWEEN -7 AND  7 
AND cone_search(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS, 0.1) ;")

;; Ref:
;; https://dev.lsstcorp.org/trac/wiki/db/Qserv/JHUScalingTest2012

;; Similar to LV3_250NodeTestPlan
;; (define LV1_JHUScalingTest2012 ;; Ok
;; "SELECT count(*) 
;; FROM  Object 
;; WHERE  ra_PS between 1 and 2 
;;    AND decl_PS between 3 and 4
;;    AND   gFlux_PS > 0.
;;    AND   rFlux_PS > 0.
;;    AND   iFlux_PS > 0.
;;    AND   zFlux_PS > 0.
;;    AND  fluxToAbMag(zFlux_PS) BETWEEN 21 AND 21.5
;;    AND  fluxToAbMag(gFlux_PS)- fluxToAbMag(rFlux_PS) BETWEEN 0.3 AND 0.4
;;    AND  fluxToAbMag(iFlux_PS)- fluxToAbMag(zFlux_PS) BETWEEN 0.1 AND 0.12;"

;; Ref:
;; https://dev.lsstcorp.org/trac/wiki/db/SpatialJoinPerf

;; (define HV1_SpatialJoinPerf ;; ???
;;   "SELECT count(*)
;; FROM   X o1, X o2 
;; WHERE  o1.bMag BETWEEN 20 AND 20.13
;;   AND  o2.rMag BETWEEN 19.97 AND 20.15
;;   AND  ABS(o1.ra - o2.ra) < 0.00083 / o2.cosRadDecl 
;;   AND  ABS(o1.decl - o2.decl) < 0.00083 
;;   AND  o1.objectId <> o2.objectId ;")

;; Ref:
;; https://dev.lsstcorp.org/trac/wiki/db/Qserv/OptimalPartitionSize

;; Chunk count overhead
(define Q1_OptimalPartitionSize ;; Ok
  "SELECT COUNT(*) FROM Source;")

;; Full scan
(define Q2_OptimalPartitionSize ;; Ok
  "SELECT objectId, ra_PS, decl_PS,  fluxToAbMag(zFlux_PS) 
   FROM Object 
   WHERE  zFlux_PS > 0. AND fluxToAbMag(zFlux_PS) BETWEEN 20 AND 24;")

;; Near neighbor (self join) subchunk overhead (probably few to several hours to execute)
(define Q3_OptimalPartitionSize ;; Ok
  "SELECT o1.objectId, o2.objectId, fluxToAbMag(o1.uFlux_PS), fluxToAbMag(o2.uFlux_PS)
    FROM Object o1, Object o2 
    WHERE o1.ra_PS BETWEEN 0.5 AND 1.5
    AND   o1.decl_PS BETWEEN 0.5 AND 1.5
    AND spDist(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_Ps) < 0.2;")

;; TODO: use cone_search
(define ConeSearch_Query_001
  "SELECT *
FROM Object
WHERE spDist(ra_PS, decl_PS, 1.3, 3.4) < 0.2;")

(define ConeSearch_Query_001_a
  "SELECT *
FROM Object
WHERE q3c_radial_query(ra_PS, decl_PS, 1.3, 3.4, 0.2);")

