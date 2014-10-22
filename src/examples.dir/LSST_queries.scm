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



(include "../sql.dir/schema-macros.scm")

;;
;; From https://dev.lsstcorp.org/trac/wiki/db/queries
;;

;; ------------------------------------------------ ;;

;; Beware that the Source table will be about 4PB,
;; so even  if we split  it by 1000 we  still have
;; 4TB tables to manage.

;; Note: LSST resolution is 0.2 arssec per pixel.

;; ------------------------------------------------ ;;


(define LSST_query_001_a ;; Ok
  "SELECT taiMidPoint, psfFlux, psfFluxSigma
FROM   Source
JOIN   Filter USING (filterId)
WHERE  objectId = 430209694171177
   AND filterName = 'i'")

(define LSST_query_001_b ;; Ok
  "SELECT taiMidPoint, ra, decl
FROM   Source
JOIN   Filter USING (filterId)
WHERE  objectId = 430209694172154
   AND filterName = 'i'")

(define LSST_query_001_c ;; Ok
  "SELECT taiMidPoint, psfFlux, psfFluxSigma, ra, decl
FROM   Source
JOIN   Filter USING (filterId)
WHERE  objectId = 430209694172015
   AND filterName = 'g'")

;; NO: _ObjectToType, ObjectType and description not in PT12
(define LSST_query_002
  "SELECT *
FROM   Object
JOIN   _ObjectToType USING(objectId)
JOIN   ObjectType USING (typeId)
WHERE  description = 'Supernova'
  AND  variability > 0.8
  AND  probability > 0.8 ;")

;; TODO: need Geometry  functions and replace zMag
;; with corresponding computation
;; areaSpec_box(:raMin, :declMin, :raMax, :declMax) 
(define LSST_query_003_original
  "SELECT  *
FROM    Object
WHERE   areaSpec_box(10.5, 30.5, 12.2, 32.7)
   AND  zMag      BETWEEN 8.2  AND 24.5
   AND  gMag-rMag BETWEEN -0.24 AND 0.35
   AND  iMag-zMag BETWEEN -0.25 AND -0.15 ;")

;; - Use spatial_rectangle instead of areaSpec_box
;;   spatial_rectangle(<ra>,<decl>,<raMin>,<declMin>,<raMax>,<declMax>)
;; - Use fluxToAbMag UDF to obtain magnitudes
;; - Be sure that all flux > 0
;; - In PT12 -3. <= ra <= 6. and -7.0 <= decl <= 7.2
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

;; TODO: Provide a UDF for efficient sampling instead of (id % .. = ..)
(define LSST_query_004_original
  "SELECT fluxToAbMag(uFlux_PS), 
       fluxToAbMag(gFlux_PS), 
       fluxToAbMag(rFlux_PS), 
       fluxToAbMag(iFlux_PS), 
       fluxToAbMag(zFlux_PS), 
       fluxToAbMag(yFlux_PS)
FROM   Object 
WHERE  uFlux_PS > 0
  AND  gFlux_PS > 0 
  AND  rFlux_PS > 0
  AND  iFlux_PS > 0
  AND  zFlux_PS > 0
  AND  yFlux_PS > 0 
  AND  (objectId  % 100 )= 42 ;")

;; - In PT12 not any object has all UGRIZY values
;; - Add flux > 0
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

;; NO:  _Alert2Type, AlertType not in PT12
(define LSST_query_005
  "SELECT objectId 
FROM   Alert 
JOIN   _Alert2Type USING (alertId) 
JOIN   AlertType USING (alertTypeId)
WHERE  alertTypeDescr = 'newTransients'
  AND  Alert.timeGenerated BETWEEN :timeMin AND :timeMax ;")

;; Problem  with  alias  'o'  because  Object  table  disappear  after
;; distribution with table Source.
(define LSST_query_006
  "SELECT s.ra, s.decl, o.raRange, o.declRange
FROM   Object o
JOIN   Source s USING (objectId)
WHERE  o.objectId = 448785427743428
AND    o.latestObsTime = s.taiMidPoint;
")


(define LSST_query_006_adapted_a
  "SELECT ra, decl, raRange, declRange
FROM   Object 
JOIN   Source USING (objectId)
WHERE  objectId = 383854112153684
AND    latestObsTime = taiMidPoint;
")

(define LSST_query_006_adapted_b
  "SELECT objectId, sourceId, ra, decl, raRange, declRange
FROM   Object 
JOIN   Source USING (objectId)
WHERE  latestObsTime = taiMidPoint
LIMIT 10;")

;; NO: no variability, we should adapt it
(define LSST_query_007_original
  "SELECT objectId, taiMidPoint, fluxToAbMag(psfMag)
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
WHERE  areaSpec_box(10.5, 30.5, 12.2, 32.7)
  AND  filterName = 'u'
  AND  variability BETWEEN .6 AND 1.
ORDER BY objectId, taiMidPoint ASC")

;; - No psfMag in PT12
;; - spatial_rectangle instead of areaSpec_box
;; - query in non empty PT12 area
;; - No variability in PT12 => replaced with G - R
;; - No results in PT12 !
(define LSST_query_007
  "SELECT objectId, taiMidPoint, fluxToAbMag(psfFlux)
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
WHERE  spatial_rectangle(ra_PS, decl_PS, -2.1, -4.2, -1.8, -3.8)
  AND  psfFlux > 0.
  AND  filterName = 'u'
  AND  fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) BETWEEN -0.24 AND 0.35
ORDER BY objectId, taiMidPoint ASC")

(define LSST_query_007_adapted
  "SELECT objectId, taiMidPoint, filterName, fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS)
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
WHERE  spatial_rectangle(ra_PS, decl_PS, 1.8, -4.2, 2.1, -3.8)
  AND  psfFlux > 0.
  AND  gFlux_PS > 0.
  AND  rFlux_PS > 0.
  AND  filterName = 'r'
  AND  fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) BETWEEN -0.4 AND 0.
ORDER BY objectId, taiMidPoint ASC")

(define LSST_query_008_original
  "SELECT objectId
FROM   Object
WHERE  areaSpec(10.5, 30.5, 12.2, 32.7)
AND    variability > 0.8")

;; - Use spatial_rectangle instead of areaSpec_box
;;   spatial_rectangle(<ra>,<decl>,<raMin>,<declMin>,<raMax>,<declMax>)
;; - No variability in PT12 => replaced with Z => Z > 0
(define LSST_query_008
  "SELECT objectId
FROM   Object
WHERE  spatial_rectangle(ra_PS, decl_PS, 1.5, 3.8, 2.2, 4.7)
AND    zFlux_PS > 0
AND    fluxToAbMag(zFlux_PS) BETWEEN 8.2  AND 24.5 ;")

(define LSST_query_009
  "SELECT *
FROM   Object
WHERE  objectId = 417861663196448 ;")

;; Not for PT12: Not any object have all UGRIZY
(define LSST_query_010
  "SELECT  objectId
FROM    Object
WHERE   extendedParameter > 0.8 
  AND   uMag BETWEEN 1 AND 27  
  AND   gMag BETWEEN 1 AND 27
  AND   rMag BETWEEN 1 AND 27
  AND   iMag BETWEEN 1 AND 27
  AND   zMag BETWEEN 1 AND 27
  AND   yMag BETWEEN 1 AND 27
  AND (                           
         uAmplitude > .1 + ABS(uMagSigma)
      OR gAmplitude > .1 + ABS(gMagSigma)
      OR rAmplitude > .1 + ABS(rMagSigma)
      OR iAmplitude > .1 + ABS(iMagSigma)
      OR zAmplitude > .1 + ABS(zMagSigma)
      OR yAmplitude > .1 + ABS(yMagSigma));
")

;; No variability in PT12
(define LSST_query_011
  "SELECT *
FROM   Object
WHERE  variability > 0.8 
   AND uTimescale < :timescaleMax
   AND gTimescale < :timescaleMax
   AND rTimescale < :timescaleMax
   AND iTimescale < :timescaleMax
   AND zTimescale < :timescaleMax
   AND yTimescale < :timescaleMax
    OR primaryPeriod BETWEEN :periodMin AND :periodMax 
    OR uAmplitude > :amplitudeMin
    OR gAmplitude > :amplitudeMin
    OR rAmplitude > :amplitudeMin
    OR iAmplitude > :amplitudeMin
    OR zAmplitude > :amplitudeMin
    OR yAmplitude > :amplitudeMin
")

;; Not understood by Postgres ??
;; No _Object2Type table in PT12 
(define LSST_query_012
  "SELECT  COUNT(*)                                               AS totalCount,
        SUM(CASE WHEN (typeId=3) THEN 1 ELSE 0 END)            AS galaxyCount,
        SUM(CASE WHEN (typeId=6) THEN 1 ELSE 0 END)            AS starCount,
        SUM(CASE WHEN (typeId NOT IN (3,6)) THEN 1 ELSE 0 END) AS otherCount
FROM    Object
JOIN    _Object2Type USING(objectId)
WHERE  (uMag-gMag > 2.0 OR uMag > 22.3) 
   AND iMag BETWEEN 0 AND 19 
   AND gMag - rMag > 1.0 
   AND ( (rMag-iMag < 0.08 + 0.42 * (gMag-rMag - 0.96)) OR (gMag-rMag > 2.26 ) )
   AND iMag-zMag < 0.25
")

(define LSST_query_013_buggee
  ;; This is wrong : "o1.fluxToAbMag(..)"
  "SELECT DISTINCT o1.objectId, o2.objectId
FROM   Object o1, 
       Object o2
WHERE  spDist(o1.ra, o1.decl, o2.ra, o2.decl) < 0.1
  AND  o1.objectId <> o2.objectId
  AND  ABS( (o1.fluxToAbMag(uFlux_PS)-o1.fluxToAbMag(gFlux_PS)) - 
            (o2.fluxToAbMag(uFlux_PS)-o2.fluxToAbMag(gFlux_PS)) ) < :colorCut1
  AND  ABS( (o1.fluxToAbMag(gFlux_PS)-o1.fluxToAbMag(rFlux_PS)) - 
            (o2.fluxToAbMag(gFlux_PS)-o2.fluxToAbMag(rFlux_PS)) ) < :colorCut2
  AND  ABS( (o1.fluxToAbMag(rFlux_PS)-o1.fluxToAbMag(iFlux_PS)) - 
            (o2.fluxToAbMag(rFlux_PS)-o2.fluxToAbMag(iFlux_PS)) ) < :colorCut3
  AND  ABS( (o1.fluxToAbMag(iFlux_PS)-o1.fluxToAbMag(zFlux_PS)) - 
            (o2.fluxToAbMag(iFlux_PS)-o2.fluxToAbMag(zFlux_PS)) ) < :colorCut4
")

(define LSST_query_013_original
  "SELECT DISTINCT o1.objectId, o2.objectId
FROM   Object o1, 
       Object o2
WHERE  spDist(o1.ra, o1.decl, o2.ra, o2.decl) < 0.1
  AND  o1.objectId <> o2.objectId
  AND  ABS( (fluxToAbMag(uFlux_PS)-fluxToAbMag(gFlux_PS)) - 
            (fluxToAbMag(uFlux_PS)-fluxToAbMag(gFlux_PS)) ) < :colorCut1
  AND  ABS( (fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS)) - 
            (fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS)) ) < :colorCut2
  AND  ABS( (fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS)) - 
            (fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS)) ) < :colorCut3
  AND  ABS( (fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS)) - 
            (fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS)) ) < :colorCut4
")

;; - TODO: cone search
(define LSST_query_013
  "SELECT DISTINCT o1.objectId, o2.objectId
FROM   Object o1, 
       Object o2
WHERE  spDist(o1.ra, o1.decl, o2.ra, o2.decl) < 0.1
  AND  o1.objectId <> o2.objectId
  AND  ABS( (fluxToAbMag(uFlux_PS)-fluxToAbMag(gFlux_PS)) - 
            (fluxToAbMag(uFlux_PS)-fluxToAbMag(gFlux_PS)) ) < :colorCut1
  AND  ABS( (fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS)) - 
            (fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS)) ) < :colorCut2
  AND  ABS( (fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS)) - 
            (fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS)) ) < :colorCut3
  AND  ABS( (fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS)) - 
            (fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS)) ) < :colorCut4
")

(define LSST_query_014_original
  "SELECT objectId
FROM   Object
WHERE  areaSpec_box(10.5, 30.5, 12.2, 32.7)
AND    extendedParameter > 0.8 ")

;; - spatial_rectangle
;; - extendedness not in PT12 => replaced by I - Z
(define LSST_query_014
  "SELECT objectId
FROM   Object
WHERE  spatial_rectangle(ra_PS, decl_PS, -0.5, -5.4, 0.5, -4.8)
AND    iFlux_PS > 0
AND    zFlux_PS > 0
AND    fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) BETWEEN 0.4 AND 0.8 ; ")

(define LSST_query_015_original
  "SELECT v.objectId, v.ra, v.decl
FROM   Object v, Object o
WHERE  o.objectId = 448781132759061
   AND spDist(v.ra, v.decl, o.ra, o.decl, :dist)
   AND v.variability > 0.8
   AND o.extendedParameter > 0.8
")

;; TODO: cone search
;; - No variability in PT12
;; - No extendedness in PT12
(define LSST_query_015
  "SELECT v.objectId, v.ra, v.decl
FROM   Object v, Object o
WHERE  o.objectId = 448781132759061
   AND spDist(v.ra, v.decl, o.ra, o.decl, :dist)
")

;; Ok to use cone_search because o is in fact constant.
(define LSST_query_015_adapted
  "SELECT v.objectId, v.ra_PS, v.decl_PS
FROM   Object v, Object o
WHERE  o.objectId = 448781132759061
   AND cone_search(v.ra_PS, v.decl_PS, o.ra_PS, o.decl_PS, 1.);")

(define LSST_query_015_bis
  "SELECT v.objectId, v.ra, v.decl
FROM   Source v, Object o
WHERE  o.objectId = 448781132759061
   AND cone_search(v.ra, v.decl, o.ra_PS, o.decl_PS, 1.)
")

(define LSST_query_018_original
  "SELECT objectId,
       uMag, gMag, rMag, iMag, zMag, yMag,
       ra, decl
FROM   Object
WHERE  ( ugColor > 2.0 OR uMag > 22.3 )
AND    extendedParameter < 0.2  
AND    iMag BETWEEN 0 AND 19 
AND    gMag-rMag > 1.0 
AND    ( rMag-iMag < (0.08 + 0.42 * (gMag-rMag - 0.96)) OR gMag-rMag > 2.26 ) 
AND    iMag-zMag < 0.25 ;")

;; uMag => fluxToAbMag(uFlux_PS)
;; ...
;; Emacs: \(.\)Mag -> fluxToAbMag(\1Flux_PS))
;; ra => ra_PS
;; decl => decl_PS
;; ugColor => fluxToAbMag(uFlux_PS) - fluxToAbMag(gFlux_PS)
;; remove extendedParameter (not in PT12)
(define LSST_query_018
  "SELECT objectId,
       fluxToAbMag(uFlux_PS), fluxToAbMag(gFlux_PS),
       fluxToAbMag(rFlux_PS), fluxToAbMag(iFlux_PS),
       fluxToAbMag(zFlux_PS), fluxToAbMag(yFlux_PS),
       ra_PS, decl_PS
FROM   Object
WHERE  uFlux_PS > 0
AND    gFlux_PS > 0
AND    rFlux_PS > 0 
AND    iFlux_PS > 0 
AND    zFlux_PS > 0 
AND    yFlux_PS > 0 
AND    ( fluxToAbMag(uFlux_PS) - fluxToAbMag(gFlux_PS) > 2.0 OR fluxToAbMag(uFlux_PS) > 22.3 )
AND    fluxToAbMag(iFlux_PS) BETWEEN 0 AND 19 
AND    fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) > 1.0 
AND    ( fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS) < (0.08 + 0.42 * (fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) - 0.96)) OR fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) > 2.26 ) 
AND    fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) < 0.25 ;")

(define LSST_query_019_original
  "SELECT objectId
FROM   Object
JOIN   DIASource USING(objectId)
WHERE  latestObsTime > :time
GROUP BY (objectId)
HAVING COUNT(objectId) = 1 ;")

;; DIASource => Source
;; :time replaced
;; Empty in PT12 ??
(define LSST_query_019
  "SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51113
GROUP BY (objectId)
HAVING COUNT(objectId) = 1 ;")

(define LSST_query_019_a
  "SELECT objectId, COUNT(*) AS c
FROM ( SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51113 ) as ids
GROUP BY (objectId) ;")

(define LSST_query_019_b
  "SELECT objectId
FROM   Object
JOIN   Source USING(objectId)
WHERE  latestObsTime > 51113
GROUP BY (objectId)
HAVING COUNT(objectId) < 5 ;")

(define LSST_query_020_original
  "SELECT gMag, objectId
FROM   Object
WHERE  extendedParameter < 0.2
   AND gMag <= 22
   AND uMag-gMag >= -0.27 AND uMag-gMag < 0.71
   AND gMag-rMag >= -0.24 AND gMag-rMag < 0.35
   AND rMag-iMag >= -0.27 AND rMag-iMag < 0.57
   AND iMag-zMag >= -0.35 AND iMag-zMag < 0.70
;")

;; Magnitudes
;; uFlux_PS > 0 ...
;; removed extendedParameter
(define LSST_query_020_old
  "SELECT fluxToAbMag(gFlux_PS), objectId
FROM   Object
WHERE  uFlux_PS > 0
AND    gFlux_PS > 0
AND    rFlux_PS > 0 
AND    iFlux_PS > 0 
AND    zFlux_PS > 0 
AND    yFlux_PS > 0 
   AND fluxToAbMag(gFlux_PS) <= 22
   AND fluxToAbMag(uFlux_PS)-fluxToAbMag(gFlux_PS) >= -0.27 AND fluxToAbMag(uFlux_PS)-fluxToAbMag(gFlux_PS) < 0.71
   AND fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) >= -0.24 AND fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) < 0.35
   AND fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS) >= -0.27 AND fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS) < 0.57
   AND fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) >= -0.35 AND fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) < 0.70
;")

(define LSST_query_020
  "SELECT fluxToAbMag(gFlux_PS), objectId
FROM   Object
WHERE  gFlux_PS > 0
AND    rFlux_PS > 0 
AND    iFlux_PS > 0 
AND    zFlux_PS > 0 
AND    yFlux_PS > 0 
   AND fluxToAbMag(gFlux_PS) <= 22
   AND fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) >= -0.24 AND fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) < 0.35
   AND fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS) >= -0.27 AND fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS) < 0.57
   AND fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) >= -0.35 AND fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) < 0.70
;")

;; Add SQUARE UDF
(define LSST_query_021_a
  "SELECT objectId
FROM   Object
WHERE  SQUARE(muRA_PS)+SQUARE(muDecl_PS) >
       SQRT (SQUARE(muRA_PS*muRA_PS_Sigma)+SQUARE(muDecl_PS*muDecl_PS_Sigma))
;")

;; No MovingObject in PT12
(define LSST_query_021_b
  "SELECT  objectId,
	rowC,colC,rowV,colV,rowVErr,colVErr,
	flags,
	psfMag_u,psfMag_g,psfMag_r,psfMag_i,psfMag_z,
	psfMagErr_u,psfMagErr_g,psfMagErr_r,psfMagErr_i,psfMagErr_z
FROM   MovingObject
WHERE  rowvErr > 0 and colvErr> 0 
  AND ((rowV * rowV) / (rowVErr * rowVErr) + (colV * colV) / (colVErr * colVErr) > 4)
;")

(define LSST_query_022_original
  "  SELECT o1.objectId AS objId1, 
         o2.objectId AS objId2,
         spDist(o1.ra, o1.decl, o2.ra, o2.decl) AS distance
  FROM   Object o1, 
         Object o2
  WHERE  areaSpec_box(:ra1, :decl1, :ra2, :decl2)
    AND  spDist(o1.ra, o1.decl, o2.ra, o2.decl) < :distance
    AND  o1.objectId <> o2.objectId ")

;; ra => ra_PS, decl => decl_PS
;; - spherical_distance
;; - spatial_rectangle
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

(define LSST_query_024
  "SELECT  G.objectId,
        G.uMag,G.gMag,G.rMag,G.iMag,G.zMag,G.yMag 
FROM    Galaxy G
JOIN    _Source2Object M1 ON (G.objectId = M1.objectId)
JOIN    _Source2Object M2 ON (M1.sourceId = M2.sourceId)
JOIN    Star S ON (M2.objectId = S.objectId); ")

(define LSST_query_025
  "SELECT DISTINCT o1.objectId, o1.ra, o1.decl, o2.iauId
FROM   Object o1, Object o2
WHERE  ABS(o2.ra   - o1.ra  ) < o2.raRange/(2*COS(RADIANS(o1.decl)))
   AND ABS(o2.decl - o1.decl) < o2.declRange/2 
   AND (
        SELECT COUNT(o3.objectId)
        FROM   Object o3
        WHERE  o1.objectId <> o3.objectId
          AND  ABS(o1.ra   - o3.ra  ) < 0.1/COS(RADIANS(o3.decl))
          AND  ABS(o1.decl - o3.decl) < 0.1
       ) > 10000;")

;; Not in PT12
(define LSST_query_026
  "SELECT objectId, ra, decl, uMag
FROM   Object
WHERE  (muRA > 0.5 OR muDecl > 0.5)
   AND rMag > 18.0
   AND gMag-rMag < 0.2
   AND extendedParam < 0.2")

;; Not in PT12
(define LSST_query_027
  "SELECT objectId
FROM   Object
WHERE  ABS(muRa)   > 1 * muRaSigma 
    OR ABS(muDecl) > 1 * muDeclSigma;")

;; Broken query (parsing should fail)
(define LSST_query_28_buggee
  "SELECT G.objectId, COUNT(N.NeighborObjID)  AS pop
FROM Galaxy AS G
JOIN Neighbors AS N ON  (G.objectId = N.objectId) 
JOIN Galaxy AS U
ON (U.objectId = N.neighborObjId)  
JOIN photoZ AS Gpz ON (G.objectId
= Gpz.objectId)  
JOIN photoZ AS  Npz ON (U.objectId  = Npz.objectId)
WHERE G.ra BETWEEN  190 AND 200
 AND G.decl BETWEEN  -5 AND 5
 AND
N.objectId  <  N.neighborObjID
  AND  ABS(Gpz.Z  -  Npz.Z)  <  0.05
AND (G.flags & @binned) > 0  
 AND (G.flags & ( @blended + @noDeBlend
+ @child)) !=  @blended
 AND (G.flags & (@edge +  @saturated)) = 0 
AND G.petroMag_i >  17.5
 AND (G.petroMag_r > 15.5  OR G.petroR50_r > 2)
 AND (G.petroMag_r < 30 AND G.g <  30 AND G.r < 30 AND G.i < 30)
AND (G.rMag < 19.2
 AND ( 1=1 or
(G.rMag < (13.1 + (7/3)*G.grColor +
4 *(G.riColor  - 0.18  )) 
AND ((  G.riColor - G.grColor/4  - 0.18)
BETWEEN  -0.2 AND 0.2  )
AND ((G.rMag  + 
2.5  * LOG(  2 *  3.1415 *
G.petroR50_r *  G.petroR50_r )) < 24.2  ) 
) OR
((G.rMag  < 19.5 )
AND ((  G.riColor - G.grColor/4  -.18) >
 AND  (G.grColor > (  1.35 +
0.25  *  G.riColor ))
  AND  ((G.rMag  + --  petSB  -  deRed_r +  2.5
log10(2Pi*petroR50^2)  
2.5  *  LOG(  2  * 3.1415  *  G.petroR50_r  *
G.petroR50_r )) < 23.3 ))
)
 )
GROUP BY G.objectId")

;; Not in PT12
(define LSST_query_029
  "SELECT o.ra, o.decl, o.flags, o.type, o.objid,
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
LIMIT 100 ;")

;; Not in PT12
(define LSST_query_031
  "SELECT *
FROM   Object
WHERE  parallax BETWEEN :parallaxMin AND :parallaxMax")

;; Not in PT12
(define LSST_query_032
  "SELECT objectId
FROM   Object
WHERE  fluxToAbMag(rMag) < :rMagMax
  AND  extendedness > 0.8  
")

;; Not in PT12
(define LSST_query_034
  "SELECT COUNT(*)
FROM  Galaxy    g1
JOIN  Neighbors n  USING (objectId)
JOIN  Galaxy    g2 ON (g2.objectId = N.NeighborObjID)
WHERE g1.objectId < g2.objectId
   AND N.NeighborType = 3
   AND g1.petrorad_u > 0 AND g2.petrorad_u > 0
   AND g1.petrorad_g > 0 AND g2.petrorad_g > 0
   AND g1.petrorad_r > 0 AND g2.petrorad_r > 0
   AND g1.petrorad_i > 0 AND g2.petrorad_i > 0
   AND g1.petrorad_z > 0 AND g2.petrorad_z > 0
   AND g1.petroradErr_g > 0 AND g2.petroradErr_g > 0
   AND g1.petroMag_g BETWEEN 16 AND 21
   AND g2.petroMag_g BETWEEN 16 AND 21
   AND g1.uMag > -9999
   AND g1.gMag > -9999
   AND g1.rMag > -9999
   AND g1.iMag > -9999
   AND g1.zMag > -9999
   AND g1.yMag > -9999
   AND g2.uMag > -9999
   AND g2.gMag > -9999
   AND g2.rMag > -9999
   AND g2.iMag > -9999
   AND g2.zMag > -9999
   AND g2.yMag > -9999
   AND abs(g1.gMag - g2.gMag) > 3
   AND (g1.petroR50_r BETWEEN 0.25*g2.petroR50_r AND 4.0*g2.petroR50_r)
   AND (g2.petroR50_r BETWEEN 0.25*g1.petroR50_r AND 4.0*g1.petroR50_r)
   AND (n.distance <= (g1.petroR50_r + g2.petroR50_r))
")

;; Not in PT12
(define LSST_query_035
  "SELECT  objectId
FROM    Object
WHERE 	uMag-gMag < 0.4
   AND  gMag-rMag < 0.7	
   AND  rMag-iMag > 0.4
   AND  iMag-zMag > 0.4
")

;; Magnitude = -2.5 * log10(flux) - 48.6
(define LSST_query_035_b
  "SELECT  objectId
FROM    Object
WHERE   uFlux_PS > 0.
   AND  gFlux_PS > 0.
   AND  rFlux_PS > 0.
   AND  iFlux_PS > 0.
   AND  zFlux_PS > 0.
   AND  fluxToAbMag(uFlux_PS) - fluxToAbMag(gFlux_PS) < 0.4
   AND  fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) < 0.7	
   AND  fluxToAbMag(rFlux_PS) - fluxToAbMag(iFlux_PS) > 0.4
   AND  fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) > 0.4 ;
")

(define LSST_query_036
  "SELECT  ROUND(uMag-gMag,0) AS UG, 
        ROUND(gMag-rMag,0) AS GR, 
        ROUND(rMag-iMag,0) AS RI, 
        ROUND(iMag-zMag,0) AS IZ,
        ROUND(zMag-yMag,0) AS ZY,
	COUNT(*) AS pop
FROM    Object
WHERE   extendedParam < 0.2
  AND   (uMag+gMag+rMag+iMag+zMag+yMag) < 150 
GROUP BY UG, GR, RI, IZ, ZY
HAVING pop > 500
ORDER BY pop")

(define LSST_query_037
  "SELECT objectId, raSG, declSG, 
       uFluxSG, gFluxSG, rFluxSG, iFluxSG, zFluxSG, yFluxSG
FROM   Object
WHERE  iFluxSG-zFluxSG > 1.0
  AND  extendedParam > 0.8")

(define LSST_query_038
  "SELECT  objectId
FROM    Object   
WHERE   extendedness > 0.8 
  AND   fluxToAbMag(rFlux_SG) < 24 
  AND   rRadius_SG BETWEEN 30 AND 60 
  AND   (POWER(rE1_SG,2) + POWER(uE1_SG,2)) > 0.25 
;")

(define LSST_query_039
  "SELECT o.ra, o.decl
FROM MovingObject o
WHERE Distance(o.position,orbit) < :distance ;")

(define LSST_query_040
  "SELECT  objectId,
	SQRT( POWER(rowv,2) + POWER(colv, 2) ) AS velocity
FROM    MovingObject                                      
WHERE  (POWER(rowv,2) + POWER(colv, 2)) BETWEEN 50 AND 1000
;")

(define LSST_query_043
  "SELECT  S1.objectId AS s1, 
        S2.objectId AS s2
FROM    Object S1,                                   
        Object S2                                    
WHERE   S1.extendedParam < 0.2                       
   AND  S2.extendedParam < 0.2                       
   AND  spDist(S1.ra, S1.decl, S2.ra, S2.decl) < .05 
   AND  S1.uMag-S1.gMag < 0.4                        
   AND  S1.gMag-S1.rMag < 0.7
   AND  S1.rMag-S1.iMag > 0.4 
   AND  S1.iMag-S1.zMag > 0.4
;")

;; TODO: uMag  etc.
(define LSST_query_043_adapted
  "SELECT  S1.objectId AS s1, 
        S2.objectId AS s2
FROM    Object S1,                                   
        Object S2                                    
WHERE   cone_search(S1.ra, S1.decl, S2.ra, S2.decl, .05)
   AND  S1.uMag-S1.gMag < 0.4                        
   AND  S1.gMag-S1.rMag < 0.7
   AND  S1.rMag-S1.iMag > 0.4 
   AND  S1.iMag-S1.zMag > 0.4
;")

(define LSST_query_044
  "SELECT v.objectId, v.ra, v.decl, 
       s.objectId, s.ra, s.decl
FROM   Object v, Object s
WHERE  v.variability > 0.8  
   AND s.extendedParam < 0.2 
   AND v.objectId <> s.objectId
   AND spDist(v.ra, v.decl, s.ra, s.decl) < :distance
   AND (s.gMag-s.rMag < 0.0 OR 
        s.rMag-s.iMag < 0.0 OR
        s.uMag-s.gMag < 1.0) ")

(define LSST_query_045
  "SELECT objectId
FROM   Object
WHERE  uVarProb = 100
   OR  gVarProb = 100
   OR  rVarProb = 100
   OR  iVarProb = 100
   OR  zVarProb = 100
   OR  yVarProb = 100
  AND  extendedParam > 0.8") 

(define LSST_query_046
  "SELECT o.objectId
FROM   BRG
JOIN   Neighbors n USING (objectId)
JOIN   Object o ON (n.neighborId = o.objectId)
WHERE  n.radius < :distance
  AND  Ellipticity(o.fwhmA, o.fwhmB) > :ellipticity
  AND  o.fwhmTheta BETWEEN :fwhmMin AND :fwhmMax ;")

(define LSST_query_047
  "SELECT v.objectId
FROM   Object g, Object v
WHERE  g.extendedParam > 0.8 
  AND  v.variability > 0.8   
  AND  spDist(g.ra, g.decl, v.ra, v.decl) < :distance")

(define LSST_query_050
  "SELECT COUNT(movingObjectId) 
FROM MovingObject 
WHERE q < 5.2             
  AND q*(1+e)/(1-e) > 9.6 ")

(define LSST_query_051
  "SELECT objectId
FROM   MovingObject
WHERE  rMag-gMag > :threshold
")

(define LSST_query_052
  "SELECT diaSourceId, ra, decl
FROM   DiaSource
WHERE  movingObjectId = :movingObjectId ;")

(define LSST_query_053
  "SELECT parallax, uMag, gMag, rMag, iMag, zMag, yMag, muRA, muDecl,
       uVarProb, gVarProb, rVarProb, iVarProb, zVarProb, yVarProb
FROM   Object
WHERE  areaSpec(10.5, 30.5, 12.2, 32.7)
  AND  extendedParameter < 0.2 ;")

(define LSST_query_053_adapted
  "SELECT *
FROM   Object
WHERE  spatial_rectangle(ra_PS, decl_PS, 1.2, 3.5, 1.5, 3.7);")

(define LSST_query_054
  "SELECT objectId
FROM   Object
WHERE  areaSpec_box(10.5, 30.5, 12.2, 32.7)
  AND  extendedParameter < 0.2
  AND  gMag-rMag > :grThreshold
  AND  (muRa * muRa + muDecl * muDecl) < :properMotionThreshold
  AND  redshift BETWEEN :z1 AND :z2
;")

(define LSST_query_055
  "SELECT COUNT(*)
FROM   Object
WHERE  areaSpec_box(10.5, 30.5, 12.2, 32.7)
  AND  extendedParameter < 0.2 ;")

(define LSST_query_055_adapted
  "SELECT COUNT(*)
FROM   Object
WHERE  spatial_rectangle(ra_PS, decl_PS, 0.5, 3.5, 1.2, 3.7) ;")

(define LSST_query_056_a
  "SELECT * FROM   Source")

(define LSST_query_056_b
  "SELECT * 
FROM   Source
JOIN   Object USING (objectId)
WHERE  extendedParam < 0.2")

(define LSST_query_056_c
  "SELECT * 
FROM   Object
JOIN   VarObject USING (objectId)
JOIN   _Object2Type USING (objectId) 
JOIN   ObjectType   USING (typeId)
WHERE  ObjectType.description = 'star'
;")

(define LSST_query_063
  "SELECT ra, dec,
       jPetroMag-aj as j,
       (jAperMag3-aj)-(kAperMag3-ak) as jmk
FROM   reliableDxsSource
WHERE
      mergedClass NOT BETWEEN -1 AND 0 AND
      jPetroMagErr BETWEEN 0 AND 0.2 AND
      kPetroMagErr BETWEEN 0 AND 0.2
;")

(define LSST_query_068
  "SELECT sce.visit, sce.raftName, sce.ccdName, 
       sro.rMag, sro.ra, sro.decl, sro.isStar, sro.refObjectId,  
       rom.nSrcMatches,
       s.sourceId,s.ra, s.decl, s.xAstrom, s.yAstrom, s.psfFlux,
       s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.flux_ESG,
       s.flux_Gaussian, s.ixx, s.iyy, s.ixy, s.psfIxx, s.psfIxxSigma,
       s.psfIyy, s.psfIyySigma, s.psfIxy, s.psfIxySigma, 
       s.resolution_SG, s.e1_SG, s.e1_SG_Sigma, s.e2_SG, s.e2_SG_Sigma,
       s.shear1_SG, s.shear1_SG_Sigma, s.shear2_SG, s.shear2_SG_Sigma,
       s.sourceWidth_SG,s.sourceWidth_SG_Sigma,s.flagForDetection  
FROM   Source AS s, 
       Science_Ccd_Exposure AS sce,    
       RefSrcMatch AS rom, 
       SimRefObject AS sro  
WHERE (s.scienceCcdExposureId = sce.scienceCcdExposureId)
  AND (s.sourceId = rom.sourceId)
  AND (rom.refObjectId = sro.refObjectId)
  AND (sce.visit = 925692311)
  AND (sce.raftName = '3,0') and (sce.ccdName = '0,2')
;")

(define LSST_query_069
  "SELECT o.objectId, 
       sro.ra, sro.decl, sro.gLat, sro.gLon, sro.sedname, 
       sro.uMag, sro.gMag, sro.rMag, sro.iMag, sro.zMag, sro.yMag, 
       sro.muRa, sro.muDecl, sro.parallax, sro.vRad, sro.varClass, 
       (sro.refObjectId-1)/2%pow(2,10) typeId 
FROM   Object o 
JOIN   RefObjMatch rom USING (objectId) 
JOIN   SimRefObject sro USING (refObjectId)
WHERE  isStar =1 and rMag < 25
;")

(define LSST_query_070
  "SELECT
    o.uFlux_PS, o.uFlux_PS_Sigma, o.gFlux_PS, o.gFlux_PS_Sigma,
    o.rFlux_PS, o.rFlux_PS_Sigma, o.iFlux_PS, o.iFlux_PS_Sigma,
    o.zFlux_PS, o.zFlux_PS_Sigma, o.yFlux_PS, o.yFlux_PS_Sigma,
    ce.visit, ce.raft, ce.ccd,
    f.filterName,
    scisql_dnToAbMag(fs.flux, ce.fluxMag0) AS forcedPsfMag,
    scisql_dnToAbMagSigma(fs.flux, fs.fluxSigma, ce.fluxMag0, ce.fluxMag0Sigma) AS forcedPsfMagErr,
    so.uMag, so.gMag, so.rMag, so.iMag, so.zMag, so.yMag

FROM
    Object                    AS o
    JOIN ForcedSource         AS fs ON (fs.objectId = o.objectId)
    JOIN Science_Ccd_Exposure AS ce ON (ce.scienceCcdExposureId = s.scienceCcdExposureId)
    JOIN RefObjMatch          AS om ON (om.objectId = o.objectId)
    JOIN SimRefObject         AS so ON (om.refObjectId = so.refObjectId)
    JOIN Filter               AS f  ON (f.filterId = ce.filterId)

WHERE
     fs.fluxSigma > 0
 AND om.nObjMatches = 1
 AND NOT (fs.flagNegative | fs.flagPixEdge | fs.flagPixSaturAny | 
          fs.flagPixSaturCen | fs.flagBadApFlux | fs.flagBadPsfFlux);")

(define LSST_tables-description
  (make-schema  ;; From PT12
   ("Filter" (columns "Filter" '("filterId" "filterName" "photClam" "photBW" )))
   
   ("Object"
    (columns
     "Object"
     '("objectId" "iauId" "ra_PS" "ra_PS_Sigma" "decl_PS" "decl_PS_Sigma"
       "radecl_PS_Cov"    "htmId20"     "ra_SG"    "ra_SG_Sigma"    "decl_SG"
       "decl_SG_Sigma"   "radecl_SG_Cov"   "raRange"  "declRange"   "muRa_PS"
       "muRa_PS_Sigma"    "muDecl_PS"   "muDecl_PS_Sigma"   "muRaDecl_PS_Cov"
       "parallax_PS"  "parallax_PS_Sigma"  "canonicalFilterId" "extendedness"
       "varProb"  "earliestObsTime"   "latestObsTime"  "meanObsTime"  "flags"
       "uNumObs"        "uExtendedness"       "uVarProb"       "uRaOffset_PS"
       "uRaOffset_PS_Sigma"      "uDeclOffset_PS"      "uDeclOffset_PS_Sigma"
       "uRaDeclOffset_PS_Cov"       "uRaOffset_SG"       "uRaOffset_SG_Sigma"
       "uDeclOffset_SG"     "uDeclOffset_SG_Sigma"     "uRaDeclOffset_SG_Cov"
       "uLnL_PS"    "uLnL_SG"    "uFlux_PS"   "uFlux_PS_Sigma"    "uFlux_ESG"
       "uFlux_ESG_Sigma" "uFlux_Gaussian" "uFlux_Gaussian_Sigma" "uTimescale"
       "uEarliestObsTime"  "uLatestObsTime" "uSersicN_SG" "uSersicN_SG_Sigma"
       "uE1_SG"    "uE1_SG_Sigma"   "uE2_SG"    "uE2_SG_Sigma"   "uRadius_SG"
       "uRadius_SG_Sigma"   "uFlags"  "gNumObs"   "gExtendedness"  "gVarProb"
       "gRaOffset_PS"          "gRaOffset_PS_Sigma"          "gDeclOffset_PS"
       "gDeclOffset_PS_Sigma"      "gRaDeclOffset_PS_Cov"      "gRaOffset_SG"
       "gRaOffset_SG_Sigma"      "gDeclOffset_SG"      "gDeclOffset_SG_Sigma"
       "gRaDeclOffset_SG_Cov" "gLnL_PS" "gLnL_SG" "gFlux_PS" "gFlux_PS_Sigma"
       "gFlux_ESG"  "gFlux_ESG_Sigma" "gFlux_Gaussian" "gFlux_Gaussian_Sigma"
       "gTimescale"    "gEarliestObsTime"    "gLatestObsTime"   "gSersicN_SG"
       "gSersicN_SG_Sigma"  "gE1_SG"  "gE1_SG_Sigma" "gE2_SG"  "gE2_SG_Sigma"
       "gRadius_SG"  "gRadius_SG_Sigma"  "gFlags"  "rNumObs"  "rExtendedness"
       "rVarProb"    "rRaOffset_PS"   "rRaOffset_PS_Sigma"   "rDeclOffset_PS"
       "rDeclOffset_PS_Sigma"      "rRaDeclOffset_PS_Cov"      "rRaOffset_SG"
       "rRaOffset_SG_Sigma"      "rDeclOffset_SG"      "rDeclOffset_SG_Sigma"
       "rRaDeclOffset_SG_Cov" "rLnL_PS" "rLnL_SG" "rFlux_PS" "rFlux_PS_Sigma"
       "rFlux_ESG"  "rFlux_ESG_Sigma" "rFlux_Gaussian" "rFlux_Gaussian_Sigma"
       "rTimescale"    "rEarliestObsTime"    "rLatestObsTime"   "rSersicN_SG"
       "rSersicN_SG_Sigma"  "rE1_SG"  "rE1_SG_Sigma" "rE2_SG"  "rE2_SG_Sigma"
       "rRadius_SG"  "rRadius_SG_Sigma"  "rFlags"  "iNumObs"  "iExtendedness"
       "iVarProb"    "iRaOffset_PS"   "iRaOffset_PS_Sigma"   "iDeclOffset_PS"
       "iDeclOffset_PS_Sigma"      "iRaDeclOffset_PS_Cov"      "iRaOffset_SG"
       "iRaOffset_SG_Sigma"      "iDeclOffset_SG"      "iDeclOffset_SG_Sigma"
       "iRaDeclOffset_SG_Cov" "iLnL_PS" "iLnL_SG" "iFlux_PS" "iFlux_PS_Sigma"
       "iFlux_ESG"  "iFlux_ESG_Sigma" "iFlux_Gaussian" "iFlux_Gaussian_Sigma"
       "iTimescale"    "iEarliestObsTime"    "iLatestObsTime"   "iSersicN_SG"
       "iSersicN_SG_Sigma"  "iE1_SG"  "iE1_SG_Sigma" "iE2_SG"  "iE2_SG_Sigma"
       "iRadius_SG"  "iRadius_SG_Sigma"  "iFlags"  "zNumObs"  "zExtendedness"
       "zVarProb"    "zRaOffset_PS"   "zRaOffset_PS_Sigma"   "zDeclOffset_PS"
       "zDeclOffset_PS_Sigma"      "zRaDeclOffset_PS_Cov"      "zRaOffset_SG"
       "zRaOffset_SG_Sigma"      "zDeclOffset_SG"      "zDeclOffset_SG_Sigma"
       "zRaDeclOffset_SG_Cov" "zLnL_PS" "zLnL_SG" "zFlux_PS" "zFlux_PS_Sigma"
       "zFlux_ESG"  "zFlux_ESG_Sigma" "zFlux_Gaussian" "zFlux_Gaussian_Sigma"
       "zTimescale"    "zEarliestObsTime"    "zLatestObsTime"   "zSersicN_SG"
       "zSersicN_SG_Sigma"  "zE1_SG"  "zE1_SG_Sigma" "zE2_SG"  "zE2_SG_Sigma"
       "zRadius_SG"  "zRadius_SG_Sigma"  "zFlags"  "yNumObs"  "yExtendedness"
       "yVarProb"    "yRaOffset_PS"   "yRaOffset_PS_Sigma"   "yDeclOffset_PS"
       "yDeclOffset_PS_Sigma"      "yRaDeclOffset_PS_Cov"      "yRaOffset_SG"
       "yRaOffset_SG_Sigma"      "yDeclOffset_SG"      "yDeclOffset_SG_Sigma"
       "yRaDeclOffset_SG_Cov" "yLnL_PS" "yLnL_SG" "yFlux_PS" "yFlux_PS_Sigma"
       "yFlux_ESG"  "yFlux_ESG_Sigma" "yFlux_Gaussian" "yFlux_Gaussian_Sigma"
       "yTimescale"    "yEarliestObsTime"    "yLatestObsTime"   "ySersicN_SG"
       "ySersicN_SG_Sigma"  "yE1_SG"  "yE1_SG_Sigma" "yE2_SG"  "yE2_SG_Sigma"
       "yRadius_SG" "yRadius_SG_Sigma" "yFlags" "chunkId" "subChunkId" )))
         
   ("Source"
    (columns
     "Source"
     '("sourceId" "scienceCcdExposureId" "filterId" "objectId"
       "movingObjectId"     "procHistoryId"     "ra"    "raSigmaForDetection"
       "raSigmaForWcs"   "decl"   "declSigmaForDetection"   "declSigmaForWcs"
       "htmId20"   "xFlux"   "xFluxSigma"   "yFlux"   "yFluxSigma"   "raFlux"
       "raFluxSigma"  "declFlux"  "declFluxSigma"  "xPeak"  "yPeak"  "raPeak"
       "declPeak"    "xAstrom"   "xAstromSigma"    "yAstrom"   "yAstromSigma"
       "raAstrom"  "raAstromSigma" "declAstrom"  "declAstromSigma" "raObject"
       "declObject"   "taiMidPoint"   "taiRange"   "psfFlux"   "psfFluxSigma"
       "apFlux"   "apFluxSigma"   "modelFlux"  "modelFluxSigma"   "petroFlux"
       "petroFluxSigma"    "instFlux"    "instFluxSigma"    "nonGrayCorrFlux"
       "nonGrayCorrFluxSigma" "atmCorrFlux"  "atmCorrFluxSigma" "apDia" "Ixx"
       "IxxSigma"  "Iyy" "IyySigma"  "Ixy" "IxySigma"  "psfIxx" "psfIxxSigma"
       "psfIyy"  "psfIyySigma" "psfIxy"  "psfIxySigma"  "e1_SG" "e1_SG_Sigma"
       "e2_SG"  "e2_SG_Sigma"  "resolution_SG" "shear1_SG"  "shear1_SG_Sigma"
       "shear2_SG"  "shear2_SG_Sigma" "sourceWidth_SG" "sourceWidth_SG_Sigma"
       "shapeFlag_SG"   "snr"    "chi2"   "sky"   "skySigma"   "extendedness"
       "flux_Gaussian"   "flux_Gaussian_Sigma"   "flux_ESG"  "flux_ESG_Sigma"
       "sersicN_SG"    "sersicN_SG_Sigma"    "radius_SG"    "radius_SG_Sigma"
       "flux_flux_SG_Cov"          "flux_e1_SG_Cov"          "flux_e2_SG_Cov"
       "flux_radius_SG_Cov"        "flux_sersicN_SG_Cov"       "e1_e1_SG_Cov"
       "e1_e2_SG_Cov"  "e1_radius_SG_Cov"  "e1_sersicN_SG_Cov" "e2_e2_SG_Cov"
       "e2_radius_SG_Cov"      "e2_sersicN_SG_Cov"     "radius_radius_SG_Cov"
       "radius_sersicN_SG_Cov"  "sersicN_sersicN_SG_Cov" "flagForAssociation"
       "flagForDetection" "flagForWcs" )))))

(define (LSST_table->fields database table)
  (and (or (eq? database 'unknown)
           (string=? database "LSST"))
       (let ((tmp (table-ref LSST_tables-description table #f)))
         (and tmp
              (let ((table-name (columns->name tmp)))
                (map (lambda (x) (field table-name x))
                     (columns->fields tmp)))))))

;; ---------------------------------------- ;;

;; qserv_areaspec_box  is  a  strange  UDF  as  it
;; should  expand with the  corresponding geometry
;; fields.
(define LSST_query_bonus_001
  "SELECT COUNT(*) FROM LSST.Object o1, LSST.Object o2 
WHERE qserv_areaspec_box(-11,-11,-1,-1) 
AND scisql_angSep(o1.ra_PS,o1.decl_PS, o2.ra_PS, o2.decl_PS) < 0.1;")

;; From https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013

(define LSST_query_bonus_002
  "SELECT COUNT(*)
 FROM Object
 WHERE gFlux_PS>1e-25;")

(define LSST_query_bonus_003
  "SELECT count(*) 
 FROM   Object 
 WHERE  qserv_areaspec_box(0,0.1,0.2,0.3)   
  AND scisql_fluxToAbMag(zFlux_PS)
      BETWEEN 21 AND 21.5  
  AND scisql_fluxToAbMag(gFlux_PS)
      - scisql_fluxToAbMag(rFlux_PS) 
      BETWEEN 0.3 AND 0.4  
  AND scisql_fluxToAbMag(iFlux_PS)
      - scisql_fluxToAbMag(zFlux_PS) 
      BETWEEN 0.1 AND 0.12;
")

(define LSST_query_bonus_004
  "SELECT objectId, ra_PS, decl_PS,
        uFlux_PS, gFlux_PS, rFlux_PS, 
        iFlux_PS, zFlux_PS, yFlux_PS 
 FROM Object
 WHERE scisql_fluxToAbMag(iFlux_PS)
       - scisql_fluxToAbMag(zFlux_PS) > 4;")

(define LSST_query_bonus_005
  "SELECT count(*) 
FROM Object
WHERE qserv_areaspec_box(1,3,2,4) 
  AND scisql_fluxToAbMag(zFlux_PS) BETWEEN 21 and 21.5;")

;; TODO
(define LSST_query_bonus_006
  "SELECT count(*) AS n,
        AVG(ra_PS),
        AVG(decl_PS),
        chunkId 
 FROM Object 
 GROUP BY chunkId;")

;; TODO: Use UDF or translate scisql_fluxToAbMag
(define LSST_query_bonus_007
  "SELECT objectId, ra_PS, decl_PS, 
        scisql_fluxToAbMag(zFlux_PS) 
 FROM LSST.Object 
 WHERE scisql_fluxToAbMag(zFlux_PS)
       BETWEEN 20 AND 24;")

;; TODO
(define LSST_query_bonus_008
  "SELECT count(*)
 FROM Object
 WHERE scisql_angSep(ra_PS, decl_PS, 0., 0.) < 0.2;")

;; KO: use either Geometry or distributivity over shards
(define LSST_query_bonus_009
  "SELECT objectId 
FROM Source 
JOIN Object USING(objectId) 
WHERE qserv_areaspec_box(.1, 0, .3, .2) ;")

;; Ok (Time: 116.349 ms with PT12 without Geometry
;; table  and special  treatment  for Object  JOIN
;; Source)
(define LSST_query_bonus_010 
  "SELECT s.ra, s.decl
 FROM   Object o
 JOIN   Source s
 USING (objectId)
 WHERE  o.objectId = 390034570102582
 AND    o.latestObsTime = s.taiMidPoint;")

;; Ok  (Time:   2020.585  ms  with   PT12  without
;; Geometry table and special treatment for Object
;; JOIN Source) 
(define LSST_query_bonus_011
  "SELECT o1.objectId AS objId1, 
        o2.objectId AS objId2,
        scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) AS distance
 FROM   Object o1, 
        Object o2
 WHERE o1.ra_PS BETWEEN 0 AND 0.02
   AND o2.ra_PS BETWEEN 0 AND 0.02
   AND o1.decl_PS BETWEEN 0.01 AND 0.03
   AND o2.decl_PS BETWEEN 0.01 AND 0.03
   AND o1.objectId <> o2.objectId ;")

(define LSST_query_bonus_012
  "SELECT min(ra_PS),max(ra_PS),min(decl_PS),max(decl_PS)
   FROM Object ;")

;; Ok (Time: 7770.182 ms with PT12 and without Geometry table)
(define LSST_query_bonus_013 
  "SELECT COUNT(*)
   FROM Object
   WHERE ra_PS BETWEEN 0.0 AND 1.0
   AND decl_PS BETWEEN 2.0 AND 3.0 ;")

;; Search all sources in an area with distance less than 10 arc seconds.
(define LSST_query_bonus_014
  "SELECT * from Source S1, Source S2
   WHERE cone_search(S1.ra,S1.decl,S2.ra,S2.decl,0.00005)
     AND spatial_rectangle(S1.ra, S1.decl, 1.8, -4.2, 2.1, -3.8)
     AND S1.sourceid <> S2.sourceid ;")

;; Same as bonus 14 but with a spatial join
(define LSST_query_bonus_014_with_spatial_join
  "SELECT * from Source S1, Source S2
   WHERE spatial_join(S1.ra,S1.decl,S2.ra,S2.decl,0.00005)
     AND spatial_rectangle(S1.ra, S1.decl, 1.8, -4.2, 2.1, -3.8)
     AND S1.sourceid <> S2.sourceid ;")


;; ---- ;;    

(define LSST_queries
  (list LSST_query_001_a LSST_query_001_b LSST_query_001_c
        LSST_query_002 LSST_query_003 LSST_query_004
        LSST_query_005 LSST_query_006 LSST_query_007
        LSST_query_008 LSST_query_009 LSST_query_010
        LSST_query_011 LSST_query_012 LSST_query_013
        LSST_query_014 LSST_query_015
        LSST_query_018 LSST_query_019
        LSST_query_020 LSST_query_021_a LSST_query_021_b
        LSST_query_022 LSST_query_024 LSST_query_025
        LSST_query_026 LSST_query_027 LSST_query_029
        LSST_query_031 LSST_query_032 LSST_query_034
        LSST_query_035 LSST_query_035_b
        LSST_query_036 LSST_query_037
        LSST_query_038 LSST_query_039 LSST_query_040
        LSST_query_043 LSST_query_044 LSST_query_045
        LSST_query_046 LSST_query_047 LSST_query_050
        LSST_query_051 LSST_query_052 LSST_query_053
        LSST_query_054 LSST_query_055
        LSST_query_056_a LSST_query_056_b LSST_query_056_c
        LSST_query_063 LSST_query_068 LSST_query_069
        LSST_query_070 
        LSST_query_bonus_001
        LSST_query_bonus_002
        LSST_query_bonus_003
        LSST_query_bonus_004
        LSST_query_bonus_005
        LSST_query_bonus_006
        LSST_query_bonus_007
        LSST_query_bonus_008
        LSST_query_bonus_009
        LSST_query_bonus_010
        LSST_query_bonus_011
        LSST_query_bonus_012
        LSST_query_bonus_013
        LSST_query_bonus_014
        ))

(define LSST_supported__PT12_queries
  (list LSST_query_001_a LSST_query_001_b LSST_query_001_c
        LSST_query_003 LSST_query_004
        LSST_query_006 LSST_query_007
        LSST_query_008 LSST_query_009 LSST_query_010
        LSST_query_011 LSST_query_013
        LSST_query_014 LSST_query_015
        LSST_query_018 LSST_query_019
        LSST_query_020 LSST_query_021_a LSST_query_021_b
        LSST_query_022 LSST_query_024 LSST_query_025
        LSST_query_026 LSST_query_027
        LSST_query_031 LSST_query_032
        LSST_query_035 LSST_query_035_b
        LSST_query_036 LSST_query_037
        LSST_query_038 LSST_query_039 LSST_query_040
        LSST_query_043 LSST_query_044 LSST_query_045
        LSST_query_047 LSST_query_050
        LSST_query_051 LSST_query_052 LSST_query_053
        LSST_query_054 LSST_query_055
        LSST_query_056_a LSST_query_056_b 
        LSST_query_bonus_001
        LSST_query_bonus_002
        LSST_query_bonus_003
        LSST_query_bonus_004
        LSST_query_bonus_005
        LSST_query_bonus_006
        LSST_query_bonus_007
        LSST_query_bonus_008
        LSST_query_bonus_009
        LSST_query_bonus_010
        LSST_query_bonus_011
        LSST_query_bonus_012
        LSST_query_bonus_013
        LSST_query_bonus_014
        ))

;; Use time with gambit to estimate computing time
(define (LSST_queries:example)
  (for-each
   (lambda (str)
     (pretty-print (SQL->AST str))
     (newline)
     (newline))
   LSST_queries))

(define (LSST_queries:example-without-printing)
  (display "Converting ") (display (length LSST_queries))
  (display " LSST standard queries to AST.") (newline)
  (for-each SQL->AST LSST_queries))

;; ---------------------------------------- ;;

(define (LSST_queries::test)
  (map (lambda (q)
         (pp q) (newline)
         (explain-rewriter q))
       LSST_supported__PT12_queries))


