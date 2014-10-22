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



;; (time (for-each (lambda (str) (AST->SQL (SQL->AST str)))  QSERV_queries))

 (define case01_0001_fetchObjectById "
-- Find an object with a particular object id
-- http://dev.lsstcorp.org/trac/wiki/dbQuery009

-- note that the data for mysql version is specially
-- precooked for this object to include chunkId and
-- subchunkId

SELECT objectId,iauId,ra_PS,ra_PS_Sigma,decl_PS,decl_PS_Sigma,radecl_PS_Cov,htmId20,ra_SG,ra_SG_Sigma,decl_SG,decl_SG_Sigma,radecl_SG_Cov,raRange,declRange,muRa_PS,muRa_PS_Sigma,muDecl_PS,muDecl_PS_Sigma,muRaDecl_PS_Cov,parallax_PS,parallax_PS_Sigma,canonicalFilterId,extendedness,varProb,earliestObsTime,latestObsTime,meanObsTime,flags,uNumObs,uExtendedness,uVarProb,uRaOffset_PS,uRaOffset_PS_Sigma,uDeclOffset_PS,uDeclOffset_PS_Sigma,uRaDeclOffset_PS_Cov,uRaOffset_SG,uRaOffset_SG_Sigma,uDeclOffset_SG,uDeclOffset_SG_Sigma,uRaDeclOffset_SG_Cov,uLnL_PS,uLnL_SG,uFlux_PS,uFlux_PS_Sigma,uFlux_ESG,uFlux_ESG_Sigma,uFlux_Gaussian,uFlux_Gaussian_Sigma,uTimescale,uEarliestObsTime,uLatestObsTime,uSersicN_SG,uSersicN_SG_Sigma,uE1_SG,uE1_SG_Sigma,uE2_SG,uE2_SG_Sigma,uRadius_SG,uRadius_SG_Sigma,uFlags,gNumObs,gExtendedness,gVarProb,gRaOffset_PS,gRaOffset_PS_Sigma,gDeclOffset_PS,gDeclOffset_PS_Sigma,gRaDeclOffset_PS_Cov,gRaOffset_SG,gRaOffset_SG_Sigma,gDeclOffset_SG,gDeclOffset_SG_Sigma,gRaDeclOffset_SG_Cov,gLnL_PS,gLnL_SG,gFlux_PS,gFlux_PS_Sigma,gFlux_ESG,gFlux_ESG_Sigma,gFlux_Gaussian,gFlux_Gaussian_Sigma,gTimescale,gEarliestObsTime,gLatestObsTime,gSersicN_SG,gSersicN_SG_Sigma,gE1_SG,gE1_SG_Sigma,gE2_SG,gE2_SG_Sigma,gRadius_SG,gRadius_SG_Sigma,gFlags,rNumObs,rExtendedness,rVarProb,rRaOffset_PS,rRaOffset_PS_Sigma,rDeclOffset_PS,rDeclOffset_PS_Sigma,rRaDeclOffset_PS_Cov,rRaOffset_SG,rRaOffset_SG_Sigma,rDeclOffset_SG,rDeclOffset_SG_Sigma,rRaDeclOffset_SG_Cov,rLnL_PS,rLnL_SG,rFlux_PS,rFlux_PS_Sigma,rFlux_ESG,rFlux_ESG_Sigma,rFlux_Gaussian,rFlux_Gaussian_Sigma,rTimescale,rEarliestObsTime,rLatestObsTime,rSersicN_SG,rSersicN_SG_Sigma,rE1_SG,rE1_SG_Sigma,rE2_SG,rE2_SG_Sigma,rRadius_SG,rRadius_SG_Sigma,rFlags,iNumObs,iExtendedness,iVarProb,iRaOffset_PS,iRaOffset_PS_Sigma,iDeclOffset_PS,iDeclOffset_PS_Sigma,iRaDeclOffset_PS_Cov,iRaOffset_SG,iRaOffset_SG_Sigma,iDeclOffset_SG,iDeclOffset_SG_Sigma,iRaDeclOffset_SG_Cov,iLnL_PS,iLnL_SG,iFlux_PS,iFlux_PS_Sigma,iFlux_ESG,iFlux_ESG_Sigma,iFlux_Gaussian,iFlux_Gaussian_Sigma,iTimescale,iEarliestObsTime,iLatestObsTime,iSersicN_SG,iSersicN_SG_Sigma,iE1_SG,iE1_SG_Sigma,iE2_SG,iE2_SG_Sigma,iRadius_SG,iRadius_SG_Sigma,iFlags,zNumObs,zExtendedness,zVarProb,zRaOffset_PS,zRaOffset_PS_Sigma,zDeclOffset_PS,zDeclOffset_PS_Sigma,zRaDeclOffset_PS_Cov,zRaOffset_SG,zRaOffset_SG_Sigma,zDeclOffset_SG,zDeclOffset_SG_Sigma,zRaDeclOffset_SG_Cov,zLnL_PS,zLnL_SG,zFlux_PS,zFlux_PS_Sigma,zFlux_ESG,zFlux_ESG_Sigma,zFlux_Gaussian,zFlux_Gaussian_Sigma,zTimescale,zEarliestObsTime,zLatestObsTime,zSersicN_SG,zSersicN_SG_Sigma,zE1_SG,zE1_SG_Sigma,zE2_SG,zE2_SG_Sigma,zRadius_SG,zRadius_SG_Sigma,zFlags,yNumObs,yExtendedness,yVarProb,yRaOffset_PS,yRaOffset_PS_Sigma,yDeclOffset_PS,yDeclOffset_PS_Sigma,yRaDeclOffset_PS_Cov,yRaOffset_SG,yRaOffset_SG_Sigma,yDeclOffset_SG,yDeclOffset_SG_Sigma,yRaDeclOffset_SG_Cov,yLnL_PS,yLnL_SG,yFlux_PS,yFlux_PS_Sigma,yFlux_ESG,yFlux_ESG_Sigma,yFlux_Gaussian,yFlux_Gaussian_Sigma,yTimescale,yEarliestObsTime,yLatestObsTime,ySersicN_SG,ySersicN_SG_Sigma,yE1_SG,yE1_SG_Sigma,yE2_SG,yE2_SG_Sigma,yRadius_SG,yRadius_SG_Sigma,yFlags 
FROM   Object
WHERE  objectId = 430213989148129
")

 (define case01_0002_fetchObjectByIdNoResult "
-- Find an object with a particular object id
-- http://dev.lsstcorp.org/trac/wiki/dbQuery009

-- not working, see ticket #1847

SELECT *
FROM   Object
WHERE  objectId = 430213989000
")

 (define case01_0003_selectMetadataForOneGalaxy "
-- select the full color image of a single given galaxy
-- http://dev.lsstcorp.org/trac/wiki/dbQuery006

SELECT s.ra, s.decl, o.raRange, o.declRange
FROM   Object o
JOIN   Source s USING (objectId)
WHERE  o.objectId = 390034570102582
AND    o.latestObsTime = s.taiMidPoint
")

 (define case01_0004_lightCurve "
-- Extract light curve for a given object (time, magnitude and position)
-- See http://dev.lsstcorp.org/trac/wiki/dbQuery001

SELECT taiMidPoint, psfFlux, psfFluxSigma, ra, decl
FROM   Source
JOIN   Filter USING (filterId)
WHERE  objectId = 402412665835716
   AND filterName = 'r'
")

 (define case01_0005_nonReplicatedTable "
-- trivial query, should return one row (some cleverness
-- is needed to execute this query on one node only!)

SELECT offset, mjdRef, drift FROM LeapSeconds where offset = 10
")

 (define case01_0006_transientVarObjNearGalaxy "
-- Select transient variable objects near a known galaxy
-- http://dev.lsstcorp.org/trac/wiki/dbQuery015

-- we don't have variability and extendedParamer columns

SELECT v.objectId, v.ra, v.decl
FROM   Object v, Object o
WHERE  o.objectId = 1234567890
   AND spDist(v.ra, v.decl, o.ra, o.decl, 0.1)
   AND v.variability > 0.8
   AND o.extendedParameter > 0.8

")

 (define case01_0010_leapSec "
-- This query is frequently executed by buildbot

-- See ticket #2048

SELECT offset, mjdRef, drift
FROM LeapSeconds 
WHERE whenUtc = (
        SELECT MAX(whenUtc) 
        FROM LeapSeconds 
        WHERE whenUtc <=  NAME_CONST('nsecs_',39900600000000000000000000)
                )
")

 ;; MySQL-ism (not supported)
'(define case01_0011_sdqaMetric "
  -- interesting syntax
  
  -- See ticket #2049
  -- This is a WONTFIX because NAME_CONST is a MySQL-ism.
  -- COLLATE will be low-priority if/when we have a test case for it that 
  -- doesn't include MySQL-isms.
  
  SELECT sdqa_metricId
  FROM   sdqa_Metric
  WHERE  metricName = NAME_CONST('metricName_',_latin1'ip.isr.numSaturatedPixels' COLLATE 'latin1_swedish_ci')")

 (define case01_0012_raftAndCcd "
-- interesting syntax

SELECT sce.filterId, sce.filterName
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.visit = 887404831)
   AND (sce.raftName = '3,3')
   AND (sce.ccdName LIKE '%')
")

 (define case01_1002_coneMagColor "
-- Cone-magnitude-color search
-- See http://dev.lsstcorp.org/trac/wiki/dbQuery003

SELECT COUNT(*)
FROM   Object
WHERE  ra_PS BETWEEN 0.1 AND 4  -- noQserv
AND    decl_PS BETWEEN -6 AND 6 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0.1, -6, 4, 6)
   AND scisql_fluxToAbMag(zFlux_PS) BETWEEN 20 AND 24
   AND scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) BETWEEN 0.1 AND 0.9
   AND scisql_fluxToAbMag(iFlux_PS)-scisql_fluxToAbMag(zFlux_PS) BETWEEN 0.1 AND 1.0
")

 (define case01_1003_coneMagColorEmptyRes "
-- Cone-magnitude-color search
-- See http://dev.lsstcorp.org/trac/wiki/dbQuery003

-- See ticket #2051


SELECT COUNT(*)
FROM   Object
WHERE  ra_PS BETWEEN 0 AND 4  -- noQserv
 AND   decl_PS BETWEEN -6 AND -5 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0, -6, 4, -5)
   AND scisql_fluxToAbMag(zFlux_PS) BETWEEN 20 AND 24
   AND scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) BETWEEN 0.1 AND 0.2
   AND scisql_fluxToAbMag(iFlux_PS)-scisql_fluxToAbMag(zFlux_PS) BETWEEN 0.1 AND 0.2
")

 (define case01_1004_varObjects "
-- Select all variable objects in given area
-- http://dev.lsstcorp.org/trac/wiki/dbQuery008

SELECT objectId
FROM   Object
WHERE  ra_PS BETWEEN 0 AND 3   -- noQserv
  AND  decl_PS BETWEEN 0 AND 10 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0, 0, 3, 10)
--   AND variability > 0.8
")

 (define case01_1005_allGalaxiesInArea "
-- Select all galaxies in a given area
-- http://dev.lsstcorp.org/trac/wiki/dbQuery014

-- Missing in current schema: extendedParameter

SELECT objectId
FROM   Object
WHERE  ra_PS BETWEEN :raMin AND :raMax       -- noQserv
   AND decl_PS BETWEEN :declMin AND :declMax -- noQserv
-- withQserv WHERE qserv_areaspec_box(:raMin, :declMin, :raMax, :declMax)
AND    extendedParameter > 0.8
")

 (define case01_1011_objectsForExposure "
-- joins, but for limited number of visits
-- sort by is here purely so that we can compare results from mysql and qserv

SELECT objectId
FROM   Source s
JOIN   Science_Ccd_Exposure sce USING (scienceCcdExposureId)
WHERE  sce.visit IN (885449631,886257441,886472151) ORDER BY objectId LIMIT 10
")

 (define case01_1012_orderByClause "
-- Just testing ORDER BY <clause>
-- (This query does not have real scientific meaning..)


SELECT objectId, iE1_SG, ABS(iE1_SG)
FROM Object
WHERE iE1_SG between -0.1 and 0.1
ORDER BY ABS(iE1_SG);
")

 (define case01_1013_orderByClauseRounded "
-- Variation of the previous query, with \"round\"
-- (This query does not have real scientific meaning..)


SELECT objectId, ROUND(iE1_SG, 3), ROUND(ABS(iE1_SG), 3)
FROM Object
WHERE iE1_SG between -0.1 and 0.1
ORDER BY ROUND(ABS(iE1_SG), 3);
")

 (define case01_1030_timeSeries "
-- Select time series data for all objects 
-- in a given area of the sky, 
-- in a given photometric band 
-- Similar query: http://dev.lsstcorp.org/trac/wiki/dbQuery007

-- See ticket #2052

SELECT objectId, taiMidPoint, scisql_fluxToAbMag(psfFlux)
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
 WHERE ra_PS BETWEEN 355 AND 360 -- noQserv
   and decl_PS BETWEEN 0 AND 20  -- noQserv
-- withQserv WHERE qserv_areaspec_box(355, 0, 360, 20)
   AND filterName = 'g'
ORDER BY objectId, taiMidPoint ASC
")

 (define case01_1031_newTransientsForEpoch "
-- Find new transients for a given epoch
-- http://dev.lsstcorp.org/trac/wiki/dbQuery005

-- Missing in current schema: Alert table

SELECT objectId 
FROM   Alert 
JOIN   _Alert2Type USING (alertId) 
JOIN   AlertType USING (alertTypeId)
WHERE  alertTypeDescr = 'newTransients'
  AND  Alert.timeGenerated BETWEEN :timeMin AND :timeMax
")

 (define case01_1051_nn "
-- Find near-neighbor objects in a given region


-- See ticket #1840

SELECT o1.objectId AS objId1,
       o2.objectId AS objId2,
       scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) AS distance
  FROM Object o1, 
       Object o2
 WHERE o1.ra_PS BETWEEN 0 AND 0.2 -- noQserv
   AND o1.decl_PS between 0 and 1 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0, 0, 0.2, 1)
   AND scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 1
   AND o1.objectId <> o2.objectId
")

 (define case01_1052_nnSimilarColors "
-- Find all objects within ? arcseconds of one another 
-- that have very similar colors
-- http://dev.lsstcorp.org/trac/wiki/dbQuery013
--
-- Similar queries:
--
-- * Find all galaxies without saturated pixels within 
--   certain distance of a given point
--   http://dev.lsstcorp.org/trac/wiki/dbQuery023

-- see ticket #1840 (the code that needs subchunks is
-- currently using hardcoded database name \"LSST\")

SELECT DISTINCT o1.objectId, o2.objectId
FROM   Object o1, 
       Object o2
WHERE  scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 1
  AND  o1.objectId <> o2.objectId
  AND  ABS( (scisql_fluxToAbMag(o1.gFlux_PS)-scisql_fluxToAbMag(o1.rFlux_PS)) - 
            (scisql_fluxToAbMag(o2.gFlux_PS)-scisql_fluxToAbMag(o2.rFlux_PS)) ) < 1
  AND  ABS( (scisql_fluxToAbMag(o1.rFlux_PS)-scisql_fluxToAbMag(o1.iFlux_PS)) - 
            (scisql_fluxToAbMag(o2.rFlux_PS)-scisql_fluxToAbMag(o2.iFlux_PS)) ) < 1
  AND  ABS( (scisql_fluxToAbMag(o1.iFlux_PS)-scisql_fluxToAbMag(o1.zFlux_PS)) - 
            (scisql_fluxToAbMag(o2.iFlux_PS)-scisql_fluxToAbMag(o2.zFlux_PS)) ) < 1
")

;; SET not supported because queries must be stateless
 '(define case01_1070_areaUsingPoly "
-- Select objects withiin a rectangular area on the sky


-- see ticket #2056


-- Create a binary representation of the search polygon
SET @poly = scisql_s2CPolyToBin(300, 2, 0.01, 2, 0.03, 2.6,  359.9, 2.6);

-- Compute HTM ID ranges for the level 20 triangles overlapping
-- @poly. They will be stored in a temp table called scisql.Region
-- with two columns, htmMin and htmMax
CALL scisql.scisql_s2CPolyRegion(@poly, 20);

-- Select reference objects inside the polygon. The join against
-- the HTM ID range table populated above cuts down on the number of
-- SimRefObject rows that need to be tested against the polygon
SELECT refObjectId, isStar, ra, decl, rMag
FROM SimRefObject AS sro INNER JOIN
    scisql.Region AS r ON (sro.htmId20 BETWEEN r.htmMin AND r.htmMax)
WHERE scisql_s2PtInCPoly(ra, decl, @poly) = 1;

")

(define case01_1080_refMatch1 "
SELECT sce.visit, sce.raftName, sce.ccdName, 
       sro.gMag, sro.ra, sro.decl, sro.isStar, sro.refObjectId,  
       rom.nSrcMatches,
       s.sourceId,s.ra,s.decl,s.xAstrom,s.yAstrom,s.psfFlux,s.psfFluxSigma,
       s.apFlux,s.apFluxSigma,s.flux_ESG,s.flux_ESG_Sigma,s.flux_Gaussian,
       s.flux_Gaussian_Sigma,s.ixx,s.iyy,s.ixy,s.psfIxx,s.psfIxxSigma,
       s.psfIyy,s.psfIyySigma,s.psfIxy,s.psfIxySigma,s.resolution_SG,
       s.e1_SG,s.e1_SG_Sigma,s.e2_SG,s.e2_SG_Sigma,s.shear1_SG,s.shear1_SG_Sigma,
       s.shear2_SG,s.shear2_SG_Sigma,s.sourceWidth_SG,s.sourceWidth_SG_Sigma,
       s.flagForDetection
FROM Source AS s, 
     Science_Ccd_Exposure AS sce,
     RefSrcMatch AS rom,
     SimRefObject AS sro
WHERE (s.scienceCcdExposureId = sce.scienceCcdExposureId)
  AND (s.sourceId = rom.sourceId)
  AND (rom.refObjectId = sro.refObjectId)
  AND (sce.visit = 888241840)
  AND (sce.raftName = '1,0') 
  AND (sce.ccdName like '%')
")

 (define case01_1081_refMatch2 "
SELECT count(*)
FROM   Object o
       INNER JOIN RefObjMatch o2t ON (o.objectId = o2t.objectId)
       LEFT  JOIN SimRefObject t ON (o2t.refObjectId = t.refObjectId)
WHERE  closestToObj = 1
    OR closestToObj is NULL
")

 (define case01_1083_refMatch3 "
select objectId, sro.*, (sro.refObjectId-1)/2%pow(2,10) typeId 
from Source s 
join RefObjMatch rom using (objectId) 
join SimRefObject sro using (refObjectId) 
where isStar =1 limit 10
")

 (define case01_2001_fullObjectScan "
-- Full table scan for Object table with some cuts.
--
-- Similar queries:
--
-- * Find quasars 
--   http://dev.lsstcorp.org/trac/wiki/dbQuery018
--
-- * Low-z QSO candidates using the color cuts
--   http://dev.lsstcorp.org/trac/wiki/dbQuery020
--
-- * Find high proper motion white dwarf candidates
--   http://dev.lsstcorp.org/trac/wiki/dbQuery026
--
-- * Find extremely red galaxies
--   http://dev.lsstcorp.org/trac/wiki/dbQuery037


SELECT objectId,
       scisql_fluxToAbMag(uFlux_PS),
       scisql_fluxToAbMag(gFlux_PS),
       scisql_fluxToAbMag(rFlux_PS),
       scisql_fluxToAbMag(iFlux_PS),
       scisql_fluxToAbMag(zFlux_PS),
       scisql_fluxToAbMag(yFlux_PS),
       ra_PS, decl_PS
FROM   Object
WHERE  ( scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) > 0.7 OR 
         scisql_fluxToAbMag(gFlux_PS) > 22.3 )
AND    scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) > 0.1
AND    ( scisql_fluxToAbMag(rFlux_PS)-scisql_fluxToAbMag(iFlux_PS) < 
         (0.08 + 0.42 * (scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) - 0.96)) 
        OR scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) > 1.26 )
AND    scisql_fluxToAbMag(iFlux_PS)-scisql_fluxToAbMag(zFlux_PS) < 0.8
ORDER BY objectId
")

 (define case01_2002_findStarsWithMultiMeasAndMagVariation "
-- Find stars with multiple measurements and with certain magnitude variations
-- http://dev.lsstcorp.org/trac/wiki/dbQuery010


-- Missing in current schema: extendedParameter

SELECT  objectId
FROM    Object
WHERE   extendedParameter > 0.8 -- a star
  AND   uMag BETWEEN 1 AND 27  -- magnitudes are reasonable
  AND   gMag BETWEEN 1 AND 27
  AND   rMag BETWEEN 1 AND 27
  AND   iMag BETWEEN 1 AND 27
  AND   zMag BETWEEN 1 AND 27
  AND   yMag BETWEEN 1 AND 27
  AND (                           -- and one of the colors is  different.
         uAmplitude > .1 + ABS(uMagSigma)
      OR gAmplitude > .1 + ABS(gMagSigma)
      OR rAmplitude > .1 + ABS(rMagSigma)
      OR iAmplitude > .1 + ABS(iMagSigma)
      OR zAmplitude > .1 + ABS(zMagSigma)
      OR yAmplitude > .1 + ABS(yMagSigma))")

 (define case01_2003_objectWithVariabilityOrPeriodOrMag "
-- Select all objects with certain variability or period or amplitude
-- http://dev.lsstcorp.org/trac/wiki/dbQuery011

-- Missing in current schema: variability

SELECT *
FROM   Object
WHERE  variability > 0.8 -- variable object
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
    OR yAmplitude > :amplitudeMin")

 (define case01_2004_objectsSimilarToQuasarsWithRedshift "
-- Find all objects similar to the colors of a quasar 
-- with redshift in a given range
-- http://dev.lsstcorp.org/trac/wiki/dbQuery012

-- Missing in current schema: ObjectToType

SELECT  COUNT(*)                                               AS totalCount,
        SUM(CASE WHEN (typeId=3) THEN 1 ELSE 0 END)            AS galaxyCount,
        SUM(CASE WHEN (typeId=6) THEN 1 ELSE 0 END)            AS starCount,
        SUM(CASE WHEN (typeId NOT IN (3,6)) THEN 1 ELSE 0 END) AS otherCount
FROM    Object
JOIN    _Object2Type USING(objectId)
WHERE  (uMag-gMag > 2.0 OR uMag > 22.3)
   AND iMag BETWEEN 0 AND 19 
   AND gMag - rMag > 1.0 
   AND ( (rMag-iMag < 0.08 + 0.42 * (gMag-rMag - 0.96)) OR (gMag-rMag > 2.26 ) )
   AND iMag-zMag < 0.25")

 (define case01_2005_varObjectsOfOneType "
-- Select all variable objects of a specific type
-- See http://dev.lsstcorp.org/trac/wiki/dbQuery002


-- missing in current schema: variability, probability

SELECT objectId
FROM   Object
JOIN   _ObjectToType USING(objectId)
JOIN   ObjectType USING (typeId)
WHERE  description = 'Supernova'
  AND  variability > 0.8
  AND  probability > 0.8

")

 (define case01_2006_randomSample "
-- Random sample of the dataq
-- http://dev.lsstcorp.org/trac/wiki/dbQuery004


SELECT fluxToAbMag(uFlux_PS), 
       fluxToAbMag(gFlux_PS), 
       fluxToAbMag(rFlux_PS), 
       fluxToAbMag(iFlux_PS), 
       fluxToAbMag(zFlux_PS), 
       fluxToAbMag(yFlux_PS)
FROM   Object 
WHERE  (objectId % 100 ) = :percentage
")

 (define case01_2010_logs "
-- interesting syntax

-- subquery - not implemented

SELECT CASE gid WHEN 1 THEN 'pipeline shutdowns seen'
                WHEN 2 THEN 'CCDs attempted'
                WHEN 3 THEN 'src writes'
                WHEN 4 THEN 'calexp writes'
       END AS descr, 
       COUNT(*) 
FROM ( SELECT CASE WHEN COMMENT LIKE 'Processing job:% visit=0 %' THEN 1
                   WHEN COMMENT LIKE 'Processing job:%' AND COMMENT NOT LIKE '% visit=0 %' THEN 2
                   WHEN COMMENT LIKE 'Ending write to BoostStorage%/src%' THEN 3
                   WHEN COMMENT LIKE 'Ending write to FitsStorage%/calexp%' THEN 4
                   ELSE 0
              END AS gid
       FROM Logs ) AS stats
WHERE gid > 0
GROUP BY gid
")

 (define case01_2100_groupByChunkId "
-- chunkId column must be filled in input data so that mysql mode
-- can give same answers as Qserv

SELECT count(*) AS n, AVG(ra_PS), AVG(decl_PS), chunkId
FROM Object
GROUP BY chunkId;
")

 '(define case01_3001_SelectInPoly "

SET @poly = scisql_s2CPolyToBin(359.5, -5.0,
                                0.05, -5.0,
                                0.05, 3.5,
                                359.5, 3.5);

SELECT count(*)
FROM Object
where scisql_s2PtInCPoly(ra_PS, decl_PS, @poly) = 1 ;
")

 (define case01_3002_sameColumnName "

SELECT o1.ra_PS,o2.ra_PS
FROM Object o1, Object o2
WHERE o1.objectid = 402391191015221
  AND o2.objectid = 390030275138483 ;
  
")

 (define case01_3003_SameColumnTwice "
-- Selecting the same column twice

SELECT o.ra_PS,o.decl_PS,o.ra_PS
FROM Object o
WHERE o.objectid = 402391191015221 ;

  
")

 (define case01_3004_nonExistingColumn "
-- Selecting a non existing field

SELECT o.foobar
FROM Object o
WHERE o.objectid = 402391191015221 ; 
")

 (define case01_3005_orderByRA "
SELECT * 
 FROM Object
 WHERE ra_PS BETWEEN 0. AND 1.   -- noQserv
   AND decl_PS BETWEEN 0. AND 1. -- noQserv
-- withQserv WHERE qserv_areaspec_box(0.,1.,0.,1.)
 ORDER BY ra_PS
")

 (define case01_3006_selectAs "
SELECT ra_PS AS ra
FROM Object
WHERE ra_PS BETWEEN 0. AND 1.
  AND decl_PS BETWEEN 0. AND 1.;
")

 (define case01_3007_countGroupBy "

SELECT count(src.sourceId), avg(o.ra_PS), avg(o.decl_PS)
FROM Object o, Source src
WHERE ra_PS  BETWEEN 0. AND 1.
 AND decl_PS BETWEEN 0. AND 1.
 GROUP BY src.objectId ;
 
")

 '(define case01_3008_showColumns "
SHOW COLUMNS FROM Object;
")

 (define case01_3009_subquery "

SELECT src.sourceId
FROM Source src
WHERE src.objectId IN (
  SELECT objectId
  FROM Object o
  WHERE ra_PS  BETWEEN 0. AND 1.
   AND decl_PS BETWEEN 0. AND 1. 
) ;
")

 (define case01_3010_selectWithComputation "
SELECT o.objectId,src.*,src.sourceId%pow(2,10)
FROM Object o, Source src
WHERE o.ra_PS  BETWEEN 0. AND 1.
 AND o.decl_PS BETWEEN 0. AND 1.
 AND o.objectId = src.objectId ;
 
")

 (define case01_3011_selectSample "

SELECT uFlux_PS, gFlux_PS, rFlux_PS, 
       iFlux_PS, zFlux_PS, yFlux_PS
FROM   Object 
WHERE  (objectId % 100 ) = 57
;
")

 (define case01_3012_similarObject "
SELECT o1.objectId, o2.objectId
FROM   Object o1, 
       Object o2
WHERE  scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 1
  AND  o1.objectId <> o2.objectId
  AND  ABS( (scisql_fluxToAbMag(o1.gFlux_PS)-scisql_fluxToAbMag(o1.rFlux_PS)) - 
            (scisql_fluxToAbMag(o2.gFlux_PS)-scisql_fluxToAbMag(o2.rFlux_PS)) ) < 1
  AND  ABS( (scisql_fluxToAbMag(o1.rFlux_PS)-scisql_fluxToAbMag(o1.iFlux_PS)) - 
            (scisql_fluxToAbMag(o2.rFlux_PS)-scisql_fluxToAbMag(o2.iFlux_PS)) ) < 1
  AND  ABS( (scisql_fluxToAbMag(o1.iFlux_PS)-scisql_fluxToAbMag(o1.zFlux_PS)) - 
            (scisql_fluxToAbMag(o2.iFlux_PS)-scisql_fluxToAbMag(o2.zFlux_PS)) ) < 1
")

 (define case01_3013_nonexistantTable "

select count(*) from Sources;
")

 (define case01_3014_limitAfterAreaspec "
-- This is testing syntax (limit after areaspec)

-- See ticket #2200

SELECT objectId
FROM Object
WHERE ra_PS BETWEEN 0.1 AND 4  -- noQserv
  AND decl_PS BETWEEN -6 AND 6 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0.1, -6, 4, 6)
LIMIT 10
")

 (define case01_3015_orderByFunction "

SELECT objectId
FROM Object
WHERE scisql_fluxToAbMag(zFlux_PS) BETWEEN 20 AND 24
ORDER BY (scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS))
")

 (define case01_3016_selectAllPairsWithinDistance "
-- Select all pairs  within some distance of points in region
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- based on https://dev.lsstcorp.org/trac/wiki/db/queries/022

SELECT o1.objectId, o2.objectId
FROM Object o1, Object o2 
WHERE o1.ra_PS BETWEEN 0.04 AND 5.  -- noQserv
  AND o1.decl_PS BETWEEN -3. AND 3. -- noQserv
-- withQserv WHERE   qserv_areaspec_box(0.04, -3., 5., 3.)
  AND scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_Ps) < 5.

")

 (define case01_8001_badLiteral "


-- note wrong literal: \"35 .1\" instead of \"35.1\"

SELECT count(*) 
FROM Object
WHERE ra_PS BETWEEN 35 AND 35 .1 -- noQserv
  AND decl_PS BETWEEN 6 AND 6.0001 -- noQserv
-- withQserv WHERE qserv_areaSpec_box(35, 6, 35 .1, 6.0001);

")

 (define case01_8002_badLiteral "


-- note wrong literal: \"35. 1\" instead of \"35.1\"

SELECT count(*) 
FROM Object
WHERE ra_PS BETWEEN 35 AND 35. 1 -- noQserv
  AND decl_PS BETWEEN 6 AND 6.0001 -- noQserv
-- withQserv WHERE qserv_areaSpec_box(35, 6, 35. 1, 6.0001);

")

 (define case01_8003_areaWithLimitClause "
-- This is testing syntax (limit after areaspec)

-- See ticket #2200

SELECT COUNT(*) 
FROM   Object 
WHERE  ra_PS BETWEEN 355 AND 356 -- noQserv
  AND  decl_PS BETWEEN 0 AND 1   -- noQserv
-- withQserv WHERE qserv_areaspec_box(355, 0, 356, 1)
LIMIT 10
")

 (define case01_8004_badArea "
-- Bad area spec box

SELECT o1.objectId, o2.objectId
FROM Object o1, Object o2 
WHERE o1.ra_PS BETWEEN 0.04 AND -3.  -- noQserv
  AND o1.decl_PS BETWEEN 5. AND 3. -- noQserv
-- withQserv WHERE   qserv_areaspec_box(0.04, 5., -3., 3.)
  AND scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_Ps) < 5.

")

 (define case02_0001_fetchObjectById "
-- Find an object with a particular object id
-- http://dev.lsstcorp.org/trac/wiki/dbQuery009

-- note that the data for mysql version is specially
-- precooked for this object to include chunkId and
-- subchunkId

SELECT
objectId, iauId, ra_PS, ra_PS_Sigma, decl_PS, decl_PS_Sigma, radecl_PS_Cov, ra_SG, ra_SG_Sigma, decl_SG, decl_SG_Sigma, radecl_SG_Cov, raRange, declRange, muRa_PS, muRa_PS_Sigma, muDecl_PS, muDecl_PS_Sigma, muRaDecl_PS_Cov, parallax_PS, parallax_PS_Sigma, canonicalFilterId, extendedness, varProb, earliestObsTime, latestObsTime, flags, uNumObs, uExtendedness, uVarProb, uRaOffset_PS, uRaOffset_PS_Sigma, uDeclOffset_PS, uDeclOffset_PS_Sigma, uRaDeclOffset_PS_Cov, uRaOffset_SG, uRaOffset_SG_Sigma, uDeclOffset_SG, uDeclOffset_SG_Sigma, uRaDeclOffset_SG_Cov, uLnL_PS, uLnL_SG, uFlux_PS, uFlux_PS_Sigma, uFlux_SG, uFlux_SG_Sigma, uFlux_CSG, uFlux_CSG_Sigma, uTimescale, uEarliestObsTime, uLatestObsTime, uSersicN_SG, uSersicN_SG_Sigma, uE1_SG, uE1_SG_Sigma, uE2_SG, uE2_SG_Sigma, uRadius_SG, uRadius_SG_Sigma, uFlags, gNumObs, gExtendedness, gVarProb, gRaOffset_PS, gRaOffset_PS_Sigma, gDeclOffset_PS, gDeclOffset_PS_Sigma, gRaDeclOffset_PS_Cov, gRaOffset_SG, gRaOffset_SG_Sigma, gDeclOffset_SG, gDeclOffset_SG_Sigma, gRaDeclOffset_SG_Cov, gLnL_PS, gLnL_SG, gFlux_PS, gFlux_PS_Sigma, gFlux_SG, gFlux_SG_Sigma, gFlux_CSG, gFlux_CSG_Sigma, gTimescale, gEarliestObsTime, gLatestObsTime, gSersicN_SG, gSersicN_SG_Sigma, gE1_SG, gE1_SG_Sigma, gE2_SG, gE2_SG_Sigma, gRadius_SG, gRadius_SG_Sigma, gFlags, rNumObs, rExtendedness, rVarProb, rRaOffset_PS, rRaOffset_PS_Sigma, rDeclOffset_PS, rDeclOffset_PS_Sigma, rRaDeclOffset_PS_Cov, rRaOffset_SG, rRaOffset_SG_Sigma, rDeclOffset_SG, rDeclOffset_SG_Sigma, rRaDeclOffset_SG_Cov, rLnL_PS, rLnL_SG, rFlux_PS, rFlux_PS_Sigma, rFlux_SG, rFlux_SG_Sigma, rFlux_CSG, rFlux_CSG_Sigma, rTimescale, rEarliestObsTime, rLatestObsTime, rSersicN_SG, rSersicN_SG_Sigma, rE1_SG, rE1_SG_Sigma, rE2_SG, rE2_SG_Sigma, rRadius_SG, rRadius_SG_Sigma, rFlags, iNumObs, iExtendedness, iVarProb, iRaOffset_PS, iRaOffset_PS_Sigma, iDeclOffset_PS, iDeclOffset_PS_Sigma, iRaDeclOffset_PS_Cov, iRaOffset_SG, iRaOffset_SG_Sigma, iDeclOffset_SG, iDeclOffset_SG_Sigma, iRaDeclOffset_SG_Cov, iLnL_PS, iLnL_SG, iFlux_PS, iFlux_PS_Sigma, iFlux_SG, iFlux_SG_Sigma, iFlux_CSG, iFlux_CSG_Sigma, iTimescale, iEarliestObsTime, iLatestObsTime, iSersicN_SG, iSersicN_SG_Sigma, iE1_SG, iE1_SG_Sigma, iE2_SG, iE2_SG_Sigma, iRadius_SG, iRadius_SG_Sigma, iFlags, zNumObs, zExtendedness, zVarProb, zRaOffset_PS, zRaOffset_PS_Sigma, zDeclOffset_PS, zDeclOffset_PS_Sigma, zRaDeclOffset_PS_Cov, zRaOffset_SG, zRaOffset_SG_Sigma, zDeclOffset_SG, zDeclOffset_SG_Sigma, zRaDeclOffset_SG_Cov, zLnL_PS, zLnL_SG, zFlux_PS, zFlux_PS_Sigma, zFlux_SG, zFlux_SG_Sigma, zFlux_CSG, zFlux_CSG_Sigma, zTimescale, zEarliestObsTime, zLatestObsTime, zSersicN_SG, zSersicN_SG_Sigma, zE1_SG, zE1_SG_Sigma, zE2_SG, zE2_SG_Sigma, zRadius_SG, zRadius_SG_Sigma, zFlags, yNumObs, yExtendedness, yVarProb, yRaOffset_PS, yRaOffset_PS_Sigma, yDeclOffset_PS, yDeclOffset_PS_Sigma, yRaDeclOffset_PS_Cov, yRaOffset_SG, yRaOffset_SG_Sigma, yDeclOffset_SG, yDeclOffset_SG_Sigma, yRaDeclOffset_SG_Cov, yLnL_PS, yLnL_SG, yFlux_PS, yFlux_PS_Sigma, yFlux_SG, yFlux_SG_Sigma, yFlux_CSG, yFlux_CSG_Sigma, yTimescale, yEarliestObsTime, yLatestObsTime, ySersicN_SG, ySersicN_SG_Sigma, yE1_SG, yE1_SG_Sigma, yE2_SG, yE2_SG_Sigma, yRadius_SG, yRadius_SG_Sigma, yFlags
FROM   Object
WHERE  objectId = 433327840428032
")

 (define case02_0002_fetchObjectByIdNoResult "
-- Find an object with a particular object id
-- http://dev.lsstcorp.org/trac/wiki/dbQuery009

-- not working, see ticket #1847

SELECT *
FROM   Object
WHERE  objectId = 430213989000
")

 (define case02_0003_selectMetadataForOneGalaxy "
-- select the full color image of a single given galaxy
-- http://dev.lsstcorp.org/trac/wiki/dbQuery006

SELECT s.ra, s.decl, o.raRange, o.declRange
FROM   Object o
JOIN   Source s USING (objectId)
WHERE  o.objectId = 433327840428032 
-- AND    o.latestObsTime = s.taiMidPoint
")

 (define case02_0004_fetchSourceById "
-- Find an object with a particular object id
-- http://dev.lsstcorp.org/trac/wiki/dbQuery009

-- note that the data for mysql version is specially
-- precooked for this object to include chunkId and
-- subchunkId

SELECT
sourceId, scienceCcdExposureId, filterId, objectId, movingObjectId, procHistoryId, ra, raErrForDetection, raErrForWcs, decl, declErrForDetection, declErrForWcs, xFlux, xFluxErr, yFlux, yFluxErr, raFlux, raFluxErr, declFlux, declFluxErr, xPeak, yPeak, raPeak, declPeak, xAstrom, xAstromErr, yAstrom, yAstromErr, raAstrom, raAstromErr, declAstrom, declAstromErr, raObject, declObject, taiMidPoint, taiRange, psfFlux, psfFluxErr, apFlux, apFluxErr, modelFlux, modelFluxErr, petroFlux, petroFluxErr, instFlux, instFluxErr, nonGrayCorrFlux, nonGrayCorrFluxErr, atmCorrFlux, atmCorrFluxErr, apDia, Ixx, IxxErr, Iyy, IyyErr, Ixy, IxyErr, snr, chi2, sky, skyErr, extendedness, flux_PS, flux_PS_Sigma, flux_SG, flux_SG_Sigma, sersicN_SG, sersicN_SG_Sigma, e1_SG, e1_SG_Sigma, e2_SG, e2_SG_Sigma, radius_SG, radius_SG_Sigma, flux_flux_SG_Cov, flux_e1_SG_Cov, flux_e2_SG_Cov, flux_radius_SG_Cov, flux_sersicN_SG_Cov, e1_e1_SG_Cov, e1_e2_SG_Cov, e1_radius_SG_Cov, e1_sersicN_SG_Cov, e2_e2_SG_Cov, e2_radius_SG_Cov, e2_sersicN_SG_Cov, radius_radius_SG_Cov, radius_sersicN_SG_Cov, sersicN_sersicN_SG_Cov, flagForAssociation, flagForDetection, flagForWcs
FROM   Source 
WHERE  sourceId = 2867930096075697 
")

 (define case02_0006_transientVarObjNearGalaxy "
-- Select transient variable objects near a known galaxy
-- http://dev.lsstcorp.org/trac/wiki/dbQuery015

-- we don't have variability and extendedParamer columns

SELECT v.objectId, v.ra, v.decl
FROM   Object v, Object o
WHERE  o.objectId = :objectId
   AND spDist(v.ra, v.decl, o.ra, o.decl, :dist)
   AND v.variability > 0.8
   AND o.extendedParameter > 0.8

")

 (define case02_1002_coneMagColor "
-- Cone-magnitude-color search
-- See http://dev.lsstcorp.org/trac/wiki/dbQuery003

SELECT COUNT(*)
FROM   Object
WHERE  ra_PS BETWEEN 0.1 AND 4  -- noQserv
AND    decl_PS BETWEEN -6 AND 6 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0.1, -6, 4, 6)
   AND scisql_fluxToAbMag(zFlux_PS) BETWEEN 20 AND 24
   AND scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) BETWEEN 0.1 AND 0.9
   AND scisql_fluxToAbMag(iFlux_PS)-scisql_fluxToAbMag(zFlux_PS) BETWEEN 0.1 AND 1.0
")

 (define case02_1003_coneMagColorEmptyRes "
-- Cone-magnitude-color search
-- See http://dev.lsstcorp.org/trac/wiki/dbQuery003

-- See ticket #2051


SELECT COUNT(*)
FROM   Object
WHERE  ra_PS BETWEEN 0 AND 4  -- noQserv
 AND   decl_PS BETWEEN -6 AND -5 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0, -6, 4, -5)
   AND scisql_fluxToAbMag(zFlux_PS) BETWEEN 20 AND 24
   AND scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) BETWEEN 0.1 AND 0.2
   AND scisql_fluxToAbMag(iFlux_PS)-scisql_fluxToAbMag(zFlux_PS) BETWEEN 0.1 AND 0.2
")

 (define case02_1004_varObjects "
-- Select all variable objects in given area
-- http://dev.lsstcorp.org/trac/wiki/dbQuery008

SELECT objectId
FROM   Object
WHERE  ra_PS BETWEEN 0 AND 3   -- noQserv
  AND  decl_PS BETWEEN 0 AND 10 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0, 0, 3, 10)
--   AND variability > 0.8
")

 (define case02_1005_allGalaxiesInArea "
-- Select all galaxies in a given area
-- http://dev.lsstcorp.org/trac/wiki/dbQuery014

-- Missing in current schema: extendedParameter

SELECT objectId
FROM   Object
WHERE  ra_PS BETWEEN :raMin AND :raMax       -- noQserv
   AND decl_PS BETWEEN :declMin AND :declMax -- noQserv
-- withQserv WHERE qserv_areaspec_box(:raMin, :declMin, :raMax, :declMax)
AND    extendedParameter > 0.8
")

 (define case02_1011_objectsForExposure "
-- joins, but for limited number of visits
-- sort by is here purely so that we can compare results from mysql and qserv

SELECT objectId
FROM   Source s
JOIN   Science_Ccd_Exposure sce USING (scienceCcdExposureId)
WHERE  sce.visit IN (885449631,886257441,886472151) ORDER BY objectId LIMIT 10
")

 (define case02_1012_orderByClause "
-- Just testing ORDER BY <clause>
-- (This query does not have real scientific meaning..)


SELECT objectId, iE1_SG, ABS(iE1_SG)
FROM Object
WHERE iE1_SG between -0.1 and 0.1
ORDER BY ABS(iE1_SG);
")

 (define case02_1013_orderByClauseRounded "
-- Variation of the previous query, with \"round\"
-- (This query does not have real scientific meaning..)


SELECT objectId, ROUND(iE1_SG, 3), ROUND(ABS(iE1_SG), 3)
FROM Object
WHERE iE1_SG between -0.1 and 0.1
ORDER BY ROUND(ABS(iE1_SG), 3);
")

 (define case02_1030_timeSeries "
-- Select time series data for all objects 
-- in a given area of the sky, 
-- in a given photometric band 
-- Similar query: http://dev.lsstcorp.org/trac/wiki/dbQuery007

-- See ticket #2052

SELECT objectId, taiMidPoint, scisql_fluxToAbMag(psfFlux)
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
 WHERE ra_PS BETWEEN 355 AND 360 -- noQserv
   and decl_PS BETWEEN 0 AND 20  -- noQserv
-- withQserv WHERE qserv_areaspec_box(355, 0, 360, 20)
   AND filterName = 'g'
ORDER BY objectId, taiMidPoint ASC
")

 (define case02_1031_newTransientsForEpoch "
-- Find new transients for a given epoch
-- http://dev.lsstcorp.org/trac/wiki/dbQuery005

-- Missing in current schema: Alert table

SELECT objectId 
FROM   Alert 
JOIN   _Alert2Type USING (alertId) 
JOIN   AlertType USING (alertTypeId)
WHERE  alertTypeDescr = 'newTransients'
  AND  Alert.timeGenerated BETWEEN :timeMin AND :timeMax
")

 (define case02_1051_nn "
-- Find near-neighbor objects in a given region


-- See ticket #1840

SELECT o1.objectId AS objId1,
       o2.objectId AS objId2,
       scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) AS distance
  FROM Object o1, 
       Object o2
 WHERE o1.ra_PS BETWEEN 0 AND 0.2 -- noQserv
   AND o1.decl_PS between 0 and 1 -- noQserv
-- withQserv WHERE qserv_areaspec_box(0, 0, 0.2, 1)
   AND scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 1
   AND o1.objectId <> o2.objectId
")

 (define case02_1052_nnSimilarColors "
-- Find all objects within ? arcseconds of one another 
-- that have very similar colors
-- http://dev.lsstcorp.org/trac/wiki/dbQuery013
--
-- Similar queries:
--
-- * Find all galaxies without saturated pixels within 
--   certain distance of a given point
--   http://dev.lsstcorp.org/trac/wiki/dbQuery023

-- see ticket #1840 (the code that needs subchunks is
-- currently using hardcoded database name \"LSST\")

SELECT DISTINCT o1.objectId, o2.objectId
FROM   Object o1, 
       Object o2
WHERE  scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 1
  AND  o1.objectId <> o2.objectId
  AND  ABS( (scisql_fluxToAbMag(o1.gFlux_PS)-scisql_fluxToAbMag(o1.rFlux_PS)) - 
            (scisql_fluxToAbMag(o2.gFlux_PS)-scisql_fluxToAbMag(o2.rFlux_PS)) ) < 1
  AND  ABS( (scisql_fluxToAbMag(o1.rFlux_PS)-scisql_fluxToAbMag(o1.iFlux_PS)) - 
            (scisql_fluxToAbMag(o2.rFlux_PS)-scisql_fluxToAbMag(o2.iFlux_PS)) ) < 1
  AND  ABS( (scisql_fluxToAbMag(o1.iFlux_PS)-scisql_fluxToAbMag(o1.zFlux_PS)) - 
            (scisql_fluxToAbMag(o2.iFlux_PS)-scisql_fluxToAbMag(o2.zFlux_PS)) ) < 1
")

;; SET not supported because queries must be stateless
 '(define case02_1070_areaUsingPoly "
-- Select objects withiin a rectangular area on the sky


-- see ticket #2056


-- Create a binary representation of the search polygon
SET @poly = scisql_s2CPolyToBin(300, 2, 0.01, 2, 0.03, 2.6,  359.9, 2.6);

-- Compute HTM ID ranges for the level 20 triangles overlapping
-- @poly. They will be stored in a temp table called scisql.Region
-- with two columns, htmMin and htmMax
CALL scisql.scisql_s2CPolyRegion(@poly, 20);

-- Select reference objects inside the polygon. The join against
-- the HTM ID range table populated above cuts down on the number of
-- SimRefObject rows that need to be tested against the polygon
SELECT refObjectId, isStar, ra, decl, rMag
FROM SimRefObject AS sro INNER JOIN
    scisql.Region AS r ON (sro.htmId20 BETWEEN r.htmMin AND r.htmMax)
WHERE scisql_s2PtInCPoly(ra, decl, @poly) = 1;

")

 (define case02_1080_refMatch1 "
SELECT sce.visit, sce.raftName, sce.ccdName, 
       sro.gMag, sro.ra, sro.decl, sro.isStar, sro.refObjectId,  
       rom.nSrcMatches,
       s.sourceId,s.ra,s.decl,s.xAstrom,s.yAstrom,s.psfFlux,s.psfFluxSigma,
       s.apFlux,s.apFluxSigma,s.flux_ESG,s.flux_ESG_Sigma,s.flux_Gaussian,
       s.flux_Gaussian_Sigma,s.ixx,s.iyy,s.ixy,s.psfIxx,s.psfIxxSigma,
       s.psfIyy,s.psfIyySigma,s.psfIxy,s.psfIxySigma,s.resolution_SG,
       s.e1_SG,s.e1_SG_Sigma,s.e2_SG,s.e2_SG_Sigma,s.shear1_SG,s.shear1_SG_Sigma,
       s.shear2_SG,s.shear2_SG_Sigma,s.sourceWidth_SG,s.sourceWidth_SG_Sigma,
       s.flagForDetection
FROM Source AS s, 
     Science_Ccd_Exposure AS sce,
     RefSrcMatch AS rom,
     SimRefObject AS sro
WHERE (s.scienceCcdExposureId = sce.scienceCcdExposureId)
  AND (s.sourceId = rom.sourceId)
  AND (rom.refObjectId = sro.refObjectId)
  AND (sce.visit = 888241840)
  AND (sce.raftName = '1,0') 
  AND (sce.ccdName like '%')
")

 (define case02_1081_refMatch2 "
SELECT count(*)
FROM   Object o
       INNER JOIN RefObjMatch o2t ON (o.objectId = o2t.objectId)
       LEFT  JOIN SimRefObject t ON (o2t.refObjectId = t.refObjectId)
WHERE  closestToObj = 1
    OR closestToObj is NULL
")

 (define case02_1083_refMatch3 "
select objectId, sro.*, (sro.refObjectId-1)/2%pow(2,10) typeId 
from Source s 
join RefObjMatch rom using (objectId) 
join SimRefObject sro using (refObjectId) 
where isStar =1 limit 10
")

 (define case02_2001_fullObjectScan "
-- Full table scan for Object table with some cuts.
--
-- Similar queries:
--
-- * Find quasars 
--   http://dev.lsstcorp.org/trac/wiki/dbQuery018
--
-- * Low-z QSO candidates using the color cuts
--   http://dev.lsstcorp.org/trac/wiki/dbQuery020
--
-- * Find high proper motion white dwarf candidates
--   http://dev.lsstcorp.org/trac/wiki/dbQuery026
--
-- * Find extremely red galaxies
--   http://dev.lsstcorp.org/trac/wiki/dbQuery037


SELECT objectId,
       scisql_fluxToAbMag(uFlux_PS),
       scisql_fluxToAbMag(gFlux_PS),
       scisql_fluxToAbMag(rFlux_PS),
       scisql_fluxToAbMag(iFlux_PS),
       scisql_fluxToAbMag(zFlux_PS),
       scisql_fluxToAbMag(yFlux_PS),
       ra_PS, decl_PS
FROM   Object
WHERE  ( scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) > 0.7 OR 
         scisql_fluxToAbMag(gFlux_PS) > 22.3 )
AND    scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) > 0.1
AND    ( scisql_fluxToAbMag(rFlux_PS)-scisql_fluxToAbMag(iFlux_PS) < 
         (0.08 + 0.42 * (scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) - 0.96)) 
        OR scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) > 1.26 )
AND    scisql_fluxToAbMag(iFlux_PS)-scisql_fluxToAbMag(zFlux_PS) < 0.8")

 (define case02_2002_findStarsWithMultiMeasAndMagVariation "
-- Find stars with multiple measurements and with certain magnitude variations
-- http://dev.lsstcorp.org/trac/wiki/dbQuery010


-- Missing in current schema: extendedParameter

SELECT  objectId
FROM    Object
WHERE   extendedParameter > 0.8 -- a star
  AND   uMag BETWEEN 1 AND 27  -- magnitudes are reasonable
  AND   gMag BETWEEN 1 AND 27
  AND   rMag BETWEEN 1 AND 27
  AND   iMag BETWEEN 1 AND 27
  AND   zMag BETWEEN 1 AND 27
  AND   yMag BETWEEN 1 AND 27
  AND (                           -- and one of the colors is  different.
         uAmplitude > .1 + ABS(uMagSigma)
      OR gAmplitude > .1 + ABS(gMagSigma)
      OR rAmplitude > .1 + ABS(rMagSigma)
      OR iAmplitude > .1 + ABS(iMagSigma)
      OR zAmplitude > .1 + ABS(zMagSigma)
      OR yAmplitude > .1 + ABS(yMagSigma))")

 (define case02_2003_objectWithVariabilityOrPeriodOrMag "
-- Select all objects with certain variability or period or amplitude
-- http://dev.lsstcorp.org/trac/wiki/dbQuery011

-- Missing in current schema: variability

SELECT *
FROM   Object
WHERE  variability > 0.8 -- variable object
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
    OR yAmplitude > :amplitudeMin")

 (define case02_2004_objectsSimilarToQuasarsWithRedshift "
-- Find all objects similar to the colors of a quasar 
-- with redshift in a given range
-- http://dev.lsstcorp.org/trac/wiki/dbQuery012

-- Missing in current schema: ObjectToType

SELECT  COUNT(*)                                               AS totalCount,
        SUM(CASE WHEN (typeId=3) THEN 1 ELSE 0 END)            AS galaxyCount,
        SUM(CASE WHEN (typeId=6) THEN 1 ELSE 0 END)            AS starCount,
        SUM(CASE WHEN (typeId NOT IN (3,6)) THEN 1 ELSE 0 END) AS otherCount
FROM    Object
JOIN    _Object2Type USING(objectId)
WHERE  (uMag-gMag > 2.0 OR uMag > 22.3)
   AND iMag BETWEEN 0 AND 19 
   AND gMag - rMag > 1.0 
   AND ( (rMag-iMag < 0.08 + 0.42 * (gMag-rMag - 0.96)) OR (gMag-rMag > 2.26 ) )
   AND iMag-zMag < 0.25")

 (define case02_2005_varObjectsOfOneType "
-- Select all variable objects of a specific type
-- See http://dev.lsstcorp.org/trac/wiki/dbQuery002


-- missing in current schema: variability, probability

SELECT objectId
FROM   Object
JOIN   _ObjectToType USING(objectId)
JOIN   ObjectType USING (typeId)
WHERE  description = 'Supernova'
  AND  variability > 0.8
  AND  probability > 0.8

")

 (define case02_2006_randomSample "
-- Random sample of the dataq
-- http://dev.lsstcorp.org/trac/wiki/dbQuery004


SELECT fluxToAbMag(uFlux_PS), 
       fluxToAbMag(gFlux_PS), 
       fluxToAbMag(rFlux_PS), 
       fluxToAbMag(iFlux_PS), 
       fluxToAbMag(zFlux_PS), 
       fluxToAbMag(yFlux_PS)
FROM   Object 
WHERE  (objectId % 100 ) = :percentage
")

 (define case02_2010_logs "
-- interesting syntax

-- subquery - not implemented

SELECT CASE gid WHEN 1 THEN 'pipeline shutdowns seen'
                WHEN 2 THEN 'CCDs attempted'
                WHEN 3 THEN 'src writes'
                WHEN 4 THEN 'calexp writes'
       END AS descr, 
       COUNT(*) 
FROM ( SELECT CASE WHEN COMMENT LIKE 'Processing job:% visit=0 %' THEN 1
                   WHEN COMMENT LIKE 'Processing job:%' AND COMMENT NOT LIKE '% visit=0 %' THEN 2
                   WHEN COMMENT LIKE 'Ending write to BoostStorage%/src%' THEN 3
                   WHEN COMMENT LIKE 'Ending write to FitsStorage%/calexp%' THEN 4
                   ELSE 0
              END AS gid
       FROM Logs ) AS stats
WHERE gid > 0
GROUP BY gid
")

 (define case02_3001_query_035 "
-- Select object based on flux interval
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/013

SELECT  objectId
FROM    Object
WHERE   scisql_fluxToAbMag(uFlux_PS)-scisql_fluxToAbMag(gFlux_PS) <  2.0
   AND  scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS) <  0.1
   AND  scisql_fluxToAbMag(rFlux_PS)-scisql_fluxToAbMag(iFlux_PS) > -0.8
   AND  scisql_fluxToAbMag(iFlux_PS)-scisql_fluxToAbMag(zFlux_PS) <  1.4 
")

 (define case02_3002_query_030 "
-- Select object based on linear flux interval query
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013 
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/012

SELECT objectId, ra_PS, decl_PS
FROM   Object
WHERE  (                  
         scisql_fluxToAbMag(rFlux_PS)-scisql_fluxToAbMag(iFlux_PS) - 
         (scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS))/4 - 0.18
        ) BETWEEN -0.2 AND 0.2
  AND  (                 
          (
            (scisql_fluxToAbMag(rFlux_PS)-scisql_fluxToAbMag(iFlux_PS)) - 
            (scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS))/4 - 0.18 
          ) > (0.45 - 4*(scisql_fluxToAbMag(gFlux_PS)-scisql_fluxToAbMag(rFlux_PS))) 
       )
")

 (define case02_3003_query_025 "
-- Select pair of objects in dense region
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/025

SELECT DISTINCT o1.objectId, o1.ra_PS, o1.decl_PS, o2.iauId
FROM   Object o1, Object o2
WHERE  ABS(o2.ra_PS   - o1.ra_PS  ) < o2.raRange/(2*COS(RADIANS(o1.decl_PS)))
   AND ABS(o2.decl_PS - o1.decl_PS) < o2.declRange/2 
   AND (
        SELECT COUNT(o3.objectId)
        FROM   Object o3
        WHERE  o1.objectId <> o3.objectId
          AND  ABS(o1.ra_PS   - o3.ra_PS  ) < 0.1/COS(RADIANS(o3.decl_PS))
          AND  ABS(o1.decl_PS - o3.decl_PS) < 0.1
       ) > 1000
")

 (define case02_3004_query_022 "
-- Find near-neighbor objects in a given region 
-- https://dev.lsstcorp.org/trac/wiki/db/queries/022

-- Note: This does not work because table LSST is currently hard-coded in Overlap tables.

SELECT o1.objectId AS objId1, 
        o2.objectId AS objId2,
        scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) AS distance
 FROM   Object o1, 
        Object o2
 WHERE  o1.ra_PS BETWEEN 1.28 AND 1.38
   AND  o1.decl_PS BETWEEN 3.18 AND 3.34
   AND  scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) < 1
   AND  o1.objectId <> o2.objectId
")

 (define case02_3005_objectCount "
-- Count the number of Objects
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013

SELECT count(*) FROM Object
")

 (define case02_3006_selectIntervalMagnitudes "
-- Select object with Magnitude in intervals
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/013

SELECT count(*) 
 FROM   Object 
 WHERE ra_PS BETWEEN 1.28 AND 1.38
   AND decl_PS BETWEEN 3.18 AND 3.34
  AND scisql_fluxToAbMag(zFlux_PS)
      BETWEEN 21 AND 21.5  
  AND scisql_fluxToAbMag(gFlux_PS)
      - scisql_fluxToAbMag(rFlux_PS) 
      BETWEEN 0.3 AND 0.4  
  AND scisql_fluxToAbMag(iFlux_PS)
      - scisql_fluxToAbMag(zFlux_PS) 
      BETWEEN 0.1 AND 0.12
")

 (define case02_3007_countObjectWithColorFluxGreaterThan "
-- Count the number of object with a color flux greater than a constant
-- See https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/026

SELECT COUNT(*)
 FROM Object
 WHERE gFlux_PS>1e-25
")

 (define case02_3008_selectObjectWithColorMagnitudeGreaterThan "
-- Select object with IZ magnitude greater than a constant
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/051

SELECT objectId, ra_PS, decl_PS,
        uFlux_PS, gFlux_PS, rFlux_PS, 
        iFlux_PS, zFlux_PS, yFlux_PS 
 FROM Object
 WHERE scisql_fluxToAbMag(iFlux_PS)
       - scisql_fluxToAbMag(zFlux_PS) > 0.08
")

 (define case02_3009_countObjectInRegionWithZFlux "
-- Count object in a region with ZFlux in a given interval
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/020

SELECT count(*) 
FROM Object
WHERE ra_PS BETWEEN 1.28 AND 1.38
 AND  decl_PS BETWEEN 3.18 AND 3.34
 AND scisql_fluxToAbMag(zFlux_PS) BETWEEN 21 and 21.5
")

 (define case02_3010_countObjectPerChunks "
-- Count object per Chunks
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/Qserv/250Development

-- Note: chunkId field is not in MySQL tables

SELECT count(*) AS n,
        AVG(ra_PS),
        AVG(decl_PS),
        objectId,
        chunkId
 FROM Object 
 GROUP BY chunkId
")

 (define case02_3011_selectObjectWithMagnitudes "
-- Select object with magnitudes between 20 and 24
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/Qserv/OptimalPartitionSize

SELECT objectId, ra_PS, decl_PS, 
       scisql_fluxToAbMag(zFlux_PS) 
 FROM Object 
 WHERE scisql_fluxToAbMag(zFlux_PS)
       BETWEEN 20 AND 24
")

 (define case02_3012_selectObjectInCircularRegion "
-- Select object in a circular region
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013


SELECT count(*)
 FROM Object
 WHERE scisql_angSep(ra_PS, decl_PS, 0., 0.) < 0.2
")

 (define case02_3013_joinObjectSourceInRegion "
-- Join Object/Source for all object in a region
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/ticket/2052

SELECT objectId 
FROM Source 
JOIN Object USING(objectId) 
WHERE ra_PS BETWEEN 1.28 AND 1.38
 AND  decl_PS BETWEEN 3.18 AND 3.34

")

 (define case02_3014_joinObjectSourceTimeInterval "
-- Join Object and Source on a given object and for a given time interval
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/006

SELECT s.ra, s.decl
 FROM   Object o
 JOIN   Source s
 USING (objectId)
 WHERE  o.objectId = 433327840429024
 AND    o.latestObsTime BETWEEN s.taiMidPoint - 300 AND s.taiMidPoint + 300
")

 (define case02_3015_selectAllPairsWithDistanceInRegion "
-- Select all pair of objects with their distance in a given region
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Based on https://dev.lsstcorp.org/trac/wiki/db/queries/022

SELECT o1.objectId AS objId1, 
        o2.objectId AS objId2,
        scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_PS) AS distance
 FROM   Object o1, 
        Object o2
 WHERE o1.ra_PS BETWEEN 1.28 AND 1.38
   AND o1.decl_PS BETWEEN 3.18 AND 3.34
   AND o2.ra_PS BETWEEN 1.28 AND 1.38
   AND o2.decl_PS BETWEEN 3.18 AND 3.34
   AND o1.objectId <> o2.objectId
")

 (define case02_3020_selectObjectWithLimit "
-- Select 10 object in a given area
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Base on https://dev.lsstcorp.org/trac/wiki/db/Qserv/Limitations

SELECT *
 FROM Object
 WHERE ra_PS BETWEEN 1 AND 2 -- noQserv
   AND decl_PS BETWEEN 3 AND 4 -- noQserv
-- withQserv  qserv_areaspec_box(1,3,2,4)
 LIMIT 10
")

 (define case02_3021_selectObjectSortedByRA "
-- Select objects in a given area sorted by right ascension
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- Base on https://dev.lsstcorp.org/trac/ticket/2051

SELECT * 
 FROM Object
 WHERE ra_PS BETWEEN 1.28 AND 3.18   -- noQserv
   AND decl_PS BETWEEN 1.38 AND 3.34 -- noQserv
-- withQserv WHERE qserv_areaspec_box(1.28,1.38,3.18,3.34)
 ORDER BY ra_PS
")

 (define case02_3022_selectAllPairsWithinSomeDistanceOfPointsInRegion "
-- Select all pairs  within some distance of points in region
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- based on https://dev.lsstcorp.org/trac/wiki/db/queries/022

SELECT o1.objectId, o2.objectId
FROM Object o1, Object o2 
WHERE o1.ra_PS BETWEEN 0.04 AND 5.  -- noQserv
  AND o1.decl_PS BETWEEN -3. AND 3. -- noQserv
-- withQserv WHERE   qserv_areaspec_box(0.04, 5., -3., 3.)
  AND scisql_angSep(o1.ra_PS, o1.decl_PS, o2.ra_PS, o2.decl_Ps) < 5.

")

 (define case02_3023_joinObjectSourceFilter "
-- Join on Source and Filter and select specific filter in region
-- https://dev.lsstcorp.org/trac/wiki/db/Qserv/IN2P3/BenchmarkMarch2013
-- https://dev.lsstcorp.org/trac/wiki/db/queries/007

SELECT objectId, taiMidPoint, fluxToAbMag(psfMag)
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
WHERE   ra_PS BETWEEN 1 AND 2 -- noQserv
   AND decl_PS BETWEEN 3 AND 4 -- noQserv
-- withQserv  qserv_areaspec_box(1,3,2,4)
   AND  filterName = 'u'
   AND  variability BETWEEN 0 AND 2
ORDER BY objectId, taiMidPoint 
")

 (define case02_8001_badLiteral "


-- note wrong literal: \"35 .1\" instead of \"35.1\"

SELECT count(*) 
FROM Object
WHERE ra_PS BETWEEN 35 AND 35 .1 -- noQserv
  AND decl_PS BETWEEN 6 AND 6.0001 -- noQserv
-- withQserv WHERE qserv_areaSpec_box(35, 6, 35 .1, 6.0001);

")

 (define case02_8002_badLiteral "


-- note wrong literal: \"35. 1\" instead of \"35.1\"

SELECT count(*) 
FROM Object
WHERE ra_PS BETWEEN 35 AND 35. 1 -- noQserv
  AND decl_PS BETWEEN 6 AND 6.0001 -- noQserv
-- withQserv WHERE qserv_areaSpec_box(35, 6, 35. 1, 6.0001);

")

 (define case02_8003_areaWithLimitClause "
-- This is testing syntax (limit after areaspec)

-- See ticket #2200

SELECT COUNT(*) 
FROM   Object 
WHERE  ra_PS BETWEEN 355 AND 356 -- noQserv
  AND  decl_PS BETWEEN 0 AND 1   -- noQserv
-- withQserv WHERE qserv_areaspec_box(355, 0, 356, 1)
LIMIT 10
")

 '(define case03_0001_showColumnsFromSource "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries 

SHOW COLUMNS FROM DeepSource;
")

 (define case03_0002.1_fetchRunAndFieldById "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries 

SELECT sce.filterName, sce.field, sce.camcol, sce.run 
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.filterName like '%') 
   AND (sce.field = 535)
   AND (sce.camcol like '%')
   AND (sce.run = 94);
")

 (define case03_0002.2_fetchRunAndFieldById "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries 

-- note, filterName is selected twice, is that needed???
-- see #2758, it is confusing qserv
SELECT sce.scienceCcdExposureId, sce.filterName, sce.field, sce.camcol, sce.run,
       sce.filterId, sce.ra, sce.decl, sce.crpix1, sce.crpix2, 
       sce.crval1, sce.crval2, sce.cd1_1, sce.cd1_2, sce.cd2_1, sce.cd2_2, 
       sce.fluxMag0, sce.fluxMag0Sigma, sce.fwhm  
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.filterName = 'g')
   AND (sce.field = 535) 
   AND (sce.camcol = 1)
   AND (sce.run = 94);
")

 (define case03_0002_fetchRunAndFieldById "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries 

SELECT distinct run, field 
FROM   Science_Ccd_Exposure
WHERE  (run = 94) AND (field = 535);
")

 (define case03_0003.1_SourcesForGivenExposure "
-- Return empty set with W13 small dataset
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries 

SELECT sce.filterName, sce.field, sce.camcol, sce.run, s.deepForcedSourceId, s.ra, s.decl, 
       s.x, s.y, s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.modelFlux, 
       s.modelFluxSigma, s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, 
       s.shapeIxy, s.flagPixInterpCen, s.flagNegative, s.flagPixEdge, 
       s.flagBadCentroid, s.flagPixSaturCen, s.extendedness  
FROM   DeepForcedSource AS s,
       Science_Ccd_Exposure AS sce
WHERE  (s.scienceCcdExposureId = sce.scienceCcdExposureId)
")

 (define case03_0003_SourcesForGivenExposure "
-- Return empty set with W13 small dataset
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries 

SELECT sce.filterName, sce.field, sce.camcol, sce.run, s.deepForcedSourceId, s.ra, s.decl, 
       s.x, s.y, s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.modelFlux, 
       s.modelFluxSigma, s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, 
       s.shapeIxy, s.flagPixInterpCen, s.flagNegative, s.flagPixEdge, 
       s.flagBadCentroid, s.flagPixSaturCen, s.extendedness  
FROM   DeepForcedSource AS s,
       Science_Ccd_Exposure AS sce
WHERE  (s.scienceCcdExposureId = sce.scienceCcdExposureId)
   AND (sce.filterName = 'g')
   AND (sce.field = 535)
   AND (sce.camcol = 1)
   AND (sce.run = 94);
")

 (define case03_0004_SourceExposureWithFilternameAndField "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterName, sce.field, sce.camcol, sce.run, s.deepForcedSourceId,
       s.ra, s.decl, s.x, s.y, s.psfFlux, s.psfFluxSigma, s.apFlux,
       s.apFluxSigma, s.modelFlux, 
       s.modelFluxSigma, s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, 
       s.shapeIxy, s.flagPixInterpCen, s.flagNegative, s.flagPixEdge, 
       s.flagBadCentroid, s.flagPixSaturCen, s.extendedness  
FROM   DeepForcedSource AS s,
       Science_Ccd_Exposure AS sce
WHERE  (s.scienceCcdExposureId = sce.scienceCcdExposureId)
   AND (sce.filterName = 'g')
   AND (sce.field = 793)
   AND (sce.camcol = 1)
   AND (sce.run = 5924)
;
")

;; MySQL-ism: not supported by the standard
 '(define case03_0005_showColumnsFromRefObject "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SHOW COLUMNS FROM RefObject;
")

 (define case03_0006_selectExposure "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterName, sce.field, sce.camcol, sce.run  
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.filterName = 'g')
   AND (sce.field = 670)
   AND (sce.camcol = 2)
   AND (sce.run = 7202)
;

")

 '(define case03_0007_selectExposureWithPoly "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT scisql_s2CPolyToBin(54.96, -0.64, 55.12, -0.64,
                           55.12, -0.41, 54.96, -0.41) 
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.filterName = 'g')
   AND (sce.field = 670)
   AND (sce.camcol = 2)
   AND (sce.run = 7202) INTO @poly;
")

 '(define case03_0008_selectRefObjectInPoly "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SET @poly = scisql_s2CPolyToBin(54.9, -1.25,
                                55.0, -1.25,
                                55.0, -0.75,
                                54.9, -0.75);

SELECT sro.refObjectId, sro.isStar, sro.ra, sro.decl, sro.uMag, sro.gMag, 
       sro.rMag, sro.iMag, sro.zMag 
FROM   RefObject AS sro 
WHERE  (scisql_s2PtInCPoly(sro.ra, sro.decl, @poly) = 1);
")

 (define case03_0009_selectCCDExposure "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterId, sce.filterName 
FROM   Science_Ccd_Exposure AS sce 
WHERE  (sce.filterName = 'g')
   AND (sce.field = 670)
   AND (sce.camcol = 2)
   AND (sce.run = 7202)
;
")

 (define case03_0010_selectSource_RefSrcMatch_RefObject "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

-- Nothing because
-- select count(*)
-- from DeepForcedSource AS s,
--      RefDeepSrcMatch AS rom
-- where (s.deepForcedSourceId = rom.deepSourceId);
-- is empty

SELECT sce.filterName, sce.field, sce.camcol, sce.run, sro.gMag, 
       sro.isStar, sro.refObjectId, s.deepForcedSourceId,  rom.nSrcMatches,s.ra, s.decl, 
       s.x, s.y, s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.modelFlux, 
       s.modelFluxSigma, s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, 
       s.shapeIxy, s.flagPixInterpCen, s.flagNegative, s.flagPixEdge, 
       s.flagBadCentroid, s.flagPixSaturCen, s.extendedness
FROM   DeepForcedSource AS s, Science_Ccd_Exposure AS sce,
       RefDeepSrcMatch AS rom,
       RefObject AS sro 
WHERE  (s.scienceCcdExposureId = sce.scienceCcdExposureId)
   AND (s.deepForcedSourceId = rom.deepSourceId)
   AND (rom.refObjectId = sro.refObjectId)
   AND (sce.filterName = 'g')
   AND (sce.field = 670)
   AND (sce.camcol = 2)
   AND (sce.run = 7202)
;
")

 (define case03_0011_selectDeepCoadd "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT DISTINCT tract,patch,filterName
FROM DeepCoadd
;
")

 (define case03_0012_selectDistinctDeepCoaddWithGivenTractPatchFiltername "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT DISTINCT tract, patch, filterName 
FROM   DeepCoadd
WHERE  (tract = 0) 
   AND (patch = '159,2')
   AND (filterName = 'r');

")

 (define case03_0013_selectDeepCoadd2 "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterName, sce.tract, sce.patch
FROM   DeepCoadd AS sce
WHERE  (sce.filterName = 'r')
   AND (sce.tract = 0)
   AND (sce.patch = '159,3');

")

 (define case03_0014.1_selectDeepCoadd3 "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.DeepCoaddId, sce.filterName, sce.tract, sce.patch, sce.filterId, 
       sce.filterName, sce.ra, sce.decl, sce.crpix1, sce.crpix2, sce.crval1, 
       sce.crval2, sce.cd1_1, sce.cd1_2, sce.cd2_1, sce.cd2_2, sce.fluxMag0, 
       sce.fluxMag0Sigma, sce.measuredFwhm  
FROM   DeepCoadd AS sce
WHERE  (sce.filterName = 'r')
   AND (sce.tract = 0)
   AND (sce.patch = '159,2');

")

 (define case03_0014_selectDeepCoadd3 "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.DeepCoaddId, sce.filterName, sce.tract, sce.patch, sce.filterId, 
       sce.ra, sce.decl, sce.crpix1, sce.crpix2, sce.crval1, 
       sce.crval2, sce.cd1_1, sce.cd1_2, sce.cd2_1, sce.cd2_2, sce.fluxMag0, 
       sce.fluxMag0Sigma, sce.measuredFwhm  
FROM   DeepCoadd AS sce
WHERE  (sce.filterName = 'r')
   AND (sce.tract = 0)
   AND (sce.patch = '159,2');

")

 (define case03_0015_selectDeepSourceDeepCoadd "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterName, sce.tract, sce.patch, s.deepSourceId, s.ra, s.decl, 
       s.x, s.y, s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.modelFlux, 
       s.modelFluxSigma, s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, 
       s.shapeIxy, s.flagPixInterpCen, s.flagNegative, s.flagPixEdge, 
       s.flagBadCentroid, s.flagPixSaturCen, s.extendedness
FROM   DeepSource AS s, 
       DeepCoadd AS sce
WHERE  (s.deepCoaddId = sce.deepCoaddId)
   AND (sce.filterName = 'r')
   AND (sce.tract = 0)
   AND (sce.patch = '159,2');

")

 '(define case03_0016_selectDeepCoaddInPoly "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT scisql_s2CPolyToBin(sce.corner1Ra, sce.corner1Decl, sce.corner2Ra, sce.corner2Decl,
                           sce.corner3Ra, sce.corner3Decl, sce.corner4Ra, sce.corner4Decl)
FROM   DeepCoadd AS sce
WHERE  (sce.filterName = 'g')
   AND (sce.tract = 0)
   AND (sce.patch = '159,1') INTO @poly;

")

 (define case03_0017_selectRefObjectInPoly "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sro.refObjectId, sro.isStar, sro.ra, sro.decl, sro.uMag, sro.gMag, 
       sro.rMag, sro.iMag, sro.zMag
FROM   RefObject AS sro
WHERE  (scisql_s2PtInCPoly(sro.ra, sro.decl, @poly) = 1);

")

 (define case03_0018_selectDeepCoaddWithGivenTractPatchFiltername "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterId, sce.filterName
FROM   DeepCoadd AS sce
WHERE  (sce.filterName = 'r') 
   AND (sce.tract = 0)
   AND (sce.patch = '159,1');

")

 (define case03_0019.1_selectDeepsourceDeepcoaddDeepsrcmatchRefobject "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterName, sce.tract, sce.patch, sro.gMag, sro.ra, sro.decl, sro.isStar,
       sro.refObjectId, s.deepSourceId,  rom.nSrcMatches,s.ra, s.decl, s.x, s.y, 
       s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.modelFlux, s.modelFluxSigma, 
       s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, s.shapeIxy, s.flagPixInterpCen, 
       s.flagNegative, s.flagPixEdge, s.flagBadCentroid, s.flagPixSaturCen, s.extendedness
FROM   DeepSource AS s, 
       DeepCoadd AS sce,
       RefDeepSrcMatch AS rom,
       RefObject AS sro
WHERE  (s.deepCoaddId = sce.deepCoaddId)
   AND (s.deepSourceId = rom.deepSourceId)
   AND (rom.refObjectId = sro.refObjectId)
   AND (sce.filterName = 'r')
   AND (sce.tract = 0)
   AND (sce.patch = '159,3');

")

 (define case03_0019_selectDeepsourceDeepcoaddDeepsrcmatchRefobject "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterName, sce.tract, sce.patch, sro.gMag, sro.ra, sro.decl, sro.isStar,
       sro.refObjectId, s.deepSourceId,  rom.nSrcMatches, s.x, s.y, 
       s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.modelFlux, s.modelFluxSigma, 
       s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, s.shapeIxy, s.flagPixInterpCen, 
       s.flagNegative, s.flagPixEdge, s.flagBadCentroid, s.flagPixSaturCen, s.extendedness
FROM   DeepSource AS s, 
       DeepCoadd AS sce,
       RefDeepSrcMatch AS rom,
       RefObject AS sro
WHERE  (s.deepCoaddId = sce.deepCoaddId)
   AND (s.deepSourceId = rom.deepSourceId)
   AND (rom.refObjectId = sro.refObjectId)
   AND (sce.filterName = 'r')
   AND (sce.tract = 0)
   AND (sce.patch = '159,3');

")

;; MySQL-ism: not supported by standard
 '(define case03_0020_showColumnsFromDeepforcedsource "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SHOW COLUMNS FROM DeepForcedSource;
")

 (define case03_0021_selectScienceCCDExposure "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries


SELECT distinct run, field
FROM   Science_Ccd_Exposure
WHERE  (run = 94)
   AND (field = 535);
")

 (define case03_0022_selectScienceCCDExposureWithFilternameFieldCamcolRun "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries


SELECT sce.filterName, sce.field, sce.camcol, sce.run
FROM   Science_Ccd_Exposure AS sce 
WHERE  (sce.filterName like '%')
   AND (sce.field = 535)
   AND (sce.camcol like '%')
   AND (sce.run = 94);
")

 (define case03_0023_selectScienceCCDExposureWithFilternameFieldCamcolRun "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries


SELECT sce.scienceCcdExposureId, sce.field, sce.camcol, sce.run,
       sce.filterId, sce.filterName, sce.ra, sce.decl, sce.crpix1, sce.crpix2, 
       sce.crval1, sce.crval2, sce.cd1_1, sce.cd1_2, sce.cd2_1, sce.cd2_2, 
       sce.fluxMag0, sce.fluxMag0Sigma, sce.fwhm
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.filterName = 'g')
   AND (sce.field = 535)
   AND (sce.camcol = 1)
   AND (sce.run = 94);
")

 (define case03_0024_selectDeepForcedSourceScienceCCDExposureWithFilternameFieldCamcolRun "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries


SELECT sce.filterName, sce.field, sce.camcol, sce.run, s.deepSourceId, s.ra, s.decl, 
       s.x, s.y, s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.modelFlux, 
       s.modelFluxSigma, s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, 
       s.shapeIxy, s.flagPixInterpCen, s.flagNegative, s.flagPixEdge, s.flagBadCentroid, 
       s.flagPixSaturCen, s.extendedness
FROM   DeepForcedSource AS s,
       Science_Ccd_Exposure AS sce
WHERE  (s.scienceCcdExposureId = sce.scienceCcdExposureId)
   AND (sce.filterName = 'g')
   AND (sce.field = 535)
   AND (sce.camcol = 1)
   AND (sce.run = 94);
")

 (define case03_0025_selectScienceCCDExposureWithFilternameFieldCamcolRun "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterName, sce.field, sce.camcol, sce.run
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.filterName = 'g')
   AND (sce.field = 535)
   AND (sce.camcol = 1)
   AND (sce.run = 94);
")

;; Mutation not supported: 'INTO @poly'
 '(define case03_0026_selectScienceCCDExposureInPoly "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT scisql_s2CPolyToBin(sce.corner1Ra, sce.corner1Decl, sce.corner2Ra, sce.corner2Decl,
                           sce.corner3Ra, sce.corner3Decl, sce.corner4Ra, sce.corner4Decl) 
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.filterName = 'g')
   AND (sce.field = 535)
   AND (sce.camcol = 1)
   AND (sce.run = 94) INTO @poly;
")

 '(define case03_0027_selectRefObjectInPoly "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SET @poly = scisql_s2CPolyToBin(54.9, -1.25,
                                55.0, -1.25,
                                55.0, -0.75,
                                54.9, -0.75);

SELECT sro.refObjectId, sro.isStar, sro.ra, sro.decl, sro.uMag, 
       sro.gMag, sro.rMag, sro.iMag, sro.zMag
FROM   RefObject AS sro
WHERE  (scisql_s2PtInCPoly(sro.ra, sro.decl, @poly) = 1) ;
")

 (define case03_0028_selectScienceCCDExposure "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries


SELECT sce.filterId, sce.filterName
FROM   Science_Ccd_Exposure AS sce
WHERE  (sce.filterName = 'g')
   AND (sce.field = 535)
   AND (sce.camcol = 1)
   AND (sce.run = 94);
")

 '(define case03_0029_selectDeepforcedsourceScienceCCDExposureRefdeepsrcmatchRefobject "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT sce.filterName, sce.field, sce.camcol, sce.run, sro.gMag, sro.ra, sro.decl,
       sro.isStar, sro.refObjectId, s.deepSourceId,  rom.nSrcMatches,s.ra, s.decl, 
       s.x, s.y, s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.modelFlux, 
       s.modelFluxSigma, s.instFlux, s.instFluxSigma, s.shapeIxx, s.shapeIyy, 
       s.shapeIxy, s.flagPixInterpCen, s.flagNegative, s.flagPixEdge, 
       s.flagBadCentroid, s.flagPixSaturCen, s.extendedness
FROM   DeepForcedSource AS s,
       Science_Ccd_Exposure AS sce use index(),
       RefDeepSrcMatch AS rom,
       RefObject AS sro
WHERE  (s.scienceCcdExposureId = sce.scienceCcdExposureId)
   AND (s.deepSourceId = rom.deepSourceId)
   AND (rom.refObjectId = sro.refObjectId)
   AND (sce.filterName = 'g')
   AND (sce.field = 794)
   AND (sce.camcol = 1)
   AND (sce.run = 5924);
")

 (define case03_0030_selectScienceCCDExposureByRunField "
-- https://dev.lsstcorp.org/trac/wiki/dbPipeQAQueries

SELECT distinct run, field 
FROM   Science_Ccd_Exposure
WHERE  (run = 94) AND (field = 536);

")

    
 (define QSERV_queries
   (list
    case01_0001_fetchObjectById 
    case01_0002_fetchObjectByIdNoResult 
    case01_0003_selectMetadataForOneGalaxy 
    case01_0004_lightCurve 
    case01_0005_nonReplicatedTable 
    case01_0006_transientVarObjNearGalaxy 
    case01_0010_leapSec 
    ;; case01_0011_sdqaMetric   ;; Contains MySQL non standard SQL construct
    case01_0012_raftAndCcd 
    case01_1002_coneMagColor 
    case01_1003_coneMagColorEmptyRes 
    case01_1004_varObjects 
    case01_1005_allGalaxiesInArea 
    case01_1011_objectsForExposure 
    case01_1012_orderByClause 
    case01_1013_orderByClauseRounded 
    case01_1030_timeSeries 
    case01_1031_newTransientsForEpoch 
    case01_1051_nn 
    case01_1052_nnSimilarColors 
    ;; case01_1070_areaUsingPoly 
    case01_1080_refMatch1 
    case01_1081_refMatch2 
    case01_1083_refMatch3 
    case01_2001_fullObjectScan 
    case01_2002_findStarsWithMultiMeasAndMagVariation 
    case01_2003_objectWithVariabilityOrPeriodOrMag 
    case01_2004_objectsSimilarToQuasarsWithRedshift 
    case01_2005_varObjectsOfOneType 
    case01_2006_randomSample 
    case01_2010_logs 
    case01_2100_groupByChunkId 
    ;; case01_3001_SelectInPoly      ;; Mutation not allowed
    case01_3002_sameColumnName 
    case01_3003_SameColumnTwice 
    case01_3004_nonExistingColumn 
    case01_3005_orderByRA 
    case01_3006_selectAs 
    case01_3007_countGroupBy 
    ;; case01_3008_showColumns   ;; SHOW not supported
    case01_3009_subquery 
    case01_3010_selectWithComputation 
    case01_3011_selectSample 
    case01_3012_similarObject 
    case01_3013_nonexistantTable 
    case01_3014_limitAfterAreaspec 
    case01_3015_orderByFunction 
    case01_3016_selectAllPairsWithinDistance 
    ;; case01_8001_badLiteral   ;; bad syntax test
    ;; case01_8002_badLiteral   ;; bad syntax test
    case01_8003_areaWithLimitClause 
    case01_8004_badArea 
    case02_0001_fetchObjectById 
    case02_0002_fetchObjectByIdNoResult 
    case02_0003_selectMetadataForOneGalaxy 
    case02_0004_fetchSourceById 
    case02_0006_transientVarObjNearGalaxy 
    case02_1002_coneMagColor 
    case02_1003_coneMagColorEmptyRes 
    case02_1004_varObjects 
    case02_1005_allGalaxiesInArea 
    case02_1011_objectsForExposure 
    case02_1012_orderByClause 
    case02_1013_orderByClauseRounded 
    case02_1030_timeSeries 
    case02_1031_newTransientsForEpoch 
    case02_1051_nn 
    case02_1052_nnSimilarColors 
    ;; case02_1070_areaUsingPoly 
    case02_1080_refMatch1 
    case02_1081_refMatch2 
    case02_1083_refMatch3 
    case02_2001_fullObjectScan 
    case02_2002_findStarsWithMultiMeasAndMagVariation 
    case02_2003_objectWithVariabilityOrPeriodOrMag 
    case02_2004_objectsSimilarToQuasarsWithRedshift 
    case02_2005_varObjectsOfOneType 
    case02_2006_randomSample 
    case02_2010_logs 
    case02_3001_query_035 
    case02_3002_query_030 
    case02_3003_query_025 
    case02_3004_query_022 
    case02_3005_objectCount 
    case02_3006_selectIntervalMagnitudes 
    case02_3007_countObjectWithColorFluxGreaterThan 
    case02_3008_selectObjectWithColorMagnitudeGreaterThan 
    case02_3009_countObjectInRegionWithZFlux 
    case02_3010_countObjectPerChunks 
    case02_3011_selectObjectWithMagnitudes 
    case02_3012_selectObjectInCircularRegion 
    case02_3013_joinObjectSourceInRegion 
    case02_3014_joinObjectSourceTimeInterval 
    case02_3015_selectAllPairsWithDistanceInRegion 
    case02_3020_selectObjectWithLimit 
    case02_3021_selectObjectSortedByRA 
    case02_3022_selectAllPairsWithinSomeDistanceOfPointsInRegion 
    case02_3023_joinObjectSourceFilter 
    ;; case02_8001_badLiteral ;; bad syntax test
    ;; case02_8002_badLiteral ;; bad syntax test
    case02_8003_areaWithLimitClause 
    ;; case03_0001_showColumnsFromSource     ;; SHOW COLUMNS not supported (MySQL specific)
    case03_0002.1_fetchRunAndFieldById 
    case03_0002.2_fetchRunAndFieldById 
    case03_0002_fetchRunAndFieldById 
    case03_0003.1_SourcesForGivenExposure 
    case03_0003_SourcesForGivenExposure 
    case03_0004_SourceExposureWithFilternameAndField 
    ;; case03_0005_showColumnsFromRefObject  ;; SHOW COLUMNS not supported (MySQL specific)
    case03_0006_selectExposure 
    ;; case03_0007_selectExposureWithPoly    ;; "SELECT INTO"
    ;; case03_0008_selectRefObjectInPoly     ;; Mutation not allowed
    case03_0009_selectCCDExposure 
    case03_0010_selectSource_RefSrcMatch_RefObject 
    case03_0011_selectDeepCoadd 
    case03_0012_selectDistinctDeepCoaddWithGivenTractPatchFiltername 
    case03_0013_selectDeepCoadd2 
    case03_0014.1_selectDeepCoadd3 
    case03_0014_selectDeepCoadd3 
    case03_0015_selectDeepSourceDeepCoadd 
    ;; case03_0016_selectDeepCoaddInPoly   ;; "SELECT INTO"
    case03_0017_selectRefObjectInPoly 
    case03_0018_selectDeepCoaddWithGivenTractPatchFiltername 
    case03_0019.1_selectDeepsourceDeepcoaddDeepsrcmatchRefobject 
    case03_0019_selectDeepsourceDeepcoaddDeepsrcmatchRefobject 
    ;; case03_0020_showColumnsFromDeepforcedsource    ;; SHOW COLUMNS not supported (MySQL specific)
    case03_0021_selectScienceCCDExposure 
    case03_0022_selectScienceCCDExposureWithFilternameFieldCamcolRun 
    case03_0023_selectScienceCCDExposureWithFilternameFieldCamcolRun 
    case03_0024_selectDeepForcedSourceScienceCCDExposureWithFilternameFieldCamcolRun 
    case03_0025_selectScienceCCDExposureWithFilternameFieldCamcolRun 
    ;; case03_0026_selectScienceCCDExposureInPoly    ;; "SELECT INTO"
    ;; case03_0027_selectRefObjectInPoly       ;; Mutation not allowed
    case03_0028_selectScienceCCDExposure 
    ;; case03_0029_selectDeepforcedsourceScienceCCDExposureRefdeepsrcmatchRefobject   ;; "use index()" is MySQL specific
    case03_0030_selectScienceCCDExposureByRunField 
    ))

(define (QSERV_queries:example-without-printing)
  (display "Converting ") (display (length QSERV_queries))
  (display " QSERV test queries to AST.") (newline)
  (for-each SQL->AST QSERV_queries))
