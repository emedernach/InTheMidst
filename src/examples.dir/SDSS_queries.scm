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


;;
;; From http://cas.sdss.org/dr4/en/help/docs/realquery.asp
;;

;; Queries with comments need to be read with the SQL_reader function.

;; ---------------------------------------- ;;

(define SDSS_query_001
  "SELECT objID, 	-- Get the unique object ID,
    field, ra, dec 	-- the field number, and coordinates
FROM PhotoObj		-- From the photometric data
WHERE run=1336 and field = 11 	-- that matches our criteria")

;; "SELECT TOP" is specific to MySQL and is not standard !
(define SDSS_query_002_original
  "SELECT TOP 1000 objID
FROM Galaxy
WHERE
    r < 22 	-- r IS NOT deredenned
    and extinction_r > 0.175 	-- extinction more than 0.175 ")

;; This one is standard SQL and should work.
(define SDSS_query_002
  "SELECT objID
FROM Galaxy
WHERE
    r < 22 	-- r IS NOT deredenned
    and extinction_r > 0.175 	-- extinction more than 0.175
LIMIT 1000 ;")

;; TODO: qualified functions are not currently supported
(define SDSS_query_003
  "SELECT specObjID
FROM SpecObj
WHERE SpecClass = dbo.fSpecClass('UNKNOWN')
;")

(define SDSS_query_004
  "SELECT objID
FROM Galaxy
WHERE ra between 250 and 270
    and dec > 50 
    and (g+rho) between 23 and 25 	-- g is blue magnitude, and rho= 5*log(r)
;")

(define SDSS_query_005
  "SELECT colc_g, colc_r
FROM Galaxy
WHERE (-0.642788 * cx + 0.766044 * cy>=0)
    and (-0.984808 * cx - 0.173648 * cy <0) 
;")

(define SDSS_query_006
  "SELECT run,
    camCol, 
    rerun, 
    field, 
    objID, 
    u, g, r, i, z, 
    ra, dec 	-- Just get some basic quantities
FROM PhotoPrimary		-- From all primary detections, regardless of class
WHERE u - g < 0.4
    and g - r < 0.7 
    and r - i > 0.4 
    and i - z > 0.4 	-- that meet the color criteria
;")

;; Hexadecimal litterals '0x..' are not supported, instead use '#x..'
(define SDSS_query_007
  "SELECT u, g, r, i, z FROM Galaxy
WHERE htmid*37 & #x000000000000FFFF < (650 * 1)
;")

(define SDSS_query_008
  "SELECT
    g, 
    run, 
    rerun, 
    camcol, 
    field, 
    objID 
FROM
    Galaxy 
WHERE ( (g <= 22)
    and (u - g >= -0.27) 
    and (u - g < 0.71) 
    and (g - r >= -0.24) 
    and (g - r < 0.35) 
    and (r - i >= -0.27) 
    and (r - i < 0.57) 
    and (i - z >= -0.35) 
    and (i - z < 0.70) )
;")

(define SDSS_query_009
  "SELECT 
    run, 
    camCol, 
    field, 
    objID, 
    rowC, colC, rowV, colV, rowVErr, colVErr, 
    flags, 
    psfMag_u, psfMag_g, psfMag_r, psfMag_i, psfMag_z, 
    psfMagErr_u, psfMagErr_g, psfMagErr_r, psfMagErr_i, psfMagErr_z 
FROM PhotoPrimary
WHERE
    -- where the velocities are reliable 
    power(rowv, 2) / power(rowvErr, 2) + 
    power(colv, 2) / power(colvErr, 2) > 4
LIMIT 1000 ;")

(define SDSS_query_010
  "SELECT objID, r, rho, isoA_r
FROM Galaxy
WHERE
    r + rho < 24 	-- red surface brightness more than
	-- 24 mag/sq-arcsec
    and isoA_r between 30/0.4 and 60/0.4 	-- major axis between 30\" and 60\"
	-- (SDSS pixels = 0.4 arcsec)
    and (power(q_r,2) + power(u_r,2)) > 0.25 	-- square of ellipticity > 0.5 squared
LIMIT 10 ;")

(define SDSS_query_011
  "SELECT
    objID, 
    sqrt( power(rowv,2) + power(colv, 2) ) as velocity 
FROM PhotoObj
WHERE
    (power(rowv,2) + power(colv, 2)) > 50 
    and rowv >= 0 and colv >=0 
;")

(define SDSS_query_012
  "SELECT run,
    camCol, 
    rerun, 
    field, 
    objID, 
    u, g, r, i, z, 
    ra, dec 
FROM Star 	-- or Galaxy
WHERE ( u - g > 2.0 or u > 22.3 )
    and ( i < 19 ) 
    and ( i > 0 ) 
    and ( g - r > 1.0 ) 
    and ( r - i < (0.08 + 0.42 * (g - r - 0.96)) or g - r > 2.26 ) 
    and ( i - z < 0.25 ) 
;")

(define SDSS_query_013
  "SELECT count(*) as 'total',
    sum( case when (Type=3) then 1 else 0 end) as 'Galaxies', 
    sum( case when (Type=6) then 1 else 0 end) as 'Stars', 
    sum( case when (Type not in (3,6)) then 1 else 0 end) as 'Other' 
FROM PhotoPrimary	-- for each object
WHERE (( u - g > 2.0) or (u > 22.3) ) -- apply the quasar color cut.
    and ( i between 0 and 19 ) 
    and ( g - r > 1.0 ) 
    and ( (r - i < 0.08 + 0.42 * (g - r - 0.96)) or (g - r > 2.26 ) ) 
    and ( i - z < 0.25 ) 
;")

(define SDSS_query_013_a
  "SELECT count(*)
FROM PhotoPrimary	-- for each object
WHERE (( u - g > 2.0) or (u > 22.3) ) -- apply the quasar color cut.
    and ( i between 0 and 19 ) 
    and ( g - r > 1.0 ) 
    and ( (r - i < 0.08 + 0.42 * (g - r - 0.96)) or (g - r > 2.26 ) ) 
    and ( i - z < 0.25 ) 
;")

(define SDSS_query_014
  "SELECT G.ObjID, G.u, G.g, G.r, G.i, G.z 	-- get the ObjID and final mags
FROM Galaxy AS G 	-- use two Views, Galaxy and Star, as a
    JOIN Star AS S 	-- convenient way to compare objects
       ON G.parentID = S.parentID 	-- JOIN condition: star has same parent
WHERE G.parentID > 0 	-- galaxy has a \"parent\", which tells us this
	-- object was deblended
LIMIT 10
;")

(define SDSS_query_015
  "SELECT
    s.psfMag_g, 	-- or whatever you want from each object
    s.run, 
    s.camCol, 
    s.rerun, 
    s.field 
FROM Star AS s
    JOIN Field AS f ON s.fieldID = f.fieldID 
WHERE s.psfMag_g < 20
    and f.pspStatus = 2 
;")

(define SDSS_query_016
  "SELECT
    g.run, 
    g.rerun, 
    g.camCol, 
    f.field, 
    p.objID, 
    p.ra, 
    p.dec, 
    p.Rowc, 
    p.Colc, 
    p.u, 
    p.modelMagErr_u , 
    p.g, 
    p.modelMagErr_g, 
    p.r, 
    p.modelMagErr_r, 
    p.petroMag_r - p.extinction_r, 
    p.petroMagErr_r, 
    p.i, 
    p.modelMagErr_i, 
    p.z, 
    p.status & 0x00002000, 
    f.psfWidth_r 
FROM
    PhotoObj AS p 
    JOIN Field AS f ON f.fieldid = p.fieldid 
    JOIN Segment AS g ON f.segmentid = g.segmentid 
WHERE
    g.run = 1336 and g.camCol = 1 
    and f.field between 11 and 13 
    and f.psfWidth_r > 1.2 
    and p.colc > 400.0
;")

;; Qualified function names such as "dbo.fField" are not supported
;; We replaced it with dbo_fField
(define SDSS_query_017
  "SELECT
    obj.run, obj.camCol, str(obj.field, 3) as field, 
    str(obj.rowc, 6, 1) as rowc, str(obj.colc, 6, 1) as colc, 
    str(dbo_fObj(obj.objId), 4) as id, 
    str(obj.psfMag_g - 0*obj.extinction_g, 6, 3) as g, 
    str(obj.psfMag_r - 0*obj.extinction_r, 6, 3) as r, 
    str(obj.psfMag_i - 0*obj.extinction_i, 6, 3) as i, 
    str(obj.psfMag_z - 0*obj.extinction_z, 6, 3) as z, 
    str(60*distance, 3, 1) as D, 
    dbo_fField(neighborObjId) as nfield, 
    str(dbo_fObj(neighborObjId), 4) as nid,'new' as 'new'
    FROM 
    (SELECT obj.objId, 
       run, camCol, field, rowc, colc, 
       psfMag_u, extinction_u, 
       psfMag_g, extinction_g, 
       psfMag_r, extinction_r, 
       psfMag_i, extinction_i, 
       psfMag_z, extinction_z, 
       NN.neighborObjId, NN.distance 
    FROM PhotoObj as obj 
      JOIN neighbors as NN on obj.objId = NN.objId 
    WHERE 
       60*NN.distance between 0 and 15 and 
       NN.mode = 1 and NN.neighborMode = 1 and 
       run = 756 and camCol = 5 and 
       obj.type = 6 and (obj.flags & 0x40006) = 0 and 
       nchild = 0 and obj.psfMag_i < 20 and 
       (g - r between 0.3 and 1.1 and r - i between -0.1 and 0.6) 
    ) as obj
    JOIN PhotoObj as nobj on nobj.objId = obj.neighborObjId
    WHERE 
    nobj.run = obj.run and 
    (abs(obj.psfMag_g - nobj.psfMag_g) < 0.5 or 
    abs(obj.psfMag_r - nobj.psfMag_r) < 0.5 or 
    abs(obj.psfMag_i - nobj.psfMag_i) < 0.5)
    ORDER BY obj.run, obj.camCol, obj.field
;")

(define SDSS_query_018
  "SELECT specObjID, 	-- get the spectroscopic object id
    z, zConf, 	-- redshift, redshift confidence
    SpecClass 	-- and spectral classification
FROM SpecObj 	-- from the spectroscopic objects
WHERE
    -- use a function to translate SpecClass bits to names; want quasars 
    (SpecClass=dbo_fSpecClass('QSO') 
    or SpecClass=dbo_fSpecClass('HIZ_QSO')) 
    -- and the redshift is 2.5 to 2.7. Remember, z is redshift in SpecObj. 
    and z between 2.5 and 2.7 
    -- and we have a high confidence redshift estimate. 
    and zConf > 0.90 
;")

(define SDSS_query_019
  "SELECT P.ObjID 	-- distinct cases
FROM PhotoPrimary AS P 	-- P is the primary object
    JOIN Neighbors AS N ON P.ObjID = N.ObjID 	-- N is the neighbor link
    JOIN PhotoPrimary AS L ON L.ObjID = N.NeighborObjID 
	-- L is the lens candidate of P
WHERE 	
    P.ObjID < L. ObjID 	-- avoid duplicates
    and abs((P.u-P.g)-(L.u-L.g))<0.05 	-- L and P have similar spectra.
    and abs((P.g-P.r)-(L.g-L.r))<0.05 	
    and abs((P.r-P.i)-(L.r-L.i))<0.05 	
    and abs((P.i-P.z)-(L.i-L.z))<0.05
LIMIT 10 ;")

(define SDSS_query_020
  "SELECT 
    run, 
    rerun, 
    camcol, 
    field, 
    objID, 
    ra, dec, 
    rowv, colv, 
    rowvErr, colvErr, 
    i, 
    (flags & dbo_fPhotoFlags('MOVED')) as MOVED, 
    (flags & dbo_fPhotoFlags('BAD_MOVING_FIT')) as BAD_MOVING_FIT 
FROM Galaxy
WHERE
    (flags & (dbo_fPhotoFlags('MOVED') + dbo_fPhotoFlags('BAD_MOVING_FIT'))) > 0 
    and (rowv * rowv + colv * colv) >= 
    (rowvErr * rowvErr + colvErr * colvErr)
LIMIT 1000 ;")

(define SDSS_query_021
  "SELECT ObjID
FROM Galaxy as G
WHERE
    G.lnlDev_r > G.lnlExp_r + 0.1 
    -- the likelihood of the deVaucouleours profile fit is 10% greater than the 
    -- likelihood of the exponential fit 
    and G.lnlExp_r > -999 
    -- and the likelihoods are actually meaningful 
    and (G.flags & (dbo_fPhotoFlags('BINNED1') + dbo_fPhotoFlags('BINNED2') + 
    dbo_fPhotoFlags('BINNED4'))) > 0 
    -- and it is detected from at least one of the binned images 
    and (G.flags & ( dbo_fPhotoFlags('BLENDED') + dbo_fPhotoFlags('NODEBLEND') + 
    dbo_fPhotoFlags('CHILD'))) != dbo_fPhotoFlags('BLENDED') 
    -- and, if it is blended, it is either a child or not deblended further 
    and (G.flags & (dbo_fPhotoFlags('EDGE') + dbo_fPhotoFlags('SATURATED'))) = 0 
    -- and it is not near a ccd edge or saturated, where measurements may be bad 
    and G.petroMag_i > 17.5 
    -- and it is fainter than 17.5 in i-band 
    and (G.petroMag_r > 15.5 or G.petroR50_r > 2) 
    and (G.petroMag_r > 0 and G.g > 0 and G.r > 0 and G.i > 0) 
    and ( (G.petroMag_r - G.extinction_r) < 19.2 
    and (G.petroMag_r - G.extinction_r < 
    (13.1 + (7/3)*(G.g - G.r) + 4 *(G.r - G.i) - 4 * 0.18) ) 
    and ( (G.r - G.i - (G.g - G.r)/4 - 0.18) < 0.2 ) 
    and ( (G.r - G.i - (G.g - G.r)/4 - 0.18) > -0.2 ) ) 
    or ( (G.petroMag_r - G.extinction_r < 19.5) 
    and ( (G.r - G.i - (G.g - G.r)/4 - 0.18) > 
    (0.45 - 4*(G.g - G.r) ) ) 
    and ( (G.g - G.r) > (1.35 + 0.25 *(G.r - G.i) ) ) ) 
    -- and many constraints on colors and mags to make it have elliptical-type colors.
;")

(define SDSS_query_022
  "SELECT modelMag_u, modelMag_g, objID 
FROM Galaxy
WHERE
    ( Flags & (dbo_fPhotoFlags('SATURATED') + 
        dbo_fPhotoFlags('BRIGHT') + 
        dbo_fPhotoFlags('EDGE')) ) = 0 
    and petroRad_r < 18 
    and ((modelMag_u - modelMag_g) - (psfMag_u - psfMag_g)) < -0.4 
LIMIT 1000 ;")

(define SDSS_query_023
  "SELECT  o.ra,o.dec,o.flags, o.type,o.objid,
    o.psfMag_g,o.psfMag_r,o.psfMag_i,o.modelMag_g,o.modelMag_r,o.modelMag_i, 
    o.petroRad_r, 
    o.q_g,o.q_r,o.q_i, 
    o.u_g,o.u_r,o.u_i, 
    o.mE1_r,o.mE2_r,o.mRrCc_r,o.mCr4_r, 
    o.isoA_r,o.isoB_r,o.isoAGrad_r,o.isoBGrad_r,o.isoPhi_r, 
    n.distance,p.r,p.g
    FROM PhotoObjAll as o 
    LEFT JOIN Neighbors as n on o.objid=n.objid 
    JOIN PhotoObjAll p ON p.objId=n.neighborObjId
    WHERE 
    (o.ra > 120) and (o.ra < 240) 
    and (o.r > 16.) and (o.r<21.0) 
    and n.neighborObjId=(select nn.neighborObjId
       from Neighbors nn join PhotoObjAll pp ON nn.neighborObjId = pp.objID 
       where nn.objId=o.objId 
       order by pp.r
       limit 1)
    LIMIT 100 ;"

  )