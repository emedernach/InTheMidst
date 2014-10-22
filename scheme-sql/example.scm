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



;; 3 tables from PT12 :
;; - Filter
;; - Object
;; - Source

;; (database-result->description result)
;; (database-result->vector result)))))
;; (execute-sql-select query)

;; Define your own data processing functions
(define (display-result query)
  (let ((result (execute-sql-select query)))
    ;; (pp (database-result->description result)) (newline)
    ;; (pp (database-result->vector result)) (newline)
    ;; 'Ok))
    result))

(define (query-length query)
  (let ((result (execute-sql-select query)))
    (vector-length (database-result->vector result))))

(define myquery "SELECT ra_ps + ra_PS_Sigma AS ra2 FROM Object LIMIT 10 ;")
(display-result myquery)
(rewriter myquery)

;; Select by objectId
(display-result "SELECT ra_PS,decl_PS FROM Object WHERE objectId = 430209694172126 ;")

(display-result 
 "SELECT objectId, rFlux_PS
FROM Object
WHERE objectId IN (430209694172100,
                   430209694172101,
                   430209694172102,
                   430209694172103)
;")

;; Select by sourceId
(display-result "SELECT ra,decl,flux_gaussian
FROM Source WHERE sourceId = 29758152061094288 ;")

;; Object JOIN Source
(display-result 
"SELECT sourceId, taiMidPoint
FROM Object JOIN Source USING (objectId)
WHERE objectId = 383841227243550
;")

(display-result "SELECT count(*)         
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

;; Full scans
(display-result "SELECT min(ra_PS), max(ra_PS),
min(decl_PS), max(decl_PS)
FROM Object
WHERE ra_PS < 180.
;")

;; Searching for dense HTM regions
(display-result "SELECT count(*) AS n, AVG(ra_PS), AVG(decl_PS), htmId20
FROM Object
GROUP BY htmId20
HAVING COUNT(*) > 1 ;")

;; LV2_IN2P3_300
(display-result "SELECT  count(*)
FROM Object
WHERE ra_PS BETWEEN 1 AND 2
 AND  decl_PS BETWEEN 3 AND 4 
 AND  zFlux_PS > 0 
 AND  fluxToAbMag(zFlux_PS) BETWEEN 21 AND 21.5 ;")

;;  LV3_IN2P3_300
(display-result "SELECT count(*) FROM Object 
WHERE   ra_PS BETWEEN 1 AND 2
AND     decl_PS BETWEEN 3 AND 4
AND   gFlux_PS > 0.
AND   rFlux_PS > 0.
AND   iFlux_PS > 0.
AND   zFlux_PS > 0.
AND	 fluxToAbMag(zFlux_PS) BETWEEN 21 AND 21.5
AND	 fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) BETWEEN 0.3 AND 0.4
AND	 fluxToAbMag(iFlux_PS) - fluxToAbMag(zFlux_PS) BETWEEN 0.1 AND 0.12 ;")

;; LV4_IN2P3_300
(display-result
  "SELECT ra, decl 
FROM Object 
JOIN Source USING (objectId)
WHERE objectId = 396210733061753
AND latestObsTime = taiMidPoint ;")

;; LSST_query_020
(display-result
  "SELECT count(*) -- fluxToAbMag(gFlux_PS), objectId
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

;; -- Perf/Q5 (db/queries/036): Ok

;; Perf_Q8_adapted ;; ~ 
(display-result
"SELECT objectId, taiMidPoint, psfFlux
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
WHERE  ra_PS BETWEEN 0.5 AND 2.2
  AND  decl_PS BETWEEN -0.5 AND 3.7
  AND  filterName = 'g'
ORDER BY objectId, taiMidPoint ASC ;")

(display-result "SELECT COUNT(*)
FROM Object
WHERE spatial_rectangle(ra_PS, decl_PS, 0.5, 2.8, 1.2, 3.7)
AND  gFlux_PS > 0.
AND  rFlux_PS > 0.
AND  fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) > 1.7
;")

(display-result "SELECT objectId, gFlux_PS, ra_PS, decl_PS
FROM Object
WHERE spatial_rectangle(ra_PS, decl_PS, 0.5, 2.8, 1.2, 3.7)
AND  gFlux_PS > 0.
AND  rFlux_PS > 0.
AND  fluxToAbMag(gFlux_PS) - fluxToAbMag(rFlux_PS) > 1.7
;")

