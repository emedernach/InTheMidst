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



;; (database-result->description result)
;; (database-result->vector result)))))
(define result
  (execute-sql-select
   ;; LSST_query_020
;;    "SELECT fluxToAbMag(gFlux_PS), objectId
;; FROM   Object
;; WHERE  gFlux_PS > 0
;; AND    rFlux_PS > 0 
;; AND    iFlux_PS > 0 
;; AND    zFlux_PS > 0 
;; AND    yFlux_PS > 0 
;;    AND fluxToAbMag(gFlux_PS) <= 22
;;    AND fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) >= -0.24 AND fluxToAbMag(gFlux_PS)-fluxToAbMag(rFlux_PS) < 0.35
;;    AND fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS) >= -0.27 AND fluxToAbMag(rFlux_PS)-fluxToAbMag(iFlux_PS) < 0.57
;;    AND fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) >= -0.35 AND fluxToAbMag(iFlux_PS)-fluxToAbMag(zFlux_PS) < 0.70
;; ;"))
   
   ;; Perf_Q5_adapted 
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
ORDER BY pop ;"))

;; (pp (database-result->description result)) (newline)
;; (pp (database-result->vector result)) (newline)
;; (newline)


