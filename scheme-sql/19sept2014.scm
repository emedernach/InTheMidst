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

(define (query-execute sql output)
  (let* ((myplan (planner
                  master
                  database-schema
                  external-table->local-name
                  sql output))
         (query-plan (car myplan))
         (cleanup-plan (cadr myplan)))
    (time (execute query-plan))
    (time (execute cleanup-plan))
    ;; cleanup-plan
    ))

(define Perf_Q8_adapted ;; Ok
  "SELECT objectId, taiMidPoint, psfFlux
FROM   Source
JOIN   Object USING(objectId)
JOIN   Filter USING(filterId)
WHERE  ra_PS BETWEEN 0.5 AND 2.2
  AND  decl_PS BETWEEN -0.5 AND 3.7
  AND  filterName = 'g'
ORDER BY objectId, taiMidPoint ASC ;")

(time (query-execute Perf_Q8_adapted "T_Perf_Q8_adapted"))

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


