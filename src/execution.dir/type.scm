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



;; -> field-type-list
(define (ast->type database-schema ast)
  (let ((top-level-scope (make-scope)))  
    (ast->field-type-list
     ast database-schema top-level-scope)))

(define (field-type->type ft)
  (let ((field-name     (field-type->field-name ft))
        (field-datatype (field-type->field-datatype ft)))
    (string-append field-name "  " field-datatype)))

(define (describe:test database-schema)
  (let* ((sql "SELECT S1.objectId AS s1, S2.objectId AS s2 FROM (SELECT * FROM master_object_018_xyz WHERE (-2.5  * log(iFlux_PS) - 48.6)  - (-2.5  * log(zFlux_PS) - 48.6) > .4  AND (-2.5  * log(rFlux_PS) - 48.6)  - (-2.5  * log(iFlux_PS) - 48.6) > .4 AND (-2.5  * log(gFlux_PS) - 48.6)  - (-2.5  * log(rFlux_PS) - 48.6) < .7 AND zFlux_PS > 0. AND iFlux_PS > 0. AND rFlux_PS > 0. AND gFlux_PS > 0. AND sin(radians(decl_PS)) BETWEEN .14999479182942466 AND .10000520817057533 AND sin(radians(ra_PS))  * cos(radians(decl_PS)) BETWEEN .09999479182942467 AND .05000520817057534 AND cos(radians(ra_PS))  * cos(radians(decl_PS)) BETWEEN .9999947918294246 AND .9500052081705753) AS S1, master_object_018_xyz AS S2 WHERE cos(radians(S1.ra_PS))  * cos(radians(S1.decl_PS)) BETWEEN cos(radians(S2.ra_PS))  * cos(radians(S2.decl_PS))  - sin(radians(.05  / 2))  * 2 AND cos(radians(S2.ra_PS))  * cos(radians(S2.decl_PS))  + sin(radians(.05  / 2))  * 2  AND sin(radians(S1.ra_PS))  * cos(radians(S1.decl_PS)) BETWEEN sin(radians(S2.ra_PS))  * cos(radians(S2.decl_PS))  - sin(radians(.05  / 2))  * 2 AND sin(radians(S2.ra_PS))  * cos(radians(S2.decl_PS))  + sin(radians(.05  / 2))  * 2 AND sin(radians(S1.decl_PS)) BETWEEN sin(radians(S2.decl_PS))  - sin(radians(.05  / 2))  * 2 AND sin(radians(S2.decl_PS))  + sin(radians(.05  / 2))  * 2 AND degrees(asin(sqrt(power(sin(radians((S2.decl_PS  - S1.decl_PS)  / 2)), 2)  + power(sin(radians((S2.ra_PS  - S1.ra_PS)  / 2)), 2)  * cos(radians(S1.decl_PS)) * cos(radians(S2.decl_PS))))  * 2) <= .05  AND S1.objectId <> S2.objectId ;")
         (ast (sql->ast sql))
         (field-type-list (ast->type database-schema ast)))
    field-type-list))