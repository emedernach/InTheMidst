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



(include "../macros.dir/macros.scm")
(include "../ast.dir/ast-macros.scm")

;;      (export replace-table)

;; replace-table replaces table  with its definition and add
;; an  alias around  it  with the  original  table name  and
;; parentheses.


;; TODO: Some  partitioned tables are  parameteriezd over an
;; ID, if we query a  particular ID then we know which table
;; to query. Samely if we  query a neighborhood we know from
;; a function which  tables to query. But is  it worth doing
;; this ? I mean with an  index the speed is already fast to
;; find that the result is empty.

;; TODO: we  have to  replace while there  are tables  to be
;; replaced !   After replacing a table it  is possible that
;; it introduces  other tables to be  replaced.  Idea: first
;; replace  in  the replacement  to  eliminate all  replaced
;; tables.   SOLUTION: We will  replace manually  before for
;; the moment.

;; (define (replace-table ast default-database table-name replacement)
;;   ;; First replace all tables in replacement
;;   ;; Then replace with replacement
;;   ...)

(define-ast-walker replace-table
  ;; table-name = table-record
  (default-database table-name replacement)
  (dispatch
   (table-record?  table-dispatch)
   (alias?         alias-dispatch))

  ;; Dispatchers

  (define (table-dispatch obj)
    (let ((database (table-record->database obj))
          (name     (table-record->name obj)))
      (if (table-with-default=? default-database obj table-name)
          (make-alias name (parenthesised-expression replacement))
          obj)))

  (define (alias-dispatch obj)
    (let ((name (alias->name obj))
          (val  (alias->value obj)))
      (make-alias
       name 
       (if (table-record? val)
           (let ((database (table-record->database val))
                 (name     (table-record->name val)))
             (if (table-with-default=? default-database val table-name)
                 (parenthesised-expression replacement)
                 (dispatch val)))
           (dispatch val)))))

  ;; End of replace-table

  )

;; Tests

(define (replace-source sql)
  (let ((ast (SQL->AST sql))
        (default-database  "LSST")
        (table-name (table-record 'Unknown "Source"))
        (replacement (SQL->AST "SELECT * FROM Source_000 WHERE objectId % 4 = 0 
 UNION ALL SELECT * FROM Source_001 WHERE objectId % 4 = 1 
 UNION ALL SELECT * FROM Source_002 WHERE objectId % 4 = 2 
 UNION ALL SELECT * FROM Source_003 WHERE objectId % 4 = 3")))
    (AST->SQL (replace-table ast default-database table-name replacement))))

(define (replace:test)
  (unit-test
   (equal? (replace-source LSST_query_001_a)
           "SELECT taiMidPoint, psfFlux, psfFluxSigma FROM (SELECT * FROM Source_000 WHERE objectId  % 4 = 0 UNION ALL SELECT * FROM Source_001 WHERE objectId  % 4 = 1 UNION ALL SELECT * FROM Source_002 WHERE objectId  % 4 = 2 UNION ALL SELECT * FROM Source_003 WHERE objectId  % 4 = 3) AS Source INNER JOIN Filter USING ( filterId ) WHERE objectId = 1234567890  AND filterName = 'r' ;")
   (equal? (replace-source LSST_query_006)
           "SELECT s.ra, s.decl, o.raRange, o.declRange FROM Object AS o INNER JOIN (SELECT * FROM Source_000 WHERE objectId  % 4 = 0 UNION ALL SELECT * FROM Source_001 WHERE objectId  % 4 = 1 UNION ALL SELECT * FROM Source_002 WHERE objectId  % 4 = 2 UNION ALL SELECT * FROM Source_003 WHERE objectId  % 4 = 3) AS s USING ( objectId ) WHERE o.objectId = 1234567890  AND o.latestObsTime = s.taiMidPoint ;")
   (equal? (replace-source LSST_query_007)  
           "SELECT objectId, taiMidPoint, fluxToAbMag(psfMag) FROM (SELECT * FROM Source_000 WHERE objectId  % 4 = 0 UNION ALL SELECT * FROM Source_001 WHERE objectId  % 4 = 1 UNION ALL SELECT * FROM Source_002 WHERE objectId  % 4 = 2 UNION ALL SELECT * FROM Source_003 WHERE objectId  % 4 = 3) AS Source INNER JOIN Object USING ( objectId ) INNER JOIN Filter USING ( filterId ) WHERE areaSpec_box(10.5, 30.5, 12.2, 32.7)  AND filterName = 'u' AND variability BETWEEN .6 AND 1. ORDER BY objectId, taiMidPoint ASC ;")
   (equal? (replace-source LSST_query_056_a)
           "SELECT * FROM (SELECT * FROM Source_000 WHERE objectId  % 4 = 0 UNION ALL SELECT * FROM Source_001 WHERE objectId  % 4 = 1 UNION ALL SELECT * FROM Source_002 WHERE objectId  % 4 = 2 UNION ALL SELECT * FROM Source_003 WHERE objectId  % 4 = 3) AS Source ;")
   (equal? (replace-source LSST_query_056_b)
           "SELECT * FROM (SELECT * FROM Source_000 WHERE objectId  % 4 = 0 UNION ALL SELECT * FROM Source_001 WHERE objectId  % 4 = 1 UNION ALL SELECT * FROM Source_002 WHERE objectId  % 4 = 2 UNION ALL SELECT * FROM Source_003 WHERE objectId  % 4 = 3) AS Source INNER JOIN Object USING ( objectId ) WHERE extendedParam < .2 ;")
   (equal? (replace-source LSST_query_068)  
           "SELECT sce.visit, sce.raftName, sce.ccdName, sro.rMag, sro.ra, sro.decl, sro.isStar, sro.refObjectId, rom.nSrcMatches, s.sourceId, s.ra, s.decl, s.xAstrom, s.yAstrom, s.psfFlux, s.psfFluxSigma, s.apFlux, s.apFluxSigma, s.flux_ESG, s.flux_Gaussian, s.ixx, s.iyy, s.ixy, s.psfIxx, s.psfIxxSigma, s.psfIyy, s.psfIyySigma, s.psfIxy, s.psfIxySigma, s.resolution_SG, s.e1_SG, s.e1_SG_Sigma, s.e2_SG, s.e2_SG_Sigma, s.shear1_SG, s.shear1_SG_Sigma, s.shear2_SG, s.shear2_SG_Sigma, s.sourceWidth_SG, s.sourceWidth_SG_Sigma, s.flagForDetection FROM (SELECT * FROM Source_000 WHERE objectId  % 4 = 0 UNION ALL SELECT * FROM Source_001 WHERE objectId  % 4 = 1 UNION ALL SELECT * FROM Source_002 WHERE objectId  % 4 = 2 UNION ALL SELECT * FROM Source_003 WHERE objectId  % 4 = 3) AS s, Science_Ccd_Exposure AS sce, RefSrcMatch AS rom, SimRefObject AS sro WHERE s.scienceCcdExposureId = sce.scienceCcdExposureId  AND s.sourceId = rom.sourceId AND rom.refObjectId = sro.refObjectId AND sce.visit = 925692311 AND sce.raftName = '3,0' and sce.ccdName = '0,2' ;")

   ))

