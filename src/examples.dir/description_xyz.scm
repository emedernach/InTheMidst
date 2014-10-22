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



;; ---------------------------------------- ;;

(include "../macros.dir/macros.scm")
(include "../sql.dir/schema-macros.scm")
(include "../ast.dir/partition.scm")

(define (partitionned-table? tbl)
  (and
   (table-record? tbl)
   (let ((table-name (table-record->name tbl)))
     (or (string-ci=? table-name "Object")
         (string-ci=? table-name "Source")))))

(define chunk-boundary-list
  ;; chunkid xmin xmax ymin ymax zmin zmax
  '((001    0.95 1.00    -0.05 0       -0.15 -0.10)
    (002    0.95 1.00    -0.05 0        -0.1 -0.05)
    (003    0.95 1.00    -0.05 0       -0.05 0    )
    (004    0.95 1.00    -0.05 0           0 0.05 )
    (005    0.95 1.00    -0.05 0        0.05 0.10 )
    (006    0.95 1.00    -0.05 0         0.1 0.15 )
    (007    0.95 1.00        0 0.05    -0.15 -0.10)
    (008    0.95 1.00        0 0.05     -0.1 -0.05)
    (009    0.95 1.00        0 0.05    -0.05 0    )
    (010    0.95 1.00        0 0.05        0 0.05 )
    (011    0.95 1.00        0 0.05     0.05 0.10 )
    (012    0.95 1.00        0 0.05      0.1 0.15 )
    (013    0.95 1.00     0.05 0.10    -0.15 -0.10)
    (014    0.95 1.00     0.05 0.10     -0.1 -0.05)
    (015    0.95 1.00     0.05 0.10    -0.05 0    )
    (016    0.95 1.00     0.05 0.10        0 0.05 )
    (017    0.95 1.00     0.05 0.10     0.05 0.10 )
    (018    0.95 1.00     0.05 0.10      0.1 0.15 )))

;; master_object_001_xyz
;; ...
;; master_object_018_xyz

(define (object-template i)
  ;; The hard part is to align with 0, we use a trick here.
  (let* ((str (number->string (+ i 1000)))
         (str-len (string-length str))
         (num (substring str 1 str-len)))
    (string-append "master_object_" num "_xyz")))

;; master_source_001_xyz
;; ...
;; master_source_018_xyz

(define (source-template i)
  ;; The hard part is to align with 0, we use a trick here.
  (let* ((str (number->string (+ i 1000)))
         (str-len (string-length str))
         (num (substring str 1 str-len)))
    (string-append "master_source_" num "_xyz")))

;; replace  all  occurences  of  Object_  in  a  distributed
;; context with master_object_00<nnn>
(define FDW_modulo-partitions
  (make-modulo-partition-table
   
   ;; spatial join uses it with Object/Source
   ("Object" "objectId" 18 object-template)
   ("Source" "objectId" 18 source-template)

   ;; distributed-query uses it with Object_/Source_
   ("Object_" "objectId" 18 object-template)
   ("Source_" "objectId" 18 source-template)
   
   ))

;; We must have a "SELECT *" in order to have a subquery when we replace it.
(define FDW_Object
  (distributed-query
   ;; Ca doit etre une requete et pas directement une table !
   (sql->ast "SELECT * FROM Object_")
   ;; (table-record 'unknown "Object_")
   (field 'unknown "objectId") 18))

(define FDW_Source
  (distributed-query
   ;; Ca doit etre une requete et pas directement une table !
   (sql->ast "SELECT * FROM Source_")
   ;; (table-record 'unknown "Source_")
   (field 'unknown "objectId") 18))




;; ---------------------------------------- ;;

;; ra_PS_Sigma decl_PS_Sigma etc, auraient pu etre integre dans Geometry

(define (FDW_cardinality-one? left right fields)
  ;; For one element of 'left' how many elements of 'right' share the same 'fields' ?
  ;; If only one then this function returns true else false

  (define (compare head)
    (let* ((head-left   (car head))
           (head-right  (cadr head))
           (head-fields (caddr head)))
      (and (string-ci=? left head-left)
           (string-ci=? right head-right)
           (let loop ((fields fields)
                      (head-fields head-fields))
             (or (and (null? fields)
                      (null? head-fields))
                 (and (not (null? fields))
                      (not (null? head-fields))
                      (let ((car-fields (car fields))
                            (car-head-fields (car head-fields)))
                        (and (string-ci=? car-fields car-head-fields)
                             (loop (cdr fields) (cdr head-fields))))))))))
    
  (define (search liste)
    (and (not (null? liste))
         (or (compare (car liste))
             (search (cdr liste)))))

  (define (helper)
    (search '(;; For each source there is only one object:
              ("Source" "Object" ("objectId"))
              ;; A remettre !! ("Object_" "Geometry_Object" ("objectId" "ra_PS" "decl_PS"))
              ;; A remettre !! ("Geometry_Object" "Object_" ("objectId" "ra_PS" "decl_PS"))
              ;; A remettre !! ("Source_" "Geometry_Source" ("sourceId" "ra" "decl"))
              ;; A remettre !! ("Geometry_Source" "Source_" ("sourceId" "ra" "decl"))
              
              ;; ("Object_" "Geometry_Object" ("objectId"))
              ;; ("Geometry_Object" "Object_" ("objectId"))
              ;; ("Source_" "Geometry_Source" ("sourceId"))
              ;; ("Geometry_Source" "Source_" ("sourceId"))
              )))

  (debug
   (display "FDW_cardinality-one?  ")
   (display left) (display "  ")
   (display right) (display "  ")
   (display fields) (display "  "))
   
  (let ((result (helper)))
    (debug (display result) (newline) (newline))
    result ))

(define (FDW_choice left-table left-name
                    right-table rigth-name)
  ;; (pp (list 'FDW_choice left-name rigth-name)) (newline) (newline)
  (let ((result
         (assoc (list left-name rigth-name)
                '(;; (("Object_" "Geometry_Object") RIGHT)
                  ;; (("Geometry_Object" "Object_") LEFT)
                  ;; (("Source_" "Geometry_Source") RIGHT)
                  ;; (("Geometry_Source" "Source_") LEFT)
                  ))))
    (if result
        (case (cadr result)
          ((LEFT) left-table)
          ((RIGHT) right-table))
        left-table)))

;; (Object-virtual-table_1 (SQL->AST " SELECT * FROM master_object_000 WHERE objectId % 4 = 0 
;;  UNION ALL SELECT * FROM master_object_001 WHERE objectId % 4 = 1 
;;  UNION ALL SELECT * FROM master_object_002 WHERE objectId % 4 = 2 
;;  UNION ALL SELECT * FROM master_object_003 WHERE objectId % 4 = 3 "))
;; (Object-virtual-table_2 (SQL->AST "SELECT * 
;;  FROM      ( SELECT * FROM master_object_000 WHERE objectId % 4 = 0 
;;    UNION ALL SELECT * FROM master_object_001 WHERE objectId % 4 = 1 
;;    UNION ALL SELECT * FROM master_object_002 WHERE objectId % 4 = 2 
;;    UNION ALL SELECT * FROM master_object_003 WHERE objectId % 4 = 3 
;;  ) AS Object_
;;  JOIN (SELECT pointId, ra_PS, decl_PS 
;;    FROM Geometry 
;;    WHERE provenance = 'Object') AS Geometry_Object
;;  USING (pointId)"))
;; (Object-virtual-table_3 (SQL->AST FDW_Object))
;;         
;; (Source-virtual-table_1 (SQL->AST " SELECT * FROM master_source_000 WHERE objectId % 4 = 0 
;;  UNION ALL SELECT * FROM master_source_001 WHERE objectId % 4 = 1 
;;  UNION ALL SELECT * FROM master_source_002 WHERE objectId % 4 = 2 
;;  UNION ALL SELECT * FROM master_source_003 WHERE objectId % 4 = 3 "))        
;; (Source-virtual-table_2 (SQL->AST "SELECT * 
;;  FROM    ( SELECT * FROM master_source_000 WHERE objectId % 4 = 0 
;;  UNION ALL SELECT * FROM master_source_001 WHERE objectId % 4 = 1 
;;  UNION ALL SELECT * FROM master_source_002 WHERE objectId % 4 = 2 
;;  UNION ALL SELECT * FROM master_source_003 WHERE objectId % 4 = 3 
;;  ) AS Source_
;;  JOIN (SELECT pointId, ra_PS, decl_PS 
;;    FROM Geometry 
;;    WHERE provenance = 'Source') AS Geometry_Source
;;  USING (pointId)"))
;; (Source-virtual-table_3 (SQL->AST FDW_Source))

;; OLD ;; (define FDW_Object_
;; OLD ;;   "SELECT * FROM master_object_000 
;; OLD ;;    UNION ALL SELECT * FROM master_object_001 
;; OLD ;;    UNION ALL SELECT * FROM master_object_002 
;; OLD ;;    UNION ALL SELECT * FROM master_object_003 ")

(define FDW_Object_
  (distributed-query
   ;; (sql->ast "SELECT * FROM Object_") ;; pourquoi pas ?
   (table-record 'unknown "Object_")
   (field 'unknown "objectId") 4))

;; 23 juin ;; (define FDW_Object ;; TODO: describe it as an AST tree (because of Virtual Table like Object_ and Source_)
;; 23 juin ;;   "SELECT * 
;; 23 juin ;; FROM  Object_
;; 23 juin ;; JOIN  Geometry_Object
;; 23 juin ;; USING (objectId,ra_PS,decl_PS)")

;; Le remplacement n'est pas correct: autour de __DISTRIB__1
;; SELECT * FROM (SELECT * FROM ((SELECT * FROM master_object_000 UNION ALL SELECT * FROM master_object_001 UNION ALL SELECT * FROM master_object_002 UNION ALL SELECT * FROM master_object_003) AS __DISTRIB__1) AS Object_) AS object LIMIT 10 ;

;; Une distributed-query <query> est remplacee par:
;;
;; (<query 000> UNION ALL <query 001> ...) AS <NewName>

;; Il faut donc que le contexte autorise ce remplacement

;; "select * from object limit 10;"
;; => "select * from (???) AS object limit 10;"
;; => "select * from (select * from (.. UNION ALL ..) as __NewName__) AS object limit 10;"

;; ---------------------------------------- ;;

;;
;; Cela  ne marche pas  parce que  les contraintes
;; ra/decl  ne sont  pas distribues  sur  la table
;; Geometry.
;;
;; (define FDW_Object
;;   "SELECT objectId 
;;  FROM  Object_
;;  JOIN  Geometry_Object
;;  USING (objectId)")

;;
;; Cela  ne  marche  pas  parce que  les  colonnes
;; ra/decl sont alors en doubles dans le resultat.
;;
;; (define FDW_Object
;;   "SELECT * 
;;  FROM  Object_
;;  JOIN  Geometry_Object
;;  USING (objectId)")

(define FDW_Source_ 
  (distributed-query
   ;; (sql->ast "SELECT * FROM Source_")
   (table-record 'unknown "Source_")
   (field 'unknown "objectId") 4))

;; 23 juin ;; (define FDW_Source
;; 23 juin ;;   "SELECT * 
;; 23 juin ;;  FROM Source_
;; 23 juin ;;  JOIN Geometry_Source
;; 23 juin ;;  USING (sourceId,ra,decl)")

;;
;; Voir les commentaires ci-dessus.
;;
;; (define FDW_Source
;;   "SELECT * 
;;  FROM Source_
;;  JOIN Geometry_Source
;;  USING (sourceId)")

;; "ra_PS"    "decl_PS"     are    extracted    to
;; Geometry_Object but we  keep them here in order
;; to  be able  to  distribute ra/decl  predicates
;; inside this table.

;; FDW_tables-description is a hash tables of <table-name> and (<column> ...)
(define FDW_tables-description
  (let ((object_columns 
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
           "yRadius_SG" "yRadius_SG_Sigma" "yFlags" "chunkId" "subChunkId" ))
        (source_columns
         '("sourceId" "scienceCcdExposureId" "filterId" "objectId" 
           "movingObjectId"     "procHistoryId"  "ra"   "raSigmaForDetection"
           "raSigmaForWcs" "decl"  "declSigmaForDetection"  "declSigmaForWcs"
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
           "flagForDetection" "flagForWcs" )))

    (let ((schema
           (make-schema
   
            ;; Test tables
            ("t_a"  (columns "t_a" '("id" "a")))
            ("t_b"  (columns "t_b" '("id" "b")))

            ;; Some LSST tables     
            ("Filter" (columns "Filter" '("filterId" "filterName" "photClam" "photBW" )))
            ;; ("Geometry_Object" (columns "Geometry_Object" '("objectId" "ra_PS" "decl_PS" "provenance")))
            ;; ("Geometry_Source" (columns "Geometry_Source" '("sourceId" "ra" "decl" "provenance")))
            ("Science_Ccd_Exposure"
             (columns "Science_Ccd_Exposure"
                      '("scienceCcdExposureId" "visit" "raft" "raftName"
                        "ccd" "ccdName" "filterId" "filterName" "ra" "decl"
                        "equinox" "raDeSys" "ctype1" "ctype2" "crpix1" "crpix2"
                        "crval1" "crval2" "cd1_1" "cd1_2" "cd2_1" "cd2_2"
                        "llcRa" "llcDecl" "ulcRa" "ulcDecl" "urcRa" "urcDecl"
                        "lrcRa" "lrcDecl" "taiMjd" "obsStart" "expMidpt"
                        "expTime" "nCombine" "binX" "binY" "readNoise"
                        "saturationLimit" "gainEff" "fluxMag0" "fluxMag0Sigma"
                        "fwhm" "poly" "flags")))
   
            ;; dummy table for tests
            ("xyz_bucket"
             (columns "xyz_bucket"
                      '("pointid" "ra" "decl")))

            ("Object_"
             (columns
              "Object_"
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

            ("ObjectBoundary"
             (columns
              "ObjectBoundary"
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

            ("Source_"
             (columns
              "Source_"
              '("sourceId" "scienceCcdExposureId" "filterId" "objectId"
                "movingObjectId"     "procHistoryId"  "ra" "raSigmaForDetection"
                "raSigmaForWcs" "decl" "declSigmaForDetection"   "declSigmaForWcs"
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
                "flagForDetection" "flagForWcs" )))
   
            ("Source"
             (columns
              "Source"
              '("sourceId" "scienceCcdExposureId" "filterId" "objectId"
                "movingObjectId"     "procHistoryId"   "ra"  "raSigmaForDetection"
                "raSigmaForWcs"  "decl"  "declSigmaForDetection"   "declSigmaForWcs"
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
                "flagForDetection" "flagForWcs" )))

            ("SourceBoundary"
             (columns
              "SourceBoundary"
              '("sourceId" "scienceCcdExposureId" "filterId" "objectId"
                "movingObjectId"     "procHistoryId"   "ra"  "raSigmaForDetection"
                "raSigmaForWcs"  "decl"  "declSigmaForDetection"   "declSigmaForWcs"
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
                "flagForDetection" "flagForWcs" )))
            
            )))

      (add-distributed-tables
       schema
       (map car chunk-boundary-list)
       (object-template object_columns)
       (source-template source_columns)))))

(define (field-present-in-table? descr table field)
  (let ((tmp (table-ref descr table #f)))
    (and tmp (member field (columns->fields tmp)))))

(define (description->table->fields descr)
  (lambda (database table)
    (and (or (eq? database 'unknown)
             (string=? database "LSST"))
         (let ((tmp (table-ref descr table #f)))
           (and tmp
                (let ((table-name (columns->name tmp)))
                  (map (lambda (x) (field table-name x))
                       (columns->fields tmp))))))))

;; (lambda (database table) ...)
(define FDW_table->fields
  (description->table->fields FDW_tables-description))

(define (FDW_table->boundary tbl)
  (if (table-record? tbl)
      (let ((database (table-record->database tbl))
            (name (table-record->name tbl)))
        (cond ((string-ci=? name "Object") "ObjectBoundary")
              ((string-ci=? name "Source") "SourceBoundary")
              (else (error "FDW_table->boundary: not yet implemented" tbl))))
      (error "FDW_table->boundary: not a table" tbl)))