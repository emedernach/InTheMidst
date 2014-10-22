;; Scheme-SQL

;; Description of database schema and pools

;; (pool pool-name hostname port dbname user)

(define master (pool "master" "localhost" 5280 "postgres" "postgres"))

(define pool1  (pool "pool1" "clrlsstbench02-vm.in2p3.fr" 5280 "postgres" "postgres"))
(define pool2  (pool "pool2" "clrlsstbench03-vm.in2p3.fr" 5280 "postgres" "postgres"))
(define pool3  (pool "pool3" "clrlsstbench04-vm.in2p3.fr" 5280 "postgres" "postgres"))
(define pool4  (pool "pool4" "clrlsstbench05-vm.in2p3.fr" 5280 "postgres" "postgres"))


;; Some convenient macros 

(define-syntax table-description
  (syntax-rules ()
    ((table-description
      (field-name field-datatype)
      ...)
     (table-type
      (list
       (field-type field-name field-datatype)
       ...)))))

(define-syntax schema-description
  (syntax-rules ()
    ((schema-description
      (table-name description virtual? pool parameters)
      ...)
     (schema
      (list
       (schema-table
        table-name description
        (case virtual?
          ((virtual-table)  #t)
          ((physical-table) #f)
          (else (error "schema-description: Unknown virtual tag" virtual?)))
        pool parameters)
       ...)))))      

;; string -> table-ref
(define-syntax translation-table
  (syntax-rules ()
    ((translation-table
      (name translation)
      ...)     
     (let ((result (make-table)))
       (table-set! result name translation)
       ...
       (lambda (str)
         (table-ref result str 'missing))))))

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

;; -- Data -- ;;

(define filter-description
  (table-description
   ;; "Filter"
   ("filterId" "smallint")
   ("filterName" "text")
   ("photClam" "real")
   ("photBW" "real")))

(define object-description
  (table-description
   ;; "Object"
   ("objectId" "bigint")
   ("iauId" "text")
   ("ra_PS" "double precision")
   ("ra_PS_Sigma" "real")
   ("decl_PS" "double precision")
   ("decl_PS_Sigma" "real")
   ("radecl_PS_Cov" "real")
   ("htmId20" "bigint")
   ("ra_SG" "double precision")
   ("ra_SG_Sigma" "real")
   ("decl_SG" "double precision")
   ("decl_SG_Sigma" "real")
   ("radecl_SG_Cov" "real")
   ("raRange" "real")
   ("declRange" "real")
   ("muRa_PS" "double precision")
   ("muRa_PS_Sigma" "real")
   ("muDecl_PS" "double precision")
   ("muDecl_PS_Sigma" "real")
   ("muRaDecl_PS_Cov" "real")
   ("parallax_PS" "double precision")
   ("parallax_PS_Sigma" "real")
   ("canonicalFilterId" "smallint")
   ("extendedness" "smallint")
   ("varProb" "real")
   ("earliestObsTime" "double precision")
   ("latestObsTime" "double precision")
   ("meanObsTime" "double precision")
   ("flags" "bigint")
   ("uNumObs" "bigint")
   ("uExtendedness" "smallint")
   ("uVarProb" "real")
   ("uRaOffset_PS" "real")
   ("uRaOffset_PS_Sigma" "real")
   ("uDeclOffset_PS" "real")
   ("uDeclOffset_PS_Sigma" "real")
   ("uRaDeclOffset_PS_Cov" "real")
   ("uRaOffset_SG" "real")
   ("uRaOffset_SG_Sigma" "real")
   ("uDeclOffset_SG" "real")
   ("uDeclOffset_SG_Sigma" "real")
   ("uRaDeclOffset_SG_Cov" "real")
   ("uLnL_PS" "real")
   ("uLnL_SG" "real")
   ("uFlux_PS" "real")
   ("uFlux_PS_Sigma" "real")
   ("uFlux_ESG" "real")
   ("uFlux_ESG_Sigma" "real")
   ("uFlux_Gaussian" "real")
   ("uFlux_Gaussian_Sigma" "real")
   ("uTimescale" "real")
   ("uEarliestObsTime" "double precision")
   ("uLatestObsTime" "double precision")
   ("uSersicN_SG" "real")
   ("uSersicN_SG_Sigma" "real")
   ("uE1_SG" "real")
   ("uE1_SG_Sigma" "real")
   ("uE2_SG" "real")
   ("uE2_SG_Sigma" "real")
   ("uRadius_SG" "real")
   ("uRadius_SG_Sigma" "real")
   ("uFlags" "bigint")
   ("gNumObs" "bigint")
   ("gExtendedness" "smallint")
   ("gVarProb" "real")
   ("gRaOffset_PS" "real")
   ("gRaOffset_PS_Sigma" "real")
   ("gDeclOffset_PS" "real")
   ("gDeclOffset_PS_Sigma" "real")
   ("gRaDeclOffset_PS_Cov" "real")
   ("gRaOffset_SG" "real")
   ("gRaOffset_SG_Sigma" "real")
   ("gDeclOffset_SG" "real")
   ("gDeclOffset_SG_Sigma" "real")
   ("gRaDeclOffset_SG_Cov" "real")
   ("gLnL_PS" "real")
   ("gLnL_SG" "real")
   ("gFlux_PS" "real")
   ("gFlux_PS_Sigma" "real")
   ("gFlux_ESG" "real")
   ("gFlux_ESG_Sigma" "real")
   ("gFlux_Gaussian" "real")
   ("gFlux_Gaussian_Sigma" "real")
   ("gTimescale" "real")
   ("gEarliestObsTime" "double precision")
   ("gLatestObsTime" "double precision")
   ("gSersicN_SG" "real")
   ("gSersicN_SG_Sigma" "real")
   ("gE1_SG" "real")
   ("gE1_SG_Sigma" "real")
   ("gE2_SG" "real")
   ("gE2_SG_Sigma" "real")
   ("gRadius_SG" "real")
   ("gRadius_SG_Sigma" "real")
   ("gFlags" "bigint")
   ("rNumObs" "bigint")
   ("rExtendedness" "smallint")
   ("rVarProb" "real")
   ("rRaOffset_PS" "real")
   ("rRaOffset_PS_Sigma" "real")
   ("rDeclOffset_PS" "real")
   ("rDeclOffset_PS_Sigma" "real")
   ("rRaDeclOffset_PS_Cov" "real")
   ("rRaOffset_SG" "real")
   ("rRaOffset_SG_Sigma" "real")
   ("rDeclOffset_SG" "real")
   ("rDeclOffset_SG_Sigma" "real")
   ("rRaDeclOffset_SG_Cov" "real")
   ("rLnL_PS" "real")
   ("rLnL_SG" "real")
   ("rFlux_PS" "real")
   ("rFlux_PS_Sigma" "real")
   ("rFlux_ESG" "real")
   ("rFlux_ESG_Sigma" "real")
   ("rFlux_Gaussian" "real")
   ("rFlux_Gaussian_Sigma" "real")
   ("rTimescale" "real")
   ("rEarliestObsTime" "double precision")
   ("rLatestObsTime" "double precision")
   ("rSersicN_SG" "real")
   ("rSersicN_SG_Sigma" "real")
   ("rE1_SG" "real")
   ("rE1_SG_Sigma" "real")
   ("rE2_SG" "real")
   ("rE2_SG_Sigma" "real")
   ("rRadius_SG" "real")
   ("rRadius_SG_Sigma" "real")
   ("rFlags" "bigint")
   ("iNumObs" "bigint")
   ("iExtendedness" "smallint")
   ("iVarProb" "real")
   ("iRaOffset_PS" "real")
   ("iRaOffset_PS_Sigma" "real")
   ("iDeclOffset_PS" "real")
   ("iDeclOffset_PS_Sigma" "real")
   ("iRaDeclOffset_PS_Cov" "real")
   ("iRaOffset_SG" "real")
   ("iRaOffset_SG_Sigma" "real")
   ("iDeclOffset_SG" "real")
   ("iDeclOffset_SG_Sigma" "real")
   ("iRaDeclOffset_SG_Cov" "real")
   ("iLnL_PS" "real")
   ("iLnL_SG" "real")
   ("iFlux_PS" "real")
   ("iFlux_PS_Sigma" "real")
   ("iFlux_ESG" "real")
   ("iFlux_ESG_Sigma" "real")
   ("iFlux_Gaussian" "real")
   ("iFlux_Gaussian_Sigma" "real")
   ("iTimescale" "real")
   ("iEarliestObsTime" "double precision")
   ("iLatestObsTime" "double precision")
   ("iSersicN_SG" "real")
   ("iSersicN_SG_Sigma" "real")
   ("iE1_SG" "real")
   ("iE1_SG_Sigma" "real")
   ("iE2_SG" "real")
   ("iE2_SG_Sigma" "real")
   ("iRadius_SG" "real")
   ("iRadius_SG_Sigma" "real")
   ("iFlags" "bigint")
   ("zNumObs" "bigint")
   ("zExtendedness" "smallint")
   ("zVarProb" "real")
   ("zRaOffset_PS" "real")
   ("zRaOffset_PS_Sigma" "real")
   ("zDeclOffset_PS" "real")
   ("zDeclOffset_PS_Sigma" "real")
   ("zRaDeclOffset_PS_Cov" "real")
   ("zRaOffset_SG" "real")
   ("zRaOffset_SG_Sigma" "real")
   ("zDeclOffset_SG" "real")
   ("zDeclOffset_SG_Sigma" "real")
   ("zRaDeclOffset_SG_Cov" "real")
   ("zLnL_PS" "real")
   ("zLnL_SG" "real")
   ("zFlux_PS" "real")
   ("zFlux_PS_Sigma" "real")
   ("zFlux_ESG" "real")
   ("zFlux_ESG_Sigma" "real")
   ("zFlux_Gaussian" "real")
   ("zFlux_Gaussian_Sigma" "real")
   ("zTimescale" "real")
   ("zEarliestObsTime" "double precision")
   ("zLatestObsTime" "double precision")
   ("zSersicN_SG" "real")
   ("zSersicN_SG_Sigma" "real")
   ("zE1_SG" "real")
   ("zE1_SG_Sigma" "real")
   ("zE2_SG" "real")
   ("zE2_SG_Sigma" "real")
   ("zRadius_SG" "real")
   ("zRadius_SG_Sigma" "real")
   ("zFlags" "bigint")
   ("yNumObs" "bigint")
   ("yExtendedness" "smallint")
   ("yVarProb" "real")
   ("yRaOffset_PS" "real")
   ("yRaOffset_PS_Sigma" "real")
   ("yDeclOffset_PS" "real")
   ("yDeclOffset_PS_Sigma" "real")
   ("yRaDeclOffset_PS_Cov" "real")
   ("yRaOffset_SG" "real")
   ("yRaOffset_SG_Sigma" "real")
   ("yDeclOffset_SG" "real")
   ("yDeclOffset_SG_Sigma" "real")
   ("yRaDeclOffset_SG_Cov" "real")
   ("yLnL_PS" "real")
   ("yLnL_SG" "real")
   ("yFlux_PS" "real")
   ("yFlux_PS_Sigma" "real")
   ("yFlux_ESG" "real")
   ("yFlux_ESG_Sigma" "real")
   ("yFlux_Gaussian" "real")
   ("yFlux_Gaussian_Sigma" "real")
   ("yTimescale" "real")
   ("yEarliestObsTime" "double precision")
   ("yLatestObsTime" "double precision")
   ("ySersicN_SG" "real")
   ("ySersicN_SG_Sigma" "real")
   ("yE1_SG" "real")
   ("yE1_SG_Sigma" "real")
   ("yE2_SG" "real")
   ("yE2_SG_Sigma" "real")
   ("yRadius_SG" "real")
   ("yRadius_SG_Sigma" "real")
   ("yFlags" "bigint")
   ("chunkId" "bigint")
   ("subChunkId" "bigint")))
  
(define source-description
  (table-description
   ;; "Source"
   ("sourceId" "bigint")
   ("scienceCcdExposureId" "bigint")
   ("filterId" "smallint")
   ("objectId" "bigint")
   ("movingObjectId" "bigint")
   ("procHistoryId" "bigint")
   ("ra" "double precision")
   ("raSigmaForDetection" "real")
   ("raSigmaForWcs" "real")
   ("decl" "double precision")
   ("declSigmaForDetection" "real")
   ("declSigmaForWcs" "real")
   ("htmId20_source" "bigint")
   ("xFlux" "double precision")
   ("xFluxSigma" "real")
   ("yFlux" "double precision")
   ("yFluxSigma" "real")
   ("raFlux" "double precision")
   ("raFluxSigma" "real")
   ("declFlux" "double precision")
   ("declFluxSigma" "real")
   ("xPeak" "double precision")
   ("yPeak" "double precision")
   ("raPeak" "double precision")
   ("declPeak" "double precision")
   ("xAstrom" "double precision")
   ("xAstromSigma" "real")
   ("yAstrom" "double precision")
   ("yAstromSigma" "real")
   ("raAstrom" "double precision")
   ("raAstromSigma" "real")
   ("declAstrom" "double precision")
   ("declAstromSigma" "real")
   ("raObject" "double precision")
   ("declObject" "double precision")
   ("taiMidPoint" "double precision")
   ("taiRange" "real")
   ("psfFlux" "double precision")
   ("psfFluxSigma" "real")
   ("apFlux" "double precision")
   ("apFluxSigma" "real")
   ("modelFlux" "double precision")
   ("modelFluxSigma" "real")
   ("petroFlux" "double precision")
   ("petroFluxSigma" "real")
   ("instFlux" "double precision")
   ("instFluxSigma" "real")
   ("nonGrayCorrFlux" "double precision")
   ("nonGrayCorrFluxSigma" "real")
   ("atmCorrFlux" "double precision")
   ("atmCorrFluxSigma" "real")
   ("apDia" "real")
   ("Ixx" "real")
   ("IxxSigma" "real")
   ("Iyy" "real")
   ("IyySigma" "real")
   ("Ixy" "real")
   ("IxySigma" "real")
   ("psfIxx" "real")
   ("psfIxxSigma" "real")
   ("psfIyy" "real")
   ("psfIyySigma" "real")
   ("psfIxy" "real")
   ("psfIxySigma" "real")
   ("e1_SG" "real")
   ("e1_SG_Sigma" "real")
   ("e2_SG" "real")
   ("e2_SG_Sigma" "real")
   ("resolution_SG" "real")
   ("shear1_SG" "real")
   ("shear1_SG_Sigma" "real")
   ("shear2_SG" "real")
   ("shear2_SG_Sigma" "real")
   ("sourceWidth_SG" "real")
   ("sourceWidth_SG_Sigma" "real")
   ("shapeFlag_SG" "smallint")
   ("snr" "real")
   ("chi2" "real")
   ("sky" "real")
   ("skySigma" "real")
   ("extendedness_source" "smallint")
   ("flux_Gaussian" "double precision")
   ("flux_Gaussian_Sigma" "real")
   ("flux_ESG" "double precision")
   ("flux_ESG_Sigma" "real")
   ("sersicN_SG" "real")
   ("sersicN_SG_Sigma" "real")
   ("radius_SG" "real")
   ("radius_SG_Sigma" "real")
   ("flux_flux_SG_Cov" "real")
   ("flux_e1_SG_Cov" "real")
   ("flux_e2_SG_Cov" "real")
   ("flux_radius_SG_Cov" "real")
   ("flux_sersicN_SG_Cov" "real")
   ("e1_e1_SG_Cov" "real")
   ("e1_e2_SG_Cov" "real")
   ("e1_radius_SG_Cov" "real")
   ("e1_sersicN_SG_Cov" "real")
   ("e2_e2_SG_Cov" "real")
   ("e2_radius_SG_Cov" "real")
   ("e2_sersicN_SG_Cov" "real")
   ("radius_radius_SG_Cov" "real")
   ("radius_sersicN_SG_Cov" "real")
   ("sersicN_sersicN_SG_Cov" "real")
   ("flagForAssociation" "smallint")
   ("flagForDetection" "bigint")
   ("flagForWcs" "smallint")))

(define database-schema

  (schema-description
   
   ("Object" object-description 'virtual-table #f #f)
   ("Source" source-description 'virtual-table #f #f)
     
   ("Filter" filter-description 'physical-table master #f)
     
   ("ObjectBoundary" object-description 'physical-table master #f)
   ("SourceBoundary" source-description 'physical-table master #f)
     
   ("master_object_001_xyz" object-description 'physical-table pool3 '(0.95   -0.05   -0.15))
   ("master_object_002_xyz" object-description 'physical-table pool1 '(0.95   -0.05    -0.1))
   ("master_object_003_xyz" object-description 'physical-table pool1 '(0.95   -0.05   -0.05))
   ("master_object_004_xyz" object-description 'physical-table pool4 '(0.95   -0.05      0.))
   ("master_object_005_xyz" object-description 'physical-table pool2 '(0.95   -0.05    0.05))
   ("master_object_006_xyz" object-description 'physical-table pool3 '(0.95   -0.05     0.1))
   ("master_object_007_xyz" object-description 'physical-table pool2 '(0.95      0.   -0.15))
   ("master_object_008_xyz" object-description 'physical-table pool4 '(0.95      0.    -0.1))
   ("master_object_009_xyz" object-description 'physical-table pool1 '(0.95      0.   -0.05))
   ("master_object_010_xyz" object-description 'physical-table pool2 '(0.95      0.      0.))
   ("master_object_011_xyz" object-description 'physical-table pool3 '(0.95      0.    0.05))
   ("master_object_012_xyz" object-description 'physical-table pool3 '(0.95      0.     0.1))
   ("master_object_013_xyz" object-description 'physical-table pool3 '(0.95    0.05   -0.15))
   ("master_object_014_xyz" object-description 'physical-table pool1 '(0.95    0.05    -0.1))
   ("master_object_015_xyz" object-description 'physical-table pool3 '(0.95    0.05   -0.05))
   ("master_object_016_xyz" object-description 'physical-table pool4 '(0.95    0.05      0.))
   ("master_object_017_xyz" object-description 'physical-table pool2 '(0.95    0.05    0.05))
   ("master_object_018_xyz" object-description 'physical-table pool3 '(0.95    0.05     0.1))
 
   ("master_source_001_xyz" source-description 'physical-table pool3 '(0.95   -0.05   -0.15))
   ("master_source_002_xyz" source-description 'physical-table pool1 '(0.95   -0.05    -0.1))
   ("master_source_003_xyz" source-description 'physical-table pool1 '(0.95   -0.05   -0.05))
   ("master_source_004_xyz" source-description 'physical-table pool4 '(0.95   -0.05      0.))
   ("master_source_005_xyz" source-description 'physical-table pool2 '(0.95   -0.05    0.05))
   ("master_source_006_xyz" source-description 'physical-table pool3 '(0.95   -0.05     0.1))
   ("master_source_007_xyz" source-description 'physical-table pool2 '(0.95      0.   -0.15))
   ("master_source_008_xyz" source-description 'physical-table pool4 '(0.95      0.    -0.1))
   ("master_source_009_xyz" source-description 'physical-table pool1 '(0.95      0.   -0.05))
   ("master_source_010_xyz" source-description 'physical-table pool2 '(0.95      0.      0.))
   ("master_source_011_xyz" source-description 'physical-table pool3 '(0.95      0.    0.05))
   ("master_source_012_xyz" source-description 'physical-table pool3 '(0.95      0.     0.1))
   ("master_source_013_xyz" source-description 'physical-table pool3 '(0.95    0.05   -0.15))
   ("master_source_014_xyz" source-description 'physical-table pool1 '(0.95    0.05    -0.1))
   ("master_source_015_xyz" source-description 'physical-table pool3 '(0.95    0.05   -0.05))
   ("master_source_016_xyz" source-description 'physical-table pool4 '(0.95    0.05      0.))
   ("master_source_017_xyz" source-description 'physical-table pool2 '(0.95    0.05    0.05))
   ("master_source_018_xyz" source-description 'physical-table pool3 '(0.95    0.05     0.1))))

(define external-table->local-name
  (translation-table
   ("Filter"  "Filter")
   ("ObjectBoundary"         "ObjectBoundary")
   ("SourceBoundary"         "SourceBoundary")
   ("master_object_001_xyz"  "object_001_xyz") 
   ("master_object_002_xyz"  "object_002_xyz") 
   ("master_object_003_xyz"  "object_003_xyz") 
   ("master_object_004_xyz"  "object_004_xyz") 
   ("master_object_005_xyz"  "object_005_xyz") 
   ("master_object_006_xyz"  "object_006_xyz") 
   ("master_object_007_xyz"  "object_007_xyz") 
   ("master_object_008_xyz"  "object_008_xyz") 
   ("master_object_009_xyz"  "object_009_xyz") 
   ("master_object_010_xyz"  "object_010_xyz") 
   ("master_object_011_xyz"  "object_011_xyz") 
   ("master_object_012_xyz"  "object_012_xyz") 
   ("master_object_013_xyz"  "object_013_xyz") 
   ("master_object_014_xyz"  "object_014_xyz") 
   ("master_object_015_xyz"  "object_015_xyz") 
   ("master_object_016_xyz"  "object_016_xyz") 
   ("master_object_017_xyz"  "object_017_xyz") 
   ("master_object_018_xyz"  "object_018_xyz") 
                                             
   ("master_source_001_xyz"  "source_001_xyz") 
   ("master_source_002_xyz"  "source_002_xyz") 
   ("master_source_003_xyz"  "source_003_xyz") 
   ("master_source_004_xyz"  "source_004_xyz") 
   ("master_source_005_xyz"  "source_005_xyz") 
   ("master_source_006_xyz"  "source_006_xyz") 
   ("master_source_007_xyz"  "source_007_xyz") 
   ("master_source_008_xyz"  "source_008_xyz") 
   ("master_source_009_xyz"  "source_009_xyz") 
   ("master_source_010_xyz"  "source_010_xyz") 
   ("master_source_011_xyz"  "source_011_xyz") 
   ("master_source_012_xyz"  "source_012_xyz") 
   ("master_source_013_xyz"  "source_013_xyz") 
   ("master_source_014_xyz"  "source_014_xyz") 
   ("master_source_015_xyz"  "source_015_xyz") 
   ("master_source_016_xyz"  "source_016_xyz") 
   ("master_source_017_xyz"  "source_017_xyz") 
   ("master_source_018_xyz"  "source_018_xyz")))