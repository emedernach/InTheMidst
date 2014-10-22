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


(define-record-type :3d-point
  (3d-point id x y z)
  3d-point?
  (id 3d-point->id)
  (x  3d-point->x)
  (y  3d-point->y)
  (z  3d-point->z))

(define (celestial-coordinate->cartesian id right-ascension declination)
  (define degree (/ (* 2 (asin 1.0)) 180))
  ;; (define arc-minute (/ pi-constant 10800))
  ;; (define arc-second (/ arc-minute 60))
  
  (let* ((right-ascension (* right-ascension degree))
         (declination (* declination degree))
         (cos_declination (cos declination))
         (z (sin declination))
         (x (* (cos right-ascension) cos_declination))
         (y (* (sin right-ascension) cos_declination)))
    (3d-point id x y z)))

(define (object-points)
  (define (fun id-str ra-str decl-str)
    ;; SQL Results are strings !
    (let ((id   (string->number id-str))
          (ra   (string->number ra-str))
          (decl (string->number decl-str)))
      (celestial-coordinate->cartesian id ra decl)))
  
  (let* ((dbresult (execute-sql-select "SELECT * FROM Geometry_Object ;"))
         (result (database-result-vector-map fun dbresult)))
    ;; External code doesn't have a garbage collector 
    (free-result dbresult)
    result))

(define (count-points)
  (define knil 0)

  (define (kons content result)
    (+ result 1))
    
  (let* ((dbresult (execute-sql-select "SELECT * FROM Geometry_Object ;"))
         (result (database-result-fold kons knil dbresult)))
    ;; External code doesn't have a garbage collector 
    (free-result dbresult)
    result))

(define op (time (object-points)))

;; Parsing SQL.
;; Transforming AST.
;; Rewritten query = "SELECT * FROM Geometry_Object ;"
;; 
;; Querying database. (Please wait)
;; (time (libpq:pqexec %%connection4 %%sql6))
;;     4768 ms real time
;;     675 ms cpu time (467 user, 208 system)
;;     no collections
;;     72 bytes allocated
;;     119663 minor faults
;;     no major faults
;; (time (object-points))
;;     30347 ms real time
;;     26224 ms cpu time (25066 user, 1158 system)
;;     60 collections accounting for 5377 ms real time (5237 user, 130 system)
;;     15423894104 bytes allocated
;;     875726 minor faults
;;     no major faults

;; 4.7 s for querying database
;; 30.3 s to convert all points to a vector

