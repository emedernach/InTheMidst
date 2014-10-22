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
(include "framedef.scm")

;; frames are very similar to  tables but are used in syntax
;; context to find variables bindings.

(define (frame-append . frame-list)
  (frame
   (apply append*
          (map frame->table-definition-list
               frame-list))))

;; -> field-type-list
(define (frame:find-table myframe name)
  (let loop ((table-definition-list (frame->table-definition-list myframe)))
    (and (not (null? table-definition-list))
         (let ((head (car table-definition-list))
               (rest (cdr table-definition-list)))
           (let ((table-name (table-definition->table-name head))
                 (field-type-list (table-definition->field-type-list head)))
             (if (string-ci=? table-name name)
                 field-type-list
                 (loop rest)))))))

(define frame->field-type-list frame:find-table)

;; mytable is a table-definition record
;; -> field-datatype
(define (table:find-column mytable field-column)
  (let ((field-type-list (table-definition->field-type-list mytable)))
    (let loop ((field-type-list field-type-list))
      (and (not (null? field-type-list))
           (let ((head (car field-type-list))
                 (rest (cdr field-type-list)))
             (let ((field-name  (field-type->field-name head))
                   (field-datatype  (field-type->field-datatype head)))
               (if (string-ci=? field-name field-column)
                   field-datatype
                   (loop rest))))))))

(define (field-type-list:find-column ft-list field-column)
  (and ft-list
       (not (null? ft-list))
       (let ((head (car ft-list))
             (rest (cdr ft-list)))
         (let ((field-name (field-type->field-name head)))
           (if (string-ci=? field-name field-column) head
               (field-type-list:find-column rest field-column))))))

;; -> field-datatype
(define (frame:find-column myframe field-column)
  (let loop ((table-definition-list
              (frame->table-definition-list myframe)))
    (and (not (null? table-definition-list))
         (let ((head (car table-definition-list))
               (rest (cdr table-definition-list)))
           (let ((result (table:find-column head field-column)))
             (if result result (loop rest)))))))
  
(define (bounded-in-frame? myframe myfield)
  (if (field? myfield)
      (let ((field-table  (field->table myfield))
            (field-column (field->column myfield)))
        (cond ((and (eq? field-table 'unknown)
                    (string=? field-column "*"))
               #t)
              ((string=? field-column "*")
               (if (frame:find-table myframe field-table) #t #f))
              ((eq? field-table 'unknown)
               (if (frame:find-column myframe field-column) #t #f))
              (else
               (let* ((ft-list (frame:find-table myframe field-table)))
                 (if (field-type-list:find-column
                      ft-list field-column)
                     #t #f)))))
      (error "bounded-in-frame?: Not a field" myfield)))

;; Returns a list of field-type
(define (frame:search-fields myframe field-list)

  (define (get-all-fields result)
    (let* ((all-tables (frame->table-definition-list myframe))
           (all-field-list
            (map table-definition->field-type-list
                 all-tables)))
      (apply append
             result
             all-field-list))) 
  
  (define (add-field-to-result myfield result)
    (cond ((field? myfield)
           (let ((field-table  (field->table myfield))
                 (field-column (field->column myfield)))
             (cond ((and (eq? field-table 'unknown)
                         (string=? field-column "*"))
                    (get-all-fields result))
                   ((string=? field-column "*") ;; T.*
                    (let ((field-type-list
                           (frame:find-table myframe field-table)))
                      (append field-type-list result)))                 
                   ((eq? field-table 'unknown) 
                    (let ((col (frame:find-column field-column)))
                      (cons col result)))
                   (else
                    (let* ((ft-list (frame:find-table myframe field-table))
                           (col (field-type-list:find-column
                                 ft-list field-column)))
                      (cons col result))))))
          ((alias? myfield)
           (let* ((name (alias->name  myfield))
                  (val  (alias->value myfield))
                  (val-type (add-field-to-result val '()))
                  (val-type
                   (if (null? (cdr val-type))
                       (car val-type)
                       (error "add-field-to-result: Non unique type ?" val)))
                  (val-datatype (field-type->field-datatype val-type))
                  (new-type (field-type name val-datatype)))
             (cons new-type result)))
	  ((function-call? myfield)
	   (let ((fun (function-call->function myfield)))
	     (cond ((string-ci=? fun "count")
		    (let ((new-type (field-type fun "bigint")))
		      (cons new-type result)))
		   (else (error "Unknown function call:" fun)))))	  
          (else (error "add-field-to-result: Not a field" myfield))))

  ;; (pp (list 'field-list field-list)) (newline)

  (let loop ((field-list field-list)
             (result '()))
    (if (null? field-list)
        result
        (let ((head (car field-list))
              (rest (cdr field-list)))
          (let ((result (add-field-to-result head result)))
            (loop rest result))))))
    

(define (frame:test)
  (unit-test
   (let ((frame_1 (frame (list (table-definition "T1" (list "a" "b" "c"))
                               (table-definition "T2" (list "a" "b" "c" "d")))))
         (frame_2 (frame (list (table-definition "T3" (list "b" "c"))
                               (table-definition "T4" (list "d" "e" "f")))))
         (frame_3 (frame (list (table-definition "T5" (list "x" "y"))
                               (table-definition "T6" (list "z"))
                               (table-definition "T7" (list "id" "u" "v"))))))
     (let* ((result (frame-append frame_1 frame_2 frame_3))
            (mylist (frame->table-definition-list result)))
       (= 7 (length mylist))))

   (let* ((myframe
           (frame (list (table-definition "T1" (list "a" "b" "c"))
                        (table-definition "T2" (list "a" "b" "c" "d"))
                        (table-definition "T3" (list "b" "c"))
                        (table-definition "T4" (list "d" "e" "f"))
                        (table-definition "T5" (list "x" "y"))
                        (table-definition "T6" (list "z"))
                        (table-definition "T7" (list "id" "u" "v")))))
          (result (frame:find-table myframe "T4")))
     (= 3 (length result))) 

   (let ((myframe
          (make-frame
           ("T1" (("a" "integer")
                  ("b" "real")
                  ("c" "real")))
           ("T2" (("d" "integer")
                  ("e" "real")))
           ("T3" (("f" "bigint")
                  ("g" "real")
                  ("h" "integer")
                  ("i" "real"))))))
     (and (bounded-in-frame? myframe (field "T2" "d"))
          (not (bounded-in-frame? myframe (field "T4" "*")))
          (bounded-in-frame? myframe (field "T3" "*"))
          (bounded-in-frame? myframe (field 'unknown "*"))
          (not (bounded-in-frame? myframe (field 'unknown "x")))
          (bounded-in-frame? myframe (field 'unknown "e"))))
          
   ))
   
