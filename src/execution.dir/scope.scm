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

;; Scopes must be  self sufficient to find if  a variable is
;; bounded or not.

(define-record-type :scope
  (scope frame-list)
  scope?
  (frame-list  scope->frame-list))

(define (make-scope)
  (scope '()))

;; fr is a frame
(define (scope-add-frame sc fr)
  (if (frame? fr)
      (let ((frame-list (scope->frame-list sc)))
        (scope (cons fr frame-list)))
      (error "Not a frame" fr)))

;; f is a field
(define (bounded-in-scope? sc f)
  (let loop ((frame-list (scope->frame-list sc)))
    (and (not (null? frame-list))
         (or (bounded-in-frame? (car frame-list) f)
             (loop (cdr frame-list))))))

(define (scope->field-type-list sc name)
  (let loop ((frame-list (scope->frame-list sc)))
    (and (not (null? frame-list))
         (or (frame->field-type-list (car frame-list) name)
             (loop (cdr frame-list))))))


(define (scope:test)
  (unit-test

   (let* ((frame_1
           (make-frame
            ("T1" (("a" "integer")
                   ("b" "real")
                   ("c" "real")))
            ("T2" (("d" "integer")
                   ("e" "real")))))
          (frame_2
           (make-frame
            ("T3" (("f" "bigint")
                   ("g" "real")
                   ("h" "integer")
                   ("i" "real")))))
          (myscope (make-scope))
          (myscope (scope-add-frame myscope frame_1))
          (myscope (scope-add-frame myscope frame_2)))
     (and (bounded-in-scope? myscope (field "T2" "e"))
          (not (bounded-in-scope? myscope (field 'unknown "z")))
          (bounded-in-scope? myscope (field "T3" "*"))
          (bounded-in-scope? myscope (field 'unknown "*"))))

   (let* ((frame_1
           (make-frame
            ("T1" (("a" "integer")
                   ("b" "real")
                   ("c" "real")))
            ("T2" (("d" "integer")
                   ("e" "real")))))
          (frame_2
           (make-frame
            ("T3" (("f" "bigint")
                   ("g" "real")
                   ("h" "integer")
                   ("i" "real")))))
          (myscope (make-scope))
          (myscope (scope-add-frame myscope frame_1))
          (myscope (scope-add-frame myscope frame_2)))
     (= 2 (length (scope->field-type-list myscope "T2"))))
   
   ))