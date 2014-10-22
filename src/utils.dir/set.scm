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



;;      (export
;;       list->set make-set set->list
;;       set-equal? set-union set?
;;       )

;; TODO:  add  a type  marker  to avoid  confusion
;; between hash tables and sets.

(define (set? obj) (table? obj))

(define (empty-set) (make-table))

(define (list->set mylist)
  (let ((hash-table (make-table)))
    (for-each (lambda (val)
                (if (string? val)
                    (error "String set must use string-set instead of set." mylist)
                    (table-set! hash-table val #t)))
              mylist)
    hash-table))

(define (make-set . elements)
  (list->set elements))

(define (set->list myset)
  (map car (table->list myset)))

(define (set-member? element myset)
  (table-ref myset element #f))

(define (set-union . list-of-sets)
  (list->set
   (apply append
          (map set->list
               list-of-sets))))

;; Anyway we have to create a copy of myset and this is already O(N)
(define (set-insert myset element)
  (cond ((not (set? myset))
         (error "set-insert: not a set" myset))
        ((string? element)
         (error "String set must use string-set instead of set."))
        (else
         (list->set (cons element (set->list myset))))))

(define (list-set-intersect? list-A set-B)
  (and (not (null? list-A))
       (let ((head (car list-A))
             (rest (cdr list-A)))
         (or (set-member?  head set-B)
             (list-set-intersect? rest set-B)))))

(define (list-set-subset? list-A set-B)
  (let loop ((list-A list-A))
    (or (null? list-A)
        (let ((head (car list-A))
              (rest (cdr list-A)))
          (and (set-member?  head set-B)
               (loop rest))))))

(define (set-subset? set-A set-B)
  (list-set-subset? (set->list set-A) set-B))

(define (set-equal? set-A set-B)
  (and (set-subset? set-A set-B)
       (set-subset? set-B set-A)))

(define (set:benchmark)
  (define list-of-sets
    (let* ((n 1000)
           (mylist (iota n)))
      (map (lambda (i)
             (list->set
              (map (lambda (j) (+ (* i n) j))
                   mylist)))
           mylist)))
  (time (apply set-union list-of-sets)))
