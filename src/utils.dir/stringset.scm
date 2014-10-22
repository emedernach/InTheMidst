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
;;       list->string-set string-set string-set->list
;;       string-set-subset? string-set-union 
;;       )

;; Strings are considered equals up to capitalization

(define-record-type :string-set
  (string-set_ content)
  string-set?
  (content string-set->content))

(define (empty-string-set)
  (string-set_ '()))

(define (all-strings? mylist)
  (or (null? mylist)
      (and (pair? mylist)
           (string? (car mylist))
           (all-strings? (cdr mylist)))))

(define (string-set mylist)
  (if (all-strings? mylist)
      (string-set_ mylist)
      (error "string-set: not a list of strings"
             mylist)))

(define (list->string-set mylist)
  (string-set mylist))

(define (make-string-set . mylist)
  (string-set mylist))

(define string-set->list string-set->content)

(define (string-set-union . list-of-string-set)
  (let* ((mylist (map string-set->list list-of-string-set))
         (concat (apply append mylist)))
    (list->string-set concat)))

(define (string-set-insert my-string-set str)
  (if (string? str)
      (string-set_ (cons str (string-set->list my-string-set)))
      (error "string-set-insert: not a string" str)))

(define (string-set-intersect? string-set-A string-set-B)
  (let* ((list-A (string-set->list string-set-A))
         (list-B (string-set->list string-set-B))
         (result (list-intersection list-A list-B string-ci=?)))
    (string-set result)))

(define (string-set-subset? string-set-A string-set-B)
  (let ((list-A (string-set->list string-set-A))
        (list-B (string-set->list string-set-B)))
    (let loop ((list-A list-A))
      (or (null? list-A)
          (and (list-member? (car list-A) list-B string-ci=?)
               (loop (cdr list-A)))))))

(define (string-set-member? str my-string-set)
  (list-member?
   str (string-set->list my-string-set)
   string-ci=?))

(define (string-set-equal? string-set-A string-set-B)
  (and (string-set-subset? string-set-A string-set-B)
       (string-set-subset? string-set-B string-set-A)))
