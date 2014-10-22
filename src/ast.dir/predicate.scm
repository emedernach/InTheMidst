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



(define-record-type :predicate
  (predicate first-expr rest-expr)
  predicate?
  (first-expr  predicate->first-expr)
  (rest-expr   predicate->rest-expr))

(define-record-type :partial-predicate
  (partial-predicate_ logic-operation predicate)
  partial-predicate?
  (logic-operation  partial-predicate->logic-operation)
  (predicate        partial-predicate->predicate))

(define (partial-predicate logic-operation predicate)
  (if (pair? predicate)
      (error "partial-predicate:" predicate)
      (partial-predicate_ logic-operation predicate)))

(define-record-type :between-predicate
  (between-predicate expr left-bound right-bound)
  between-predicate?
  (expr         between-predicate->expr)
  (left-bound   between-predicate->left-bound)
  (right-bound  between-predicate->right-bound))

(define-record-type :negation-predicate
  (negation-predicate predicate)
  negation-predicate?
  (predicate  negation-predicate->predicate))

(define-record-type :set-predicate
  (set-predicate expr set-of-values)
  set-predicate?
  (expr           set-predicate->expr)
  (set-of-values  set-predicate->set-of-values))

(define-record-type :comparison
  (comparison comparator left-expr right-expr)
  comparison?
  (comparator  comparison->comparator)
  (left-expr   comparison->left-expr)
  (right-expr  comparison->right-expr))

(define-record-type :like-operator
  (like-operator expr not? pattern)
  like-operator?
  (expr     like-operator->expr)
  (not?     like-operator->not?)
  (pattern  like-operator->pattern))

(define-record-type :is-null-record
  (is-null-record field not?)
  is-null-record?
  (field  is-null-record->field)
  (not?   is-null-record->not?))

(define-record-type :any-record
  (any-record subquery)
  any-record?
  (subquery  any-record->subquery))

(define-record-type :all-record
  (all-record subquery)
  all-record?
  (subquery  all-record->subquery))

(define-record-type :some-record
  (some-record subquery)
  some-record?
  (subquery  some-record->subquery))

(define-record-type :exists-record
  (exists-record subquery)
  exists-record?
  (subquery  exists-record->subquery))

;; ----

(define (binary-predicate op left-pred right-pred)
  (predicate
   left-pred
   (list (partial-predicate op right-pred))))

(define (and-predicate pred . list-of-pred)
  (predicate
   pred
   (map (lambda (p) (partial-predicate "AND" p))
        list-of-pred)))