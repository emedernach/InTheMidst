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


(define-record-type :case-expr
  (case-expr case-clauses-list else-clause)
  case-expr?
  (case-clauses-list  case-expr->case-clauses-list)
  (else-clause        case-expr->else-clause))

(define-record-type :case-value
  (case-value expr case-clauses-list else-clause)
  case-value?
  (expr  case-value->expr)
  (case-clauses-list  case-value->case-clauses-list)
  (else-clause        case-value->else-clause))

(define-record-type :case-clause
  (case-clause predicate expr)
  case-clause?
  (predicate  case-clause->predicate)
  (expr       case-clause->expr))

(define-record-type :else-clause
  (else-clause expr)
  else-clause?
  (expr  else-clause->expr))



  