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



(define-record-type :cast-expression
  (cast-expression expr type)
  cast-expression?
  (expr  cast-expression->expr)
  (type  cast-expression->type))

(define-record-type :parenthesised-expression
  (parenthesised-expression_ expr)
  parenthesised-expression?
  (expr  parenthesised-expression->expr))

(define (parenthesised-expression expr)
  (cond ((table-record? expr) 
	 (error "parenthesised-expression: cannot put () around tables" expr))
	(else (parenthesised-expression_ expr))))

(define-record-type :arithmetic
  (arithmetic first-expr rest-expr)
  arithmetic?
  (first-expr  arithmetic->first-expr)
  (rest-expr   arithmetic->rest-expr))

(define-record-type :partial-arithmetic
  (partial-arithmetic operation expr)
  partial-arithmetic?
  (operation  partial-arithmetic->operation)
  (expr       partial-arithmetic->expr))

(define-record-type :unary-arithmetic
  (unary-arithmetic unary-op expr)
  unary-arithmetic?
  (unary-op   unary-arithmetic->unary-op)
  (expr       unary-arithmetic->expr))