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



(define (parenthesised-alias? ast)
  (and (alias? ast)
       (let ((content (alias->value ast)))
         (and (parenthesised-expression? content)
              (parenthesised-expression->expr content)))))

(define (sql-subquery? ast)
  (let ((content (parenthesised-alias? ast)))
    (and content
         (sql-record? content))))

(define (combined-subquery? ast)
  (let ((content (parenthesised-alias? ast)))
    (and content
         (table-combinator? content))))

(define (distributed-subquery? ast)
  (let ((content (parenthesised-alias? ast)))
    (and content
         (distributed-query? content))))

(define (make-subquery subquery name)
  (make-alias name (parenthesised-expression subquery)))
