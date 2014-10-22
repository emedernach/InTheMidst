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
(include "../ast.dir/ast-macros.scm")

(define-record-type :placeholder
  (placeholder id)
  placeholder?
  (id  placeholder->id))

;; definition-vector contains a  vector of id -> replacement
;; It   is  used   for  replacing   placeholders   by  their
;; definitions.

(define-ast-walker replace-placeholders
  (definition-vector) ;;
  (dispatch 
   (placeholder? dispatch-placeholder)
   (parenthesised-expression? parenthesised-expression-dispatch)
   )

  (define (parenthesised-expression-dispatch obj)
    (let* ((expr  (parenthesised-expression->expr obj))
	   (new-expr (dispatch expr)))
      (if (table-record? new-expr) new-expr
	  (parenthesised-expression new-expr))))

  (define (dispatch-placeholder obj)
    (let ((id (placeholder->id obj)))
      (vector-ref definition-vector id))))