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

;; Validate if all joins are legal 

(define-ast-walker join-validation
  (table->fields) ;; arguments
  (dispatch
   (join-table-using? join-table-using-dispatch)
   (join-table-using-partial?  join-table-using-partial-dispatch))

  ;; Dispatchers

  (define (join-table-using-dispatch obj)
    (let ((type         (join-table-using->type obj))
          (left-table   (dispatch (join-table-using->left-table obj)))
          (right-table  (dispatch (join-table-using->right-table obj)))
          (join-fields  (join-table-using->join-fields obj)))

      (let ((left-table-fields  (collect-fields left-table  table->fields))
            (right-table-fields (collect-fields right-table table->fields)))
        (if (and (collect-fields-subset? join-fields left-table-fields)
                 (collect-fields-subset? join-fields right-table-fields))
            obj
            (error "Invalid join: fields used are not in both tables."
                   join-fields)))))

  (define (join-table-using-partial-dispatch obj)
    (let ((type  (join-table-using-partial->type obj))
          (table (join-table-using-partial->table obj))
          (join-fields (join-table-using-partial->join-fields obj)))

      (error "join-table-using-partial should not be there")))

  ;; End of join-validation
  )
  