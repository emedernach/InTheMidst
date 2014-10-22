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

(define-ast-walker crossmatch-rewriter
  () ;; No arguments
  (dispatch  (crossmatch-list? crossmatch-list-dispatch))

  (define (crossmatch-list-dispatch obj)
    (let loop
        ((list-of-crossmatch  (crossmatch-list->list obj))
         (query               (crossmatch-list->query obj)))
      (if (null? list-of-crossmatch)
          (dispatch query)
          (let ((head (car list-of-crossmatch))
                (rest (cdr list-of-crossmatch)))
            (let ((table1  (crossmatch->table1 head))
                  (ra1     (crossmatch->ra1 head))
                  (decl1   (crossmatch->decl1 head))
                  (table2  (crossmatch->table2 head))
                  (ra2     (crossmatch->ra2 head))
                  (decl2   (crossmatch->decl2 head))
                  (radius  (crossmatch->radius head)))
              (let ((new-query
                     (spatial-join
                      table1 ra1 decl1
                      table2 ra2 decl2
                      radius
                      query
                      chunk-boundary-list 
                      FDW_modulo-partitions
                      FDW_table->boundary
                      )))
                (loop rest new-query))))))))
