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



;; When creating a table from a query, anonymous columns are
;; in conflict. We have to name them.

(define (deanonymize my-sql-record)

  (define new-name
    (let ((num 0))
      (lambda ()
        (let ((str (number->string num)))
          (set! num (+ num 1))
          (string-append "_anonymous_" str)))))

  (define (deanonymize-field f)
    (cond
     ((field? f) f)
     ((alias? f) f)
     ((arithmetic? f)
      (let ((new-f (parenthesised-expression f)))
        (make-alias (new-name) new-f)))
     ((parenthesised-expression? f)
      (make-alias (new-name) f))
     (else (error "deanonymize: Unknown type" f))))
  
  (cond ((sql-record? my-sql-record)
         (let* ((select     (sql-record->select my-sql-record))
                (from       (sql-record->from my-sql-record))
                (where      (sql-record->where my-sql-record))
                (group-by   (sql-record->group-by my-sql-record))
                (order-by   (sql-record->order-by my-sql-record))
                (limit      (sql-record->limit my-sql-record))
                (distinct?  (projection->distinct? select))
                (fields     (projection->fields select))
                (new-fields (map deanonymize-field fields))
                (new-select (projection distinct? new-fields)))
           (sql-record
            new-select from
            where group-by
            order-by limit)))
        ((table-combinator? my-sql-record)
         (let ((tble (table-combinator->table my-sql-record))
               (tbl-comb-list (table-combinator->list-of-table-combinators
                               my-sql-record)))
           (table-combinator_
            (deanonymize tble)
            (map deanonymize tbl-comb-list))))
        ((table-combinator-partial? my-sql-record)
         (let ((comb (table-combinator-partial->combinator my-sql-record))
               (tble (table-combinator-partial->table my-sql-record)))
           (table-combinator-partial_
            comb
            (deanonymize tble))))
        (else (error
               "deanonymize: Not a sql record"
               (pp (ast->sql my-sql-record))))))

(define (deanonymize:test)
  (deanonymize (sql->ast "select a,b from T;"))
  (deanonymize (sql->ast "select a+b from T;"))
  (deanonymize (sql->ast "select (a+b) from T;"))
  )
  