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

(define (alias-unwrap ast)
  (if (alias? ast)
      (alias->value ast)
      ast))

(define (union-all tbl . tbl-list)
  (table-combinator
   (alias-unwrap tbl)
   (map (lambda (tbl)
          (table-combinator-partial
           "UNION ALL" tbl))
        (map alias-unwrap tbl-list))))

(define (modulo-partition->ast mp)
  (let ((name     (modulo-partition->name mp))
        (key      (modulo-partition->key mp))
        (number   (modulo-partition->number mp))
        (template (modulo-partition->template mp)))
    (apply
     union-all
     (map 
      (lambda (i)
        (sql-record
         (projection #f (list (field 'unknown "*")))
         (table-record 'unknown (template i))
         'empty-where 'empty-group 'empty-order-by 'empty-limit))
      (iota number)))))

(define-ast-walker replace-all-partition-tables
  (name->modulo-partition) ;; arguments
  (dispatch (distributed-query?  distributed-query-dispatch))

  (define (distributed-query-dispatch obj)
    (replace-distributed-query obj name->modulo-partition))

  ;; End of replace-all-partition-tables
  )


;; ---------------------------------------- ;;


;; ast =>
;; Union of all queries such that
;; ast i = replace all partitionned tables in original ast

;; exemple:

;; SELECT * FROM A JOIN B USING (id)
;; =>
;; SELECT * FROM A_001 JOIN B_001 USING (id)
;; UNION ALL
;; SELECT * FROM A_002 JOIN B_002 USING (id)
;; ...

;; for all modulo partition mp

(define-ast-walker replace-modulo-partition
  (name->modulo-partition index digits)
  (dispatch (table-record?  table-dispatch))

  (define (table-dispatch obj)
    (let ((database (table-record->database obj))
          (name     (table-record->name obj)))
      (let ((mp (name->modulo-partition name)))
        (if (modulo-partition? mp)
            (let ((name     (modulo-partition->name mp))
                  (key      (modulo-partition->key mp))
                  (number   (modulo-partition->number mp))
                  (template (modulo-partition->template mp)))
              (let ((new-name (template index)))
                (table-record 'unknown new-name)))
            obj))))

  ;; End of replace-modulo-partition
  )

(define (replace-distributed-query dq name->modulo-partition)

  (define (make-new-label)
    (string-append
     "__DISTRIB__"
     (number->string (gensym))))
   
  (let ((query  (distributed-query->query dq))
        (key    (distributed-query->key dq))
        (number (distributed-query->number dq)))
    (let* ((tmp (/ (log (+ number 1)) (log 10.)))
           (tmp (inexact->exact (ceiling tmp)))
           (digits (expt 10 tmp)))
      (select-all
       (make-alias
        (make-new-label)
        (parenthesised-expression 
         (apply union-all
                (map (lambda (i)
                       (replace-modulo-partition
                        query name->modulo-partition i digits))
                     (iota number)))))))))
                
  




