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



(define-record-type :sql-record
  (sql-record select from where group-by order-by limit)
  sql-record?
  (select    sql-record->select)
  (from      sql-record->from)
  (where     sql-record->where)
  (group-by  sql-record->group-by)
  (order-by  sql-record->order-by)
  (limit     sql-record->limit))

;; (define (sql-record select from where group-by order-by limit)
;;   (if (and ;; select = *
;;        (projection? select)
;;        (let ((fields (projection->fields select)))
;;          (and (pair? fields)
;;               (null? (cdr fields))
;;               (let ((f (car fields)))
;;                 (and (field? f)
;;                      (eq? (field->table f) 'unknown)
;;                      (string=? (field->column f) "*")))))
;;        (eq? where 'empty-where)
;;        (eq? group-by 'empty-group)
;;        (eq? order-by 'empty-order-by)
;;        (eq? limit 'empty-limit))
;;       from
;;       (sql-record_ select from where group-by order-by limit)))

(define (select-all from)
  (let ((star (projection #f (list (field 'unknown "*")))))
    (sql-record
     star from
     'empty-where 'empty-group
     'empty-order-by 'empty-limit)))