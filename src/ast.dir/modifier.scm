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



(define-record-type :order-by
  (order-by direction fields)
  order-by?
  (direction  order-by->direction)
  (fields     order-by->fields))

(define-record-type :sql-limit
  (sql-limit number)
  sql-limit?
  (number  sql-limit->number))

(define-record-type :group-by
  (group-by fields having)
  group-by?
  (fields  group-by->fields)
  ;; having = predicate or #t
  (having  group-by->having))