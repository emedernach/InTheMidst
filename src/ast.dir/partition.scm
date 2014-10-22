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



;; Virtual AST node types used for partitioning

(define-record-type :modulo-partition
  (modulo-partition name key number template)
  modulo-partition?
  (name     modulo-partition->name)
  (key      modulo-partition->key)
  (number   modulo-partition->number)
  (template modulo-partition->template))

;; A distributed query  describes a set of queries
;; to be run in parallel on all chunks, we get the
;; union at the end.
(define-record-type :distributed-query
  (distributed-query query key number)
  distributed-query?
  (query  distributed-query->query)
  (key    distributed-query->key)
  (number distributed-query->number))

(define-syntax make-modulo-partition-table
  (syntax-rules ()
    ((make-modulo-partition-table
      (name key number template)
      ...)
     (let ((result (make-table)))
       (table-set! result name (modulo-partition name key number template))
       ...
       (lambda (x) (table-ref result x #f))))))
