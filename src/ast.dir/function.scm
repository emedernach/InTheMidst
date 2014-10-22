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



(define-record-type :function-call
  (function-call function arguments)
  function-call?
  (function  function-call->function)
  (arguments function-call->arguments))

(define-record-type :crossmatch
  (crossmatch
   table1 ra1 decl1
   table2 ra2 decl2
   radius)
  crossmatch?
  (table1  crossmatch->table1)
  (ra1     crossmatch->ra1)
  (decl1   crossmatch->decl1)
  (table2  crossmatch->table2)
  (ra2     crossmatch->ra2)
  (decl2   crossmatch->decl2)
  (radius  crossmatch->radius))

(define-record-type :crossmatch-list
  (crossmatch-list
   list-of-crossmatch
   query)
  crossmatch-list?
  (list-of-crossmatch  crossmatch-list->list)
  (query               crossmatch-list->query))

