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



(define-syntax alist->function
  (syntax-rules ()
    ((alist->function ((<key> ...) <val>) ...)
     (let ((hashtable (make-table)))
       (table-set! hashtable (list <key> ...) <val>) ... 
       (lambda args
         (table-ref hashtable args #f))))))

(define-syntax make-schema
  (syntax-rules ()
    ((make-schema
      (<table-name> (<column> ...))
      ...)
     (let ((result (make-table)))
       (table-set! result <table-name> (<column> ...))
       ...
       result))))
       
(define-syntax add-distributed-tables
  (syntax-rules ()
    ((add-distributed-tables
      <schema> <chunkid-list>
      (<template> <columns>)
      ...)
     (let ((result <schema>))
       (for-each
        (lambda (chunkid)
          (let ((table-name (<template> chunkid)))
            (table-set!
             result table-name
             (columns table-name <columns>))))
        <chunkid-list>)
       ...
       result))))


