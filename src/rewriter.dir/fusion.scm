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

;; TODO: inner-join unfolding to join-table-using

;; Fusion of distributed-query inner joins

(define-ast-walker distributed-query-fusion
  () ;; arguments
  (dispatch (inner-joins? inner-joins-dispatch))

  (define (inner-joins-dispatch ast)

    ;; returns #f or a list with key removed
    (define (join-fields-search join-fields-list key)
      (let loop ((join-fields-list join-fields-list)
                 (result '()))
        (and (not (null? join-fields-list))
             (let ((head (car join-fields-list))
                   (join-fields-list (cdr join-fields-list )))
               (if (and (= (length head) 1)
                        (let ((content (car head)))
                          (field-equal? content key)))                          
                   (append result join-fields-list)
                   (loop join-fields-list (cons head result)))))))
    
    (define (fusion distributed-tables-list common-key number)
      (if (null? (cdr distributed-tables-list))
          (car distributed-tables-list)
          (distributed-query
           (sql-record
            (projection #f (list (field 'unknown "*")))
            (inner-joins
             (map distributed-query->query
                  distributed-tables-list)
             (list (list common-key)))
            'empty-where 'empty-group
            'empty-order-by 'empty-limit)
           common-key number)))

    ;; TODO:
    ;;

    ;;  (fusion-search
    ;;   ()
    ;;   ((#<:distributed-query #2
    ;;                          query: #<:table-record #3 database: unknown name: "Object_">
    ;;                          key: #<:field #4 table: unknown column: "objectId">
    ;;                          number: 4>
    ;;     #<:distributed-query #5
    ;;                          query: #<:table-record #6 database: unknown name: "Source_">
    ;;                          key: #<:field #7 table: unknown column: "objectId">
    ;;                          number: 4>))
    ;;   ((#<:field #8 table: unknown column: "objectId">)))
    
    ;;  (fusion-loop
    ;;   ()
    ;;   ((#<:distributed-query #2
    ;;                          query: #<:table-record #3 database: unknown name: "Object_">
    ;;                          key: #<:field #4 table: unknown column: "objectId">
    ;;                          number: 4>
    ;;     #<:distributed-query #5
    ;;                          query: #<:table-record #6 database: unknown name: "Source_">
    ;;                          key: #<:field #7 table: unknown column: "objectId">
    ;;                          number: 4>))
    ;;   ((#<:field #8 table: unknown column: "objectId">)))

    ;; *** ERROR IN #<procedure #14> -- fusion-search: (null? join-fields-list) #!void

    (define (fusion-finalize result join-fields-list)
      (let ((result-length (length result))
            (join-length   (length join-fields-list)))
        (cond
         ((and (null? join-fields-list)
               (not (null? result))
               (null? (cdr result)))
          (let ((tmp (car result)))
            (if (distributed-query? tmp)
                (make-alias (string-append "__FUSION__" (number->string (gensym)))
                            (parenthesised-expression tmp))
                tmp)))                
         ((= result-length (+ join-length 1)) 
          (inner-joins result join-fields-list))
         (else (error "fusion-finalize: wrong length for a join"
                      (list result-length join-length))))))
               
    (define (fusion-loop result distributed-list-list join-fields-list)
      ;; result is a list of tables
      ;; (pp (list 'fusion-loop result distributed-list-list join-fields-list)) (newline)
      (if (null? distributed-list-list)
          (fusion-finalize result join-fields-list)
          (let* ((distributed-tables-list (car distributed-list-list))
                 (distributed-list-list (cdr distributed-list-list))
                 (common-key (distributed-query->key (car distributed-tables-list)))
                 (number (distributed-query->number (car distributed-tables-list)))
                 (new-join-fields-list (join-fields-search join-fields-list common-key)))
            (if new-join-fields-list
                (if (null? (cdr distributed-tables-list))
                    ;; No fusion required if there is only one table !
                    (fusion-loop (cons (car distributed-tables-list) result)
                                 distributed-list-list
                                 join-fields-list)
                    (let ((fusioned-table (fusion distributed-tables-list common-key number)))
                      (fusion-loop (cons fusioned-table result)
                                   distributed-list-list
                                   new-join-fields-list)))
                (fusion-loop (append distributed-tables-list result)
                             distributed-list-list
                             join-fields-list)))))
      
    (define (fusion-search non-distributed distributed-list-list join-fields-list)
      ;; non-distributed is a list of non distributed tables
      ;; distributed-list-list is a list of list of distributed tables on a common key
      ;; output is an inner-join-dispatch result
      ;; (pp (list 'fusion-search non-distributed distributed-list-list join-fields-list)) (newline)
      (fusion-loop non-distributed distributed-list-list join-fields-list))
      
    (define (insert dist-query distributed)
      (let* ((key (distributed-query->key dist-query))
             (box (table-ref distributed key 'empty)))
        (begin
          (if (eq? box 'empty)
              (table-set! distributed key (list dist-query))
              (table-set! distributed key (cons dist-query box)))
          distributed )))
        
              
    (define (split table-list join-fields-list)
      (let loop ((table-list table-list)
                 (non-distributed '())
                 (distributed (make-table)))
        (if (null? table-list)
            (let ((distributed-list (map cdr (table->list distributed))))
              (if (null? distributed-list) ast
                  (fusion-search non-distributed
                                 distributed-list
                                 join-fields-list)))
            (let ((head   (car table-list))
                  (others (cdr table-list)))
              (if (distributed-query? head)
                  (loop others
                        non-distributed
                        (insert head distributed))
                  (loop others
                        (cons head non-distributed)
                        distributed))))))

    ;; We have a list of tables and a list of join fields
    (let ((table-list  (inner-joins->table-list ast))
          (join-fields-list (inner-joins->join-fields-list ast)))
      ;; We must lookup for all distributed queries inside 'table-list'
      ;; (pp (list 'inner-joins-dispatch table-list join-fields-list)) (newline)
      (split table-list join-fields-list)))

  ;; End of distributed-query-fusion

  )
    

