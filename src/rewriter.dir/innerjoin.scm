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

;; inner-join unfolding to join-table-using

;; 
;; T_0 JOIN
;; T_1 USING (FL_1)
;; ...
;; T_i USING (FL_i)
;; ...
;; T_N USING (FL_N)
;; 
;; T_i may be a subquery, SQL requires subqueries to have an alias name:
;; 
;;  T_i = ( <SQL> ) AS <NAME>
;; 
;; <NAME>  could  be used  later  in  conditionals  but either  we  could
;; distribute this condition inside <SQL>  or we could remove <NAME> (use
;; directly the joined tables)
;; 
;; T_0 JOIN
;; T_1 USING (FL_1)
;; ...
;; T_i USING (FL_i)
;; ...
;; T_N USING (FL_N)
;; 
;;  <==>
;; 
;; T_0, T_1, ..., T_N
;; WHERE
;;     T_0.FL_1 = T_1.FL_1
;; AND ...
;; AND [Union of T_{1} ... T_{i-1}].FL_i = T_i.FL_i
;; AND ...
;; AND [Union of T_{1} ... T_{N-1}].FL_N = T_N.FL_N
;; 
;; But as columns grow we could permute joins
;; 
;; --
;; 
;; The inverse problem is this:
;; {T_0, .., T_N} and {FL_1, .., FL_N}
;; Find permutations p,q such that
;; 
;; T_q(0) JOIN
;; T_q(1) USING (FL_p(1))
;; ...
;; T_q(i) USING (FL_p(i))
;; ...
;; T_q(N) USING (FL_p(N))
;; 
;; is a valid join
;; 
;; It is valid iff
;; 
;; FL_p(i) uses a set of fields both present in T_q(i)
;; and in Union(T_q(0), .., T_q(i-1))
;; 
;; So first extract fields set from all FL_i: FS_i
;; 
;; we extract fields set from all T_i: TS_i
;; 
;; Then for each FS_i we search of which TS_j it is a subset.
;; 
;; FL_p(1) must be a subset of both TS_q(0) and TS_q(1)
;; 
;; When we found  a FL_i which is a subset of  at least 2 table
;; field set  we could join them,  we obtain a  new table which
;; field set is an union of both table field set.
;; 
;; So ...
;; 
;; For  each  FL_i  we  have   a  set  of  T_j  with  non-empty
;; intersection and a set of T_k for which FL_i is a subset.
;; 
;; Then we scan  and make a join each time there  is at least 2
;; tables in the T_k set.
;; 
;; We continue scanning until only one table is left.

(define-ast-walker inner-join-unfolding
  (table->fields) ;; arguments
  (dispatch (inner-joins? inner-joins-dispatch))

  (define (make-new-label)
    (string-append
     "__WRAPPED_INNERJOIN__"
     (number->string (gensym))))
  
  (define (wrap-table-if-necessary mytable)
    (cond ((sql-record? mytable)
           (make-subquery mytable (make-new-label)))
          ((or (alias? mytable)
               (table-record? mytable)
               (join-table-using? mytable)
               (distributed-query? mytable))
           ;;  We  don't put alias  around distributed table
           ;; because it will  be added when we will replace
           ;; all distributed tables.
           mytable)
          (else (error "wrap-table-if-necessary: Unknown type" mytable))))
  
  (define (make-atomic-inner-join first-table second-table join-fields)
    ;; (join-table-using 'inner first-table second-table join-fields)
    (let ((first-table-wrapped  (wrap-table-if-necessary first-table))
          (second-table-wrapped (wrap-table-if-necessary second-table)))
      (join-table-using 'inner
                        first-table-wrapped
                        second-table-wrapped
                        join-fields)))

  (define (scan-for-subset join-fields table-list)
  
    ;; Returns #f if no possible join was found
    ;; or a new table-list if we found one.
    (define (scan-for-subset_ join-fields table-list)
      (let loop ((table-list table-list)
                 (subset-list '())
                 (non-subset-list '()))
        (if (null? table-list)
            (and (> (length subset-list) 1)
                 ;; We found a possible join
                 (let* ((first-table  (list-ref subset-list 0))
                        (second-table (list-ref subset-list 1))
                        (others-table (list-tail subset-list 2))
                        (new-join (make-atomic-inner-join first-table second-table join-fields)))
                   (cons new-join (append others-table non-subset-list))))
            (let* ((table  (car table-list))
                   (table-list (cdr table-list))
                   (table-fields-set (collect-fields table table->fields)))
              (if (collect-fields-subset? join-fields table-fields-set)
                  (loop table-list
                        (cons table subset-list)
                        non-subset-list)
                  (loop table-list
                        subset-list
                        (cons table non-subset-list)))))))

    (let ((result (scan-for-subset_ join-fields table-list)))
      result))

  (define (maximal-join-fields? join-fields join-fields-list)
    (let ((join-fields-str (string-set (map field->column join-fields))))
      (let loop ((join-fields-str-list
                  (map (lambda (jf) (string-set (map field->column jf)))
                       join-fields-list)))
        (or (null? join-fields-str-list)
            (let ((head (car join-fields-str-list))
                  (join-fields-str-list (cdr join-fields-str-list)))
              (and (not (string-set-subset? join-fields-str head))
                   (loop join-fields-str-list)))))))
  
  (define (scan-loop join-fields-list
                     other-join-fields-list
                     table-list)

    
    (if (null? join-fields-list)
        (if (null? other-join-fields-list)
            (cond ((null? table-list)
                   (error "scan-loop: table list is null"
                          (list 'scan-loop join-fields-list table-list)))
                  ((not (null? (cdr table-list)))
                   (error "scan-loop: table list contains more than one table"
                          (list 'scan-loop join-fields-list table-list)))
                  (else (car table-list)))
            (scan-loop other-join-fields-list '() table-list))
        (let* ((join-fields (car join-fields-list))
               (join-fields-list (cdr join-fields-list))
               (max?  (maximal-join-fields? join-fields join-fields-list)))
          
          ;; TODO: Attention si  join-fields est un sous-ensemble d'un
          ;; autre  dans join-fields-list alors on risque  de faire la
          ;;  premiere jointure  et de  ne  plus pouvoir  faire la  2e
          ;;  jointure !  Par consequent  si le  cas apparait  il faut
          ;; scanner ce join-fields plus tard.
          
          (if max?
              (let ((result (scan-for-subset join-fields table-list)))
                ;; (pp (list 'result result join-fields table-list )) (newline)
                (if result
                    (scan-loop join-fields-list other-join-fields-list result)
                    (scan-loop join-fields-list
                               (cons join-fields other-join-fields-list)
                               table-list)))
              (scan-loop join-fields-list
                         (cons join-fields other-join-fields-list) 
                         table-list))))) 
        
  ;; Dispatchers
  
  (define (inner-joins-dispatch ast)
    (let* ((table-list  (map dispatch (inner-joins->table-list ast)))
           (join-fields-list (inner-joins->join-fields-list ast)))
      (scan-loop join-fields-list '() table-list)))
      
  ;; End of inner-join-unfolding
  )


  