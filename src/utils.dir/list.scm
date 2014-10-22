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



;;      (export
;;       append* flatten fold iota keep list-interpose
;;       list-intersection list-member? list-of-list->list
;;       remove)

(define (fold fun zero mylist)
  (if (null? mylist)
      zero
      (let ((head (car mylist))
            (rest (cdr mylist)))
        (fold fun (fun head zero) rest))))

(define (list-search mylist pred?)
  (and (not (null? mylist))
       (let ((head (car mylist)))
         (if (pred? head)
             head
             (list-search (cdr mylist) pred?)))))           

;; SRFI 1 iota
(define (iota count)
  (let loop ((i 1) (result '()))
    (if (> i count)
        (reverse result)
        (loop (+ i 1) (cons i result)))))

(define (identity x) x)

(define (keep present? mylist)
  (let loop ((mylist mylist)
             (result '()))
    (if (null? mylist)
        (reverse result)
        (let ((head (car mylist))
              (rest (cdr mylist)))
          (loop rest
                (if (present? head)
                    (cons head result)
                    result))))))

(define (list-intersection list-A list-B compare?)
  (define (helper list-A list-B)
    (let loop-A ((list-A list-A)
                 (result '()))
      (if (null? list-A)
          result
          (let ((head-A (car list-A))
                (list-A (cdr list-A)))
            (let loop-B ((list-B list-B))
              (if (null? list-B)
                  (loop-A list-A result)
                  (let ((head-B (car list-B))
                        (list-B (cdr list-B)))
                    (if (compare? head-A head-B)
                        (loop-A list-A (cons head-A result))
                        (loop-B list-B)))))))))
  (let ((length-A (length list-A))
        (length-B (length list-B)))
    (if (< length-A length-B)
        (helper list-A list-B)
        (helper list-B list-A))))

(define (list-member? obj mylist compare?)
  (let loop ((mylist mylist))
    (and (not (null? mylist))
         (or (compare? obj (car mylist))
             (loop (cdr mylist))))))

(define (cons-end val mylist)
  (let* ((reversed-list (reverse mylist))
         (tmp (cons val reversed-list))
         (result (reverse tmp)))
    result))

(define (remove mylist list-of-val)
  (let loop ((mylist mylist)
             (result '()))
    (if (null? mylist)
        (reverse result)
        (let ((head (car mylist))
              (rest (cdr mylist)))
          (loop rest 
                (if (member head list-of-val)
                    result
                    (cons head result)))))))

(define (cons* mylist rest)
  (if (null? rest)
      mylist
      (cons* (cons (car rest) mylist)
             (cdr rest))))

;; Copy avoiding version of append
(define (append* . args)
  (define (helper l1 l2)
    ;; l2 is in general smaller than l1
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else
           (let loop ((reversed-l1 (reverse l1))
                      (l2 l2))
             (if (null? l2)
                 (reverse reversed-l1)
                 (loop (cons (car l2) reversed-l1)
                       (cdr l2)))))))
  (fold helper '() args))



;; (time (begin (append (iota 100000) (iota 100)) 0))
;; 43 ms

;; (define foo (iota 1000000))
;; (time (begin (reverse foo) 0))  ;; 53 ms
;; (time (begin (append foo (iota 100)) 0))  ;; 137 ms



'(define (append* mylist . rest)
   (let loop ((mylist mylist)
              (rest rest))
     (if (null? rest)
         mylist
         (let ((head (car rest))
               (tail (cdr rest)))
           (loop (cons* mylist head) tail)))))

(define (append-if-not-false . all-list)
  (apply append (keep identity all-list)))      

(define (all-true some-list)
  (or (null? some-list)
      (and (car some-list)
           (all-true (cdr some-list)))))

(define (num-max mylist)
  (let loop ((i 0)
             (current-index 1)
             (current-max (car mylist))
             (mylist (cdr mylist)))
    (if (null? mylist)
        (list i current-max)
        (let ((head (car mylist))
              (rest (cdr mylist))
              (next-index (+ current-index 1)))
          (if (> head current-max)
              (loop current-index next-index head rest)
              (loop i next-index current-max rest))))))

(define (num-min mylist)
  (let loop ((i 0)
             (current-index 1)
             (current-max (car mylist))
             (mylist (cdr mylist)))
    (if (null? mylist)
        (list i current-max)
        (let ((head (car mylist))
              (rest (cdr mylist))
              (next-index (+ current-index 1)))
          (if (< head current-max)
              (loop current-index next-index head rest)
              (loop i next-index current-max rest))))))

(define (list-seq nmin nmax)
  (let loop ((index nmax)
             (result '()))
    (if (< index nmin)
        result
        (loop (- index 1)
              (cons index result)))))

(define (list-of-max-sorted sorted-list get-val get-content)
  (if (null? sorted-list)
      sorted-list
      (let* ((head (car sorted-list))
             (rest (cdr sorted-list))
             (max-list (get-val head)))
        (let loop ((sorted-list rest)
                   (result (list (get-content head))))
          (if (null? sorted-list)
              result
              (let* ((head (car sorted-list))
                     (rest (cdr sorted-list))
                     (val (get-val head)))
                (if (= max-list val)
                    (loop rest (cons (get-content head) result))
                    result)))))))

(define (list-of-list->list mylist)
  (let loop ((mylist mylist)
             (result '()))
    (if (null? mylist)
        (reverse result)
        (let ((head (car mylist))
              (rest (cdr mylist)))
          (loop rest
                (cond ((null? head) result)
                      ((pair? head) (append (reverse head) result))
                      (else (cons head result))))))))

(define (flatten mylist)
  (define (helper head result)
    (cond ((null? head) result)
          ((pair? head) (fold helper result head))
          (else (cons head result))))
  (if (pair? mylist)
      (reverse (fold helper '() mylist))
      mylist))

(define (flatten-all flist . start+end)

  (define (helper flist result)
    (if (pair? flist)
        (let ((head (car flist))
              (rest (cdr flist)))
          (if (pair? head)
              (helper rest (append (reverse head) result))
              (helper rest (cons head result))))
        (reverse result)))
  
  (define (helper-with-end flist result end)
    (if (pair? flist)
        (let ((head (car flist))
              (rest (cdr flist)))
          (if (pair? head)
              (helper-with-end rest (append (reverse head) result) end)
              (helper-with-end rest (cons head result) end)))
        (reverse (cons end result))))
  
  (case (length start+end)
    ((0) (helper flist '()))
    ((1) (helper flist start+end))
    ((2) (let ((start (car start+end))
               (end (cadr start+end)))
           (helper-with-end flist (list start) end)))
    (else (error 'flatten-all "Wrong number of arguments"))))

(define (flatten-with-parentheses mylist)
  (cond ((pair? mylist)
         (let ((flist (map flatten-with-parentheses mylist)))
           (flatten-all flist "(" ")")))
        (else mylist)))

;; Example:
;; > (flatten-with-parentheses '(a b (c d ((e f) g) (h (i j) k)) l)) 
;; ("(" a b "(" c d "(" "(" e f ")" g ")" "(" h "(" i j ")" k ")" ")" l ")")

(define (list-interpose obj mylist)
  (cond ((null? mylist) mylist)
        ((null? (cdr mylist)) mylist)
        (else
         (let ((head (car mylist))
               (rest (cdr mylist)))
           (let ((new-list (map (lambda (x) (list obj x))
                                rest)))
             (cons head new-list))))))

;; ---------------------------------------- ;;

(define (list:benchmark)
  (define list-of-list
    (let* ((n 1000)
           (mylist (iota n)))
      (map (lambda (i)
             (map (lambda (j) (+ (* i n) j))
                  mylist))
           mylist)))
  ;;  (time (begin (length (list-of-list->list list-of-list)))))
  (time (begin (length (flatten list-of-list)))))

