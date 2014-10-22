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



;; CURRENTLY UNUSED

(include "../macros.dir/macros.scm")


;; Functional queues
;; Based on Okasaki's 1998 book "Purely functional data structures"

(define-record-type :internal-queue
  (internal-queue back front)
  internal-queue?
  (back  internal-queue->back)
  (front internal-queue->front))

;; Used to append queues but the result is too slow !
(define-record-type :compound-queue
  (compound-queue queues)
  compound-queue?
  (queues  compound-queue->queues))

(define (queue? obj)
  (or (internal-queue? obj)
      (compound-queue? obj)))

(define (queue . args) (internal-queue '() args))

(define (queue-length q)
  (+ (length (internal-queue->back q))
     (length (internal-queue->front q))))

(define (list->queue l) (internal-queue '() l))

(define (queue-empty? q)
  (cond ((internal-queue? q)
         (let ((back  (internal-queue->back q))
               (front (internal-queue->front q)))
           (and (null? back) (null? front))))
        ((compound-queue? q)
         (let loop ((queues (compound-queue->queues q)))
           (or (null? queues)
               (let ((head (car queues))
                     (rest (cdr queues)))
                 (and (queue-empty? head)
                      (loop rest))))))))

(define (queue-append . queue-list)
  (let loop ((queue-list queue-list))
    (if (null? queue-list)
        (queue)
        (let ((head (car queue-list))
              (rest (cdr queue-list)))
          (if (queue-empty? head)
              (loop rest)
              (compound-queue queue-list))))))

(define (queue-push obj q)
  (cond ((internal-queue? q)
         (let ((back  (internal-queue->back q))
               (front (internal-queue->front q)))
           (internal-queue (cons obj back) front)))
        ((compound-queue? q)
         (let ((queues (compound-queue->queues q)))
           ;;  Take the  last  queue and  push  in it,  then
           ;; recreate  a queue from the others  and the new
           ;; last queue.
           (if (null? queues)
               (queue obj)
               (let* ((reversed-queues (reverse queues))
                      (last   (car reversed-queues)) 
                      (new-last (queue-push obj last))
                      (others (cdr reversed-queues))
                      (new-reversed-queues (cons new-last others))
                      (result (reverse new-reversed-queues)))
                 (apply queue-append result)))))))

(define (queue-pop q empty-cont non-empty-cont)
  (define (internal-queue-pop)

    (define (helper back front)
      (let ((top (car front))
            (new-front (cdr front)))
        (non-empty-cont top (internal-queue back new-front))))
    
    (let ((back  (internal-queue->back q))
          (front (internal-queue->front q)))
      (if (and (null? back) (null? front))
          (empty-cont)
          (if (null? front)
              (helper '() (reverse back))
              (helper back front)))))
  
  (define (compound-queue-pop)
    ;;  Take  the front  pop its element  and rebuild  a new
    ;;  queue with the new front and the rest
    (let ((queues (compound-queue->queues q)))
      (if (null? queues)
          (empty-cont)
          (let loop ((first-queue (car queues))
                     (rest-queue  (cdr queues)))
            (queue-pop
             first-queue
             (lambda ()
               (if (null? rest-queue)
                   (empty-cont)
                   (loop (car rest-queue)
                         (cdr rest-queue))))
             (lambda (obj new-first-queue)
               (let* ((queues (if (queue-empty? new-first-queue)
                                  rest-queue
                                  (cons new-first-queue rest-queue)))
                      (new-queue (cond ((null? queues) (queue))
                                       ((= (length queues) 1) (car queues))
                                       (else (apply queue-append queues)))))
                 (non-empty-cont obj new-queue))))))))
  
  (cond ((internal-queue? q) (internal-queue-pop))
        ((compound-queue? q) (compound-queue-pop))
        (else (error "queue-pop" q))))

(define (queue->list q)
  (let loop ((q q) (result '()))
    (queue-pop
     q
     (lambda () (reverse result))
     (lambda (obj new-q)
       (loop new-q (cons obj result))))))

(define (queue:test)
  (let* ((q (queue 1 2 3 4))
         (q1 (queue 10 20 30))
         (q2 (queue 11 21 31))
         (q3 (queue 12 22 32))
         (qq (queue-append q1 q2 q3))
         (qqq (queue-append (queue-append q1 q2) q3))
         )
    (unit-test
     (equal? (queue->list q) '(1 2 3 4))
     (equal? (queue->list (queue-push 5 q)) '(1 2 3 4 5))
     (queue-pop q (lambda () #f)
                (lambda (obj new-q)
                  (and (= obj 1)
                       (equal? (queue->list new-q) '(2 3 4)))))
     (equal? (queue->list qq)
             '(10 20 30 11 21 31 12 22 32))
     (equal? (queue->list (queue-push 5 qq))
             '(10 20 30 11 21 31 12 22 32 5))
     (queue-pop
      qq
      (lambda () #f)
      (lambda (obj new-qq)
        (and (= obj 10)
             (equal? (queue->list new-qq)
                     '(20 30 11 21 31 12 22 32)))))
     (queue-pop
      qqq
      (lambda () #f)
      (lambda (obj new-qqq)
        (and (= obj 10)
             (equal? (queue->list new-qqq)
                     '(20 30 11 21 31 12 22 32)))))
     )))


