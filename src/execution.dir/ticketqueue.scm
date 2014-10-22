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



(define-record-type :ticket-queue
  (ticket-queue content length)
  ticket-queue?
  (content  ticket-queue->content)
  (length   ticket-queue->length))

(define (make-ticket-queue)
  (let ((empty-queue '()))
    (ticket-queue empty-queue 0)))

(define (ticket-queue->number tq)
  (ticket-queue->length tq))
         
(define (ticket-queue-push tq obj)
  (let ((content (ticket-queue->content tq))
        (length  (ticket-queue->length tq)))
    (let ((new-content (cons obj content)))
      (ticket-queue new-content (+ length 1)))))

(define (ticket-queue->vector tq)
  (let ((content (ticket-queue->content tq)))
    (list->vector (reverse content))))


(define (ticket-queue:test)
  (let ((tq (make-ticket-queue)))
    (display
     (list
      (ticket-queue->content tq)
      (ticket-queue->number tq)))
    (newline)
    (let* ((tq (ticket-queue-push tq 'a))
           (tq (ticket-queue-push tq 'b))
           (tq (ticket-queue-push tq 'c)))
      (display
       (list
        (ticket-queue->content tq)
        (ticket-queue->number tq)))
      (newline)
      (display (ticket-queue->vector tq))
      (newline)
      (let* ((tq (ticket-queue-push tq 'd))
             (tq (ticket-queue-push tq 'e)))
        (display
         (list
          (ticket-queue->content tq)
          (ticket-queue->number tq)))
        (newline)
        (display (ticket-queue->vector tq))
        (newline)))))
           
      
                   
         