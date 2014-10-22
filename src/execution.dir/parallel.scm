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



(define (parallel-for-each fun mylist)
  (define (make-pool-thread obj)
    (make-thread
     (lambda ()
       (thread-quantum-set!
        (current-thread) 0)
       (fun obj))))
  (let* ((thread-pool (map make-pool-thread mylist))
         (thread-pool (map thread-start! thread-pool)))
    (for-each thread-join! thread-pool)))

(define (parallel:test)
  (define (loop-and-display n)
    (display "Start of thread ") (display n) (newline)
    (let loop ((i 0))
      (and (< i 100000)
           (begin
             (display n)
             (loop (+ i 1)))))
    (display "End of thread ") (display n) (newline))

  (parallel-for-each loop-and-display (list 1 2 3 4)))