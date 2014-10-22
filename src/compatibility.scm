
;; Small incompatibilities but still a nuisance :
;; Guile:  (display 0.8) => 0.8
;; Gambit: (display 0.8) => .8

(cond-expand
 (guile

  ;; Syntax rules
  (use-syntax (ice-9 syncase))

  ;; SRFI 9
  (use-modules (srfi srfi-9))

  ;; Pretty printer
  (use-modules (ice-9 pretty-print))
  
  ;; current-time

  (define (present-time)
    (let* ((p (gettimeofday))
           (seconds (car p))
           (microseconds (cdr p)))
      (+ seconds (* (expt 0.1 6) microseconds))))

  (define-syntax run-with-elapsed-time
    (syntax-rules ()
      ((run-with-elapsed-time <body> ...)
       (let* ((start-time (present-time))
              (result (call-with-values (lambda () <body> ...) list))
              (finish-time (present-time)))
         (display '(run-with-elapsed-time <body> ...) (current-error-port))
         (newline (current-error-port))
         (display "    " (current-error-port))
         (display (- finish-time start-time) (current-error-port))
         (display " s" (current-error-port))
         (newline (current-error-port))
         (apply values result)))))

  
  ;; Hash tables
  (define table? hash-table?)
  (define make-table make-hash-table)
  (define table-ref hash-ref)
  (define table-set! hash-set!)
  (define (table->list mytable) (hash-map->list cons mytable))

  (debug-set! stack 200000)

  )
 
 (gambit

  (define (present-time)
    (time->seconds (current-time)))
  
  )
 (else
  (error "cond-expand: Unknown Scheme implementation")
  ))
