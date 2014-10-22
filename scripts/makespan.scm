
(load "src/utils.dir/sort-glue.scm")

(define-record-type :machine
  (machine load content)
  machine?
  (load     machine->load)
  (content  machine->content))

(define-record-type :task
  (task taskid load)
  task?
  (taskid  task->taskid)
  (load    task->load))

(define (makespan machines)
  (apply max
         (map machine->load
              (vector->list machines))))

(define (least-loaded-machine machines nbmachines)
  (let ((first-machine (vector-ref machines 0)))
    (let loop ((i 1)
               (current-load (machine->load first-machine))
               (result 0))
      (if (= i nbmachines) result
          (let* ((mach (vector-ref machines i))
                 (load (machine->load mach)))
            (if (< load current-load)
                (loop (+ i 1) load i)
                (loop (+ i 1) current-load result)))))))

(define (task-compare? t1 t2)
  (> (task->load t1)
     (task->load t2)))

(define (LPT nbmachines task-list)
  (let ((machines (make-vector nbmachines (machine 0 '())))
        (sorted-task-list (sort-glue task-list task-compare?)))
    (let loop ((task-list sorted-task-list))
      (if (null? task-list) machines
          (let* ((head (car task-list))
                 (rest (cdr task-list))
                 (i (least-loaded-machine machines nbmachines))
                 (mach (vector-ref machines i))
                 (load (machine->load mach))
                 (content (machine->content mach))
                 (new-load (+ load (task->load head)))
                 (new-content (cons head content))
                 (new-machine (machine new-load new-content)))
            (vector-set! machines i new-machine)
            (loop rest))))))

;; For double random :
;; task-compare? and select 2 different machines randomly and choose the least loaded

(define (list-scheduling task-compare? least-loaded-machine)
  (lambda (nbmachines task-list)
    (let ((machines (make-vector nbmachines (machine 0 '())))
          (sorted-task-list (sort-glue task-list task-compare?)))
      (let loop ((task-list sorted-task-list))
        (if (null? task-list) machines
            (let* ((head (car task-list))
                   (rest (cdr task-list))
                   (i (least-loaded-machine machines nbmachines))
                   (mach (vector-ref machines i))
                   (load (machine->load mach))
                   (content (machine->content mach))
                   (new-load (+ load (task->load head)))
                   (new-content (cons head content))
                   (new-machine (machine new-load new-content)))
              (vector-set! machines i new-machine)
              (loop rest)))))))

(define (random-select machines nbmachines)
  (let* ((i (random-integer nbmachines))
         (j (random-integer nbmachines))
         (i-load (machine->load (vector-ref machines i)))
         (j-load (machine->load (vector-ref machines j))))
    (if (< i-load j-load) i j)))

(define (random-compare? a b)
  (= (random-integer 2) 0))

(define double-random (list-scheduling random-compare? random-select))

