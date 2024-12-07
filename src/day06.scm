(define-module (day06) #:export (count-guard-positions))

(use-modules (util io) (util input) (srfi srfi-1) (srfi srfi-9 gnu))

;; part 1
(define (count-guard-positions file)
 (define grid (make-grid file))
 (define visited (make-hash-table))
 (define (recursive-guard-step arr pos direction)
  (let* ((next-pos (next-position arr pos direction)))
   (begin
    (hash-set! visited pos #t)
    (cond
     ((not (is-in-bounds? arr next-pos))
      (hash-count (const #t) visited))
     ((is-obstruction? arr next-pos)
      (recursive-guard-step arr pos (rotate-right direction)))
     (else
      (recursive-guard-step arr next-pos direction))))))

 (recursive-guard-step grid
                       (array-index grid
                                    (lambda (cell)
                                     (equal? #\^ cell)))
                       'up))

;; assumes array is 2-d and square
(define (array-index arr pred)
 (let* ((size (array-length arr)))
  (car (filter-map (lambda (pos)
                    (if (pred (apply array-ref (cons arr pos)))
                     pos
                     #f))
                   (combinations-2 (iota size) (iota size))))))

(define (is-in-bounds? arr pos)
 (apply array-in-bounds? (cons arr pos)))

(define (is-obstruction? arr pos)
 (equal? #\# (apply array-ref (cons arr pos))))

(define (next-position arr pos direction)
 (let ((x (first pos))
       (y (second pos)))
  (case direction
   ((up)
    (list (1- x) y))
   ((right)
    (list x (1+ y)))
   ((down)
    (list (1+ x) y))
   ((left)
    (list x (1- y)))
   (else
    (raise-exception "unrecognized direction")))))

(define (rotate-right direction)
 (case direction
  ((up)
   'right)
  ((right)
   'down)
  ((down)
   'left)
  ((left)
   'up)
  (else
   (raise-exception "unrecognized direction"))))
