(define-module (day06)
               #:export
               (count-guard-positions count-good-obstructions))

(use-modules (util input) (srfi srfi-1))

;; part 1
(define (count-guard-positions file)
 (hash-count (const #t) (guard-positions (make-grid file))))

;; part 2
;; get the positions visited without obstructions
;; for each of these positions:
;;      run the guard steps again:
;;      - if out of bounds, no cycle
;;      - if reached same (position, direction) more than once, cycle
;;      - otherwise, keep going
;; 
(define (count-good-obstructions file)
 (define arr (make-grid file))
 (define start-pos
         (array-index arr
                      (lambda (cell)
                       (equal? #\^ cell))))
 (define obstruction-positions
         (hash-map->list (lambda (k v)
                          k)
                         (guard-positions arr)))
 (length (filter (lambda (pos)
                  (causes-cycle? arr pos start-pos 'up))
                 obstruction-positions)))

(define (causes-cycle? arr obstruction-pos start-pos start-direction)
 (define visited (make-hash-table))
 (define (recursive-guard-step arr pos direction)
  (let* ((next-pos (next-position arr pos direction)))
   (begin
    (hash-increment visited (list pos direction))
    (cond
     ((not (is-in-bounds? arr next-pos))
      #f)
     ;; cycle check
     ((> (hash-ref visited (list pos direction) 0) 1)
      #t)
     ((or (is-obstruction? arr next-pos)
          (equal? next-pos obstruction-pos))
      (recursive-guard-step arr pos (rotate-right direction)))
     (else
      (recursive-guard-step arr next-pos direction))))))

 (recursive-guard-step arr start-pos 'up))

;; common to both parts
(define (guard-positions arr)
 (define visited (make-hash-table))
 (define (recursive-guard-step arr pos direction)
  (let* ((next-pos (next-position arr pos direction)))
   (begin
    (hash-increment visited pos)
    (cond
     ((not (is-in-bounds? arr next-pos))
      visited)
     ((is-obstruction? arr next-pos)
      (recursive-guard-step arr pos (rotate-right direction)))
     (else
      (recursive-guard-step arr next-pos direction))))))

 (recursive-guard-step arr
                       (array-index arr
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

(define (hash-increment hash-table key)
 (hash-set! hash-table key (1+ (hash-ref hash-table key 0))))
