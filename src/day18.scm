;; NOTE: in hindsight, using a-star was complete overkill and overengineering.
;; I was banking on some change in part 2 that would have made a-star necessary,
;; but that is a classic case of solving a problem that didn't exist yet. In reality,
;; my implementation for part 2 didn't even use a-star! So, the lesson here is to start
;; with a simpler, more intuitive algorithm, and only optimize when needed. A-star is an
;; optimization over Djikstra's, Djikstra's is an optimization over BFS, so next time just
;; do damned BFS unless it's slow1
(define-module (day18) #:export (find-shortest-path find-blocking-byte))

(use-modules (util queue)
             (util input)
             (srfi srfi-1)
             (srfi srfi-11)
             (ice-9 curried-definitions)
             (ice-9 hash-table))

;; part 1
(define ((find-shortest-path size byte-limit) file)
 (define end (list (1- size) (1- size)))
 (define bytes (list->hash-set (take (parse-input file) byte-limit)))
 (hash-ref (a-star '(0 0) end size bytes) end))

;; part 2
(define ((find-blocking-byte size) file)
 (define end (list (1- size) (1- size)))
 (define bytes (parse-input file))
 (define (helper i)
  (if (hash-ref (a-star '(0 0) end size (list->hash-set (take bytes i))) end)
   (helper (1+ i))
   i))

 (list-ref bytes (1- (helper 0))))

;; common to both parts
(define (a-star start end size bytes)
 (define frontier (push '() start 0))
 (define cost-so-far (alist->hash-table (list (cons start 0))))
 (define (get-neighbours pos)
  (filter (lambda (p)
           (and (>= (first p) 0)
                (>= (second p) 0)
                (< (first p) size)
                (< (second p) size)
                (not (hash-ref bytes pos))))
          (apply neighbours pos)))

 (define (helper frontier cost-so-far)
  (if (or (null? frontier)
          (equal? (caar (pop frontier)) end))
   cost-so-far
   (let-values
    (((current rest) (pop frontier)))
    (apply
     helper
     (fold (lambda (next prev)
            (let* ((f (first prev))
                   (c (second prev))
                   (new-cost (1+ (hash-ref cost-so-far (car current))))
                   (next-cost (hash-ref cost-so-far next))
                   (priority (+ new-cost (apply manhattan (append next end)))))
             (if (or (not next-cost)
                     (< new-cost next-cost))
              (list (push f next priority)
                    (begin
                     (hash-set! c next new-cost)
                     (values c)))
              prev)))
           (list rest cost-so-far)
           (get-neighbours (car current)))))))

 (helper frontier cost-so-far))

(define (list->hash-set lst)
 (alist->hash-table (map (lambda (x)
                          (cons x #t))
                         lst)))

;; input parsing
(define (parse-input file)
 (map (lambda (line)
       (strings->numbers (string-split line #\,)))
      (lines file)))

(define (manhattan x1 y1 x2 y2)
 (+ (abs (- y1 y2)) (abs (- x1 x2))))
