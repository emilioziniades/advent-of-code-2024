(define-module (day10) #:export (trailhead-scores trailhead-ratings))

(use-modules (util io) (util input) (srfi srfi-1) (ice-9 curried-definitions))

;; part 1
(define (trailhead-scores file)
 (let* ((arr (make-grid file))
        (trailheads (find-trailheads arr)))
  (list-sum
   ;; deduplicate reachable 9s for part 1
   (map length (map list-distinct (map (reachable-9s arr) trailheads))))))

;; part 2
(define (trailhead-ratings file)
 (let* ((arr (make-grid file))
        (trailheads (find-trailheads arr)))
  (list-sum (map length (map (reachable-9s arr) trailheads)))))

;; common to both parts
(define*
 ((reachable-9s arr) pos)
 (define (helper pos)
  (let* ((x (first pos))
         (y (second pos))
         (height (parse-int (apply array-ref (cons arr pos))))
         (neighbours (list (list (1- x) y)
                           (list (1+ x) y)
                           (list x (1- y))
                           (list x (1+ y))))
         (valid-neighbours (filter
                            (lambda (p)
                             (and
                              (apply array-in-bounds? (cons arr p))
                              (equal?
                               (1+ height)
                               (parse-int (apply array-ref (cons arr p))))))
                            neighbours)))
   (if (equal? height 9)
    (list pos)
    (append-map helper valid-neighbours))))

 (helper pos))

(define (find-trailheads arr)
 (filter (lambda (pos)
          (equal? #\0 (apply array-ref (cons arr pos))))
         (array-indexes arr)))

(define (array-indexes arr)
 (let ((size (array-length arr)))
  (combinations-2 (iota size) (iota size))))
