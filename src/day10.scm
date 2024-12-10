(define-module (day10) #:export (trailhead-scores trailhead-ratings))

(use-modules (util io) (util input) (srfi srfi-1))

;; part 1
(define (trailhead-scores file)
 (let* ((arr (make-grid file))
        (trailheads (find-trailheads arr)))
  (list-sum (map (lambda (th)
                  (trailhead-score arr th))
                 trailheads))))

(define (trailhead-score arr pos)
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

 (length (list-distinct (helper pos))))

;; part 2
(define (trailhead-ratings file)
 (let* ((arr (make-grid file))
        (trailheads (find-trailheads arr)))
  (list-sum (map (lambda (th)
                  (trailhead-rating arr th))
                 trailheads))))

(define (trailhead-rating arr pos)
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
    1
    (list-sum (map helper valid-neighbours)))))

 (helper pos))

;; common to both parts
(define (find-trailheads arr)
 (filter (lambda (pos)
          (equal? #\0 (apply array-ref (cons arr pos))))
         (array-indexes arr)))

(define (array-indexes arr)
 (let ((size (array-length arr)))
  (combinations-2 (iota size) (iota size))))
