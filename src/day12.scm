(define-module (day12) #:export (price-fences price-fences-discount))

(use-modules (util input) (srfi srfi-1) (ice-9 hash-table))

;; part 1
(define (price-fences file)
 (list-sum (map price-fence (find-regions (make-grid file)))))

(define (find-regions arr)
 (define seen (make-hash-table))
 ;; keep exploring neighbours, building regions (lists of positions), and updating `seen` until there are none left
 (define (floodfill pos)
  (if (hash-ref seen pos #f)
   '()
   (begin
    (hash-set! seen pos #t)
    (let ((valid-neighbours (filter
                             (lambda (next-pos)
                              (and
                               (array-in-bounds-nd? arr next-pos)
                               (equal? (array-ref-nd arr pos)
                                       (array-ref-nd arr next-pos))
                               (not (hash-ref seen next-pos #f))))
                             (neighbours (first pos) (second pos)))))
     (cons pos
           (append-map (lambda (next-pos)
                        (floodfill next-pos))
                       valid-neighbours))))))

 (let* ((size (array-length arr))
        (points (combinations-2 (iota size) (iota size))))
  (filter-map (lambda (pos)
               (if (hash-ref seen pos #f)
                #f
                (floodfill pos)))
              points)))

(define (price-fence lst)
 (define area (length lst))
 (define (perimeter lst)
  (let* ((hashmap (alist->hash-table (map (lambda (pos)
                                           (cons pos #t))
                                          lst)))
         (shared-sides (length
                        (append-map (lambda (pos)
                                     (filter (lambda (neighbour)
                                              (hash-ref hashmap neighbour #f))
                                             (apply neighbours pos)))
                                    lst))))
   (- (* 4 area) shared-sides)))

 (* area (perimeter lst)))

; part 2
(define (price-fences-discount file)
 (list-sum (map (lambda (x)
                 (* (length x) (count-sides x)))
                (find-regions (make-grid file)))))

; Count the top and bottom edges for each row, and count the left and
; right edges for each column. Using top edges as an example, this is
; how I counted edges:
; In a row, you can count the top edges by counting the number of "side-groups".
; "side-groups" are a sequence of blocks with nothing above it.
(define (count-sides lst)
 ; data
 (define xs (map cadr lst))
 (define ys (map car lst))
 (define min-x (apply min xs))
 (define min-y (apply min ys))
 (define max-x (apply max xs))
 (define max-y (apply max ys))
 (define all-xs (iota (1+ (- max-x min-x)) min-x))
 (define all-ys (iota (1+ (- max-y min-y)) min-y))
 (define hashset (list->hash-set lst))
 ; helper functions
 (define (count-sides-row y)
  (+ (count-true-groups (map (lambda (x)
                              (has-edge 'up x y))
                             all-xs))
     (count-true-groups (map (lambda (x)
                              (has-edge 'down x y))
                             all-xs))))

 (define (count-sides-column x)
  (+ (count-true-groups (map (lambda (y)
                              (has-edge 'left x y))
                             all-ys))
     (count-true-groups (map (lambda (y)
                              (has-edge 'right x y))
                             all-ys))))

 (define (has-edge direction x y)
  (let ((pos (list y x))
        (other-pos (case direction
                    ((up)
                     (list (1- y) x))
                    ((down)
                     (list (1+ y) x))
                    ((left)
                     (list y (1- x)))
                    ((right)
                     (list y (1+ x)))
                    (else
                     (raise-exception "unexpected direction")))))
   (and (hash-ref hashset pos)
        (not (hash-ref hashset other-pos)))))

 (+ (list-sum (map count-sides-row all-ys))
    (list-sum (map count-sides-column all-xs))))

(define (count-true-groups lst)
 (define (helper l)
  (cond
   ((null? l)
    '())
   ((not (car l))
    (helper (cdr l)))
   (else
    (cons (take-while identity l) (helper (drop-while identity l))))))

 (length (helper lst)))
