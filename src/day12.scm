(define-module (day12) #:export (price-fences))

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

;; utility functions
;;
;; assumes hashmap has list values
(define (hash-append! hashmap key value)
 (hash-set! hashmap key (cons value (hash-ref hashmap key '()))))

(define (array-ref-nd arr lst)
 (apply array-ref (cons arr lst)))

(define (array-in-bounds-nd? arr lst)
 (apply array-in-bounds? (cons arr lst)))
