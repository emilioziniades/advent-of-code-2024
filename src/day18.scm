(define-module (day18) #:export (find-shortest-path))

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
 (hash-ref (djikstra '(0 0) end size bytes) end))

(define (djikstra start end size bytes)
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
    (apply helper
           (fold (lambda (next prev)
                  (let ((f (first prev))
                        (c (second prev))
                        (new-cost (1+ (hash-ref cost-so-far (car current))))
                        (next-cost (hash-ref cost-so-far next)))
                   (if (or (not next-cost)
                           (< new-cost next-cost))
                    (list (push f next new-cost)
                          (begin
                           (hash-set! c next new-cost)
                           (values c)))
                    prev)))
                 (list rest cost-so-far)
                 (get-neighbours (car current)))))))

 (helper frontier cost-so-far))

;; input parsing
(define (parse-input file)
 (map (lambda (line)
       (strings->numbers (string-split line #\,)))
      (lines file)))

;; utility functions
(define (list->hash-set lst)
 (alist->hash-table (map (lambda (x)
                          (cons x #t))
                         lst)))
