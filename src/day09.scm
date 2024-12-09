(define-module (day09) #:export (filesystem-checksum))

(use-modules (util io) (srfi srfi-1))

;; part 1
(define (filesystem-checksum file)
 (let* ((disk-map (parse-input file))
        (expanded-disk-map (expand-disk-map disk-map)))
  (calculate-checksum (compact-disk-map expanded-disk-map))))

(define (compact-disk-map arr)
 (define (recursive-compact-disk-map arr pointer1 pointer2)
  (cond
   ((>= pointer1 pointer2)
    arr)
   ((array-ref arr pointer1)
    (recursive-compact-disk-map arr (1+ pointer1) pointer2))
   ((not (array-ref arr pointer2))
    (recursive-compact-disk-map arr pointer1 (1- pointer2)))
   (else
    (begin
     (array-swap! arr pointer1 pointer2)
     (recursive-compact-disk-map arr (1+ pointer1) (1- pointer2))))))

 (recursive-compact-disk-map arr 0 (1- (array-length arr))))

(define (calculate-checksum arr)
 (apply +
        (map (lambda (i)
              (* i (array-ref arr i)))
             (iota (1+ (array-index-right arr identity))))))

;; utility functions
(define (expand-disk-map disk-map)
 (define (expand-files lst)
  (let ((arr (list->array 1 lst)))
   (map (lambda (i)
         (make-list (parse-int (array-ref arr i)) i))
        (iota (array-length arr)))))

 (define (expand-free-space lst)
  (map (lambda (n)
        (make-list (parse-int n) #f))
       lst))

 (let* ((files (even-items disk-map))
        (free-space (odd-items disk-map))
        (files-expanded (expand-files files))
        (free-space-expanded (expand-free-space free-space)))
  (list->array 1 (flatten (interleave files-expanded free-space-expanded)))))

(define (even-items arr)
 (map (lambda (i)
       (array-ref arr i))
      (iota (ceiling (/ (array-length arr) 2)) 0 2)))

(define (odd-items arr)
 (map (lambda (i)
       (array-ref arr i))
      (iota (floor (/ (array-length arr) 2)) 1 2)))

(define (parse-int char)
 (- (char->integer char) 48))

(define (interleave lst1 lst2)
 (if (null? lst1)
  lst2
  (cons (car lst1) (interleave lst2 (cdr lst1)))))

(define (array-index arr pred)
 (define (recursive-array-index arr idx)
  (if (pred (array-ref arr idx))
   idx
   (recursive-array-index arr (1+ idx))))

 (recursive-array-index arr 0))

(define (array-index-right arr pred)
 (define (recursive-array-index-right arr idx)
  (if (pred (array-ref arr idx))
   idx
   (recursive-array-index-right arr (1- idx))))

 (recursive-array-index-right arr (1- (array-length arr))))

(define (array-swap! arr idx1 idx2)
 (let ((n1 (array-ref arr idx1))
       (n2 (array-ref arr idx2)))
  (begin
   (array-set! arr n1 idx2)
   (array-set! arr n2 idx1))))

(define (flatten lst)
 (append-map identity lst))

;; input parsing
(define (parse-input file)
 (list->array 1 (string->list (string-trim-right file #\newline))))
