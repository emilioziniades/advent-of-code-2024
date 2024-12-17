(define-module (day11) #:export (count-stones))

(use-modules (util input) (ice-9 hash-table) (ice-9 curried-definitions))

;; part 1 and 2
(define ((count-stones n) file)
 (list-sum (hash-map-values (blink-stones-n (parse-input file) n))))

(define (blink-stones-n hashmap n)
 (if (equal? n 0)
  hashmap
  (blink-stones-n (blink-stones hashmap) (1- n))))

(define (blink-stones stones)
 (hash-fold (lambda (key value accumulator)
             (for-each (lambda (new-key)
                        (hash-update! accumulator
                                      new-key
                                      (lambda (old-value)
                                       (+ old-value value))
                                      0))
                       (blink-stone key))
             accumulator)
            (make-hash-table)
            stones))

(define (blink-stone n)
 (cond
  ((equal? n 0)
   '(1))
  ((even-digits? n)
   (split-digits n))
  (else
   (list (* n 2024)))))

;; utility functions
(define (even-digits? n)
 (equal? 0 (remainder (string-length (number->string n)) 2)))

(define (split-digits n)
 (let* ((digits (number->string n))
        (len (string-length digits))
        (midpoint (floor (/ (string-length digits) 2)))
        (n1 (string->number (substring digits 0 midpoint)))
        (n2 (string->number (substring digits midpoint))))
  (list n1 n2)))

(define (hash-update! hashmap key proc default)
 (let ((value (hash-ref hashmap key #f)))
  (if value
   (hash-set! hashmap key (proc value))
   (hash-set! hashmap key (proc default))))
 hashmap)

;; input parsing
(define (parse-input file)
 (alist->hash-table
  (map (lambda (x)
        (cons x 1))
       (strings->numbers (split-spaces (string-trim-both file #\newline))))))
