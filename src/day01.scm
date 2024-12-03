(define-module (day01) #:export (total-distance similarity-score))
(use-modules (util input)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-13))
;; input parsing
(define (parse-input file)
 (unzip2
  (map strings->numbers
       (map drop-null-strings (map split-spaces (lines file))))))

;; part 1
(define (distance lst)
 (abs (- (first lst) (second lst))))

(define (list-sum lst)
 (fold + 0 lst))

(define (total-distance file)
 (let-values
  (((left-list right-list) (parse-input file)))
  (list-sum
   (map distance (zip (sort-list left-list <) (sort-list right-list <))))))

;; part 2
(define (counter lst)
 (define h (make-hash-table))
 (map (lambda (key)
       (let ((value (hashq-ref h key)))
        (if value
         (hashq-set! h key (+ 1 value))
         (hashq-set! h key 1))))
      lst)
 h)

(define (similarity-score file)
 (let*-values
  (((left-list right-list) (parse-input file)) ((count) (counter right-list)))
  (list-sum (map (lambda (n)
                  (* n (hashq-ref count n 0)))
                 left-list))))
