(define-module (day07) #:export (count-possible-equations))

(use-modules (util io) (util input) (ice-9 regex) (srfi srfi-1))

;; part 1
(define (count-possible-equations file)
 (apply +
        (filter-map (lambda (ns)
                     (if (is-true-equation? (car ns) (reverse (cdr ns)))
                      (car ns)
                      #f))
                    (parse-input file))))

(define (is-true-equation? answer numbers)
 (let ((n (car numbers))
       (rest (cdr numbers)))
  (if (null? rest)
   (equal? n answer)
   (or (is-true-equation? (/ answer n) rest)
       (is-true-equation? (- answer n) rest)))))

;; input parsing
(define (parse-input file)
 (map (lambda (line)
       (strings->numbers (map match:substring (list-matches "[0-9]+" line))))
      (lines file)))
