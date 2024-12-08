(define-module
 (day07)
 #:export
 (count-possible-equations is-true-equation? is-true-equation-with-concat?))

(use-modules (util input)
             (ice-9 regex)
             (ice-9 curried-definitions)
             (srfi srfi-1))

;; common to both parts
(define ((count-possible-equations pred) file)
 (apply +
        (filter-map (lambda (ns)
                     (if (pred (car ns) (reverse (cdr ns)))
                      (car ns)
                      #f))
                    (parse-input file))))

;; part 1
(define (is-true-equation? answer numbers)
 (let ((n (car numbers))
       (rest (cdr numbers)))
  (if (null? rest)
   (equal? n answer)
   (or (is-true-equation? (/ answer n) rest)
       (is-true-equation? (- answer n) rest)))))

;; part 2
(define (is-true-equation-with-concat? answer numbers)
 (let ((n (car numbers))
       (rest (cdr numbers)))
  (cond
   ;; unconcat resulted in a non-number, so returned #f
   ((not answer)
    #f)
   ;; answer is a fraction, not an integer, and so exit early
   ((not (integer? answer))
    #f)
   ((null? rest)
    (equal? n answer))
   (else
    (or (is-true-equation-with-concat? (/ answer n) rest)
        (is-true-equation-with-concat? (- answer n) rest)
        (is-true-equation-with-concat? (unconcat answer n) rest))))))

(define (unconcat n1 n2)
 (let ((s1 (number->string n1))
       (s2 (number->string n2)))
  (if (and (string-suffix? s2 s1)
           (integer? n1))
   (string->number (string-drop-right s1 (string-length s2)))
   #f)))

;; input parsing
(define (parse-input file)
 (map (lambda (line)
       (strings->numbers (map match:substring (list-matches "[0-9]+" line))))
      (lines file)))
