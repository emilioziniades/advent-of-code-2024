(define-module (day02)
               #:export
               (count-safe-reports count-safe-reports-with-dampener))

(use-modules (util input) (srfi srfi-1) (srfi srfi-11))

;; input parsing
(define (parse-input file)
 (map strings->numbers (map split-spaces (lines file))))

;; part 1
(define (chunks-2 lst)
 (let ((chunk (take lst 2))
       (rest (drop lst 1)))
  (if (<= (length lst) 2)
   `(,chunk)
   (cons chunk (chunks-2 rest)))))

(define (count-safe-reports file)
 (length (filter safe? (parse-input file))))

(define (negative? n)
 (< n 0))

(define (positive? n)
 (> n 0))

(define (safe? lst)
 (let* ((chunks (chunks-2 lst))
        (differences (map (lambda (lst)
                           (- (second lst) (first lst)))
                          chunks)))
  (and (or (every positive? differences)
           (every negative? differences))
       (every (lambda (n)
               (<= (abs n) 3))
              differences))))

;; part 2
(define (count-safe-reports-with-dampener file)
 (length (filter safe-with-dampener? (parse-input file))))

(define (list-remove lst n)
 (append (take lst n) (drop lst (+ 1 n))))

(define (safe-with-dampener? lst)
 (let ((lsts (cons lst
                   (map (lambda (n)
                         (list-remove lst n))
                        (iota (length lst))))))
  (any safe? lsts)))
