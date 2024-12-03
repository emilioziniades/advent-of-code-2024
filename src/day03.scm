(define-module (day03) #:export (execute-muls execute-muls-with-donts))

(use-modules (util io) (ice-9 regex) (srfi srfi-9 gnu))

;; part 1
(define (execute-muls file)
 (let ((pattern "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"))
  (fold-matches pattern
                file
                0
                (lambda (match count)
                 (+ count (do-mul match))))))

;; part 2
(define-immutable-record-type <output>
                              (output n do-mul)
                              output?
                              (n output-n set-output-n)
                              (do-mul output-do-mul set-output-do-mul))

(define (execute-muls-with-donts file)
 (let ((pattern "do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"))
  (output-n (fold-matches pattern file (output 0 #t) update-output))))

(define (update-output match output)
 (cond
  ((equal? "do()" (match:substring match))
   (set-output-do-mul output #t))
  ((equal? "don't()" (match:substring match))
   (set-output-do-mul output #f))
  ((string-prefix? "mul" (match:substring match))
   (if (output-do-mul output)
    (set-output-n output (+ (output-n output) (do-mul match)))
    output))))

;; common to both parts
(define (do-mul match)
 (* (string->number (match:substring match 1))
    (string->number (match:substring match 2))))
