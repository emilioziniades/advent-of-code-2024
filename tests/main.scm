(add-to-load-path "src")

(use-modules (srfi srfi-1) (srfi srfi-64) (fetch) (io) (day01) (day02))

;; TODO: figure out test filtering/skipping

;; (define (print text) (display text) (display "\n"))
;; (define (contains? value my-list) (any (lambda (x) (equal? value x)) my-list))
;; (define args (command-line))
;; (define filter-string (if (and (eq? (length args) 3) (contains? "--filter" args)) (last args) #f))
;; (define tests '(((name . day-1) ()) ((name . day-1))))
;; (print args)
;; (print filter-string)

(define* (run-test day filename test-fn expected #:key do-fetch) 
         (when do-fetch (fetch-input day))
         (define data (read-file filename))
         (define actual (test-fn data))
         (test-equal expected actual))

(test-begin "advent-of-code")

(run-test 1 "example/day01.txt" total-distance 11)
(run-test 1 "input/day01.txt" total-distance 2192892 #:do-fetch #t)
(run-test 1 "example/day01.txt" similarity-score 31)
(run-test 1 "input/day01.txt" similarity-score 22962826 #:do-fetch #t)

(run-test 2 "example/day02.txt" count-safe-reports 2)
(run-test 2 "input/day02.txt" count-safe-reports 356 #:do-fetch #t)

(define n-failures (test-runner-fail-count (test-runner-current)))

(test-end "advent-of-code")

(if (> n-failures 0) (exit 1) (exit 0))
