(add-to-load-path "src")

(use-modules (srfi srfi-1) (srfi srfi-64) (fetch) (io) (day01))

;; (define (print text) (display text) (display "\n"))
;;
;; (define (contains? value my-list) (any (lambda (x) (equal? value x)) my-list))
;;
;; (define args (command-line))
;;
;; (define filter-string (if (and (eq? (length args) 3) (contains? "--filter" args)) (last args) #f))
;;
;; (define tests '(((name . day-1) ()) ((name . day-1))))
;;
;; (print args)
;; (print filter-string)

; TODO: somehow return non-zero exit code if a test fails

(define* (run-test day filename fn expected #:key do-fetch) 
         (when do-fetch (fetch-input day))
         (define data (read-file filename))
         (define actual (fn data))
         (test-equal expected actual))


(test-begin "tests")

(run-test 1 "example/day01.txt" total-distance 11)
(run-test 1 "input/day01.txt" total-distance 2192892 #:do-fetch #t)

(test-end "tests")

