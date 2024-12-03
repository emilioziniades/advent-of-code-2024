(add-to-load-path "src")

(use-modules
  (ice-9 getopt-long)
  (ice-9 pretty-print)
  (srfi srfi-1)
  (srfi srfi-64)
  (util fetch)
  (util io)
  (day01)
  (day02)
  (day03))

;; get optional tests filter from command line

(define option-spec '((filter (value #t))))
(define options (getopt-long (command-line) option-spec))
(define filter (option-ref options 'filter #f))

;; setup

(unless (file-exists? "input") (mkdir "input"))

;; helper functions

(define* (run-test name filename test-fn expected #:key fetch-day) 
         (when fetch-day (fetch-input fetch-day))
         (define data (read-file filename))
         (define actual (test-fn data))
         (test-equal name expected actual))

;; run the tests

(test-begin "advent-of-code")

(when filter
  (test-skip (lambda (runner)
               (not (string-prefix? filter (test-runner-test-name runner))))))

(run-test "day01-part1-example" "example/day01.txt" total-distance 11)
(run-test "day01-part1-input" "input/day01.txt" total-distance 2192892 #:fetch-day 1)
(run-test "day01-part2-example" "example/day01.txt" similarity-score 31)
(run-test "day01-part2-input" "input/day01.txt" similarity-score 22962826 #:fetch-day 1)

(run-test "day02-part1-example" "example/day02.txt" count-safe-reports 2)
(run-test "day02-part1-input" "input/day02.txt" count-safe-reports 356 #:fetch-day 2)
(run-test "day02-part2-example" "example/day02.txt" count-safe-reports-with-dampener 4)
(run-test "day02-part2-input" "input/day02.txt" count-safe-reports-with-dampener 413 #:fetch-day 2)

(run-test "day03-part1-example" "example/day03.txt" execute-muls 161)
(run-test "day03-part1-input" "input/day03.txt" execute-muls 184511516 #:fetch-day 3)
(run-test "day03-part2-example" "example/day03-part2.txt" execute-muls-with-donts 48)
(run-test "day03-part2-input" "input/day03.txt" execute-muls-with-donts 90044227 #:fetch-day 3)

(define n-failures (test-runner-fail-count (test-runner-current)))

(test-end "advent-of-code")

(if (> n-failures 0) (exit 1) (exit 0))
