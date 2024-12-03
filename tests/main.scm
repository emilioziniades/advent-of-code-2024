(add-to-load-path "src")

(use-modules
  (ice-9 getopt-long)
  (ice-9 format)
  (srfi srfi-1)
  (srfi srfi-64)
  (util fetch)
  (util io)
  (util colour)
  (day01)
  (day02)
  (day03))

;; get optional tests filter from command line

(define option-spec '((filter (value #t))))
(define options (getopt-long (command-line) option-spec))
(define filter (option-ref options 'filter #f))

;; setup directory for test data

(unless (file-exists? "input") (mkdir "input"))

;; helper functions

(define* (run-test name filename test-fn expected #:key fetch-day) 
         (when fetch-day (fetch-input fetch-day))
         (define data (read-file filename))
         (define actual (test-fn data))
         (test-equal name expected actual))

(define (print-test-result runner)
  (let ((test-name (test-result-ref runner 'test-name #f))
        (actual-value (object->string (test-result-ref runner 'actual-value #f)))
        (expected-value (object->string (test-result-ref runner 'expected-value #f)))
        (result-kind (symbol->string (test-result-ref runner 'result-kind #f))))
    (begin
      (cond ((equal? result-kind "skip")
             (display-colour grey (string-append test-name ": " result-kind)))
            ((equal? result-kind "pass") (display-colour green (string-append test-name ": " result-kind)))
            (#t (display-colour red (string-append test-name ": " result-kind ". expected: " expected-value ", actual: " actual-value))))
      (newline))))

(define (print-group-begin runner suite-name count)
  (begin
    (newline)
    (display "START GROUP ")
    (display suite-name)
    (newline)
    (newline)))

(define (test-on-final runner) 
  (begin
    (newline)
    (display "pass: ")
    (display (test-runner-pass-count runner))
    (newline)
    (display "fail: ")
    (display (test-runner-fail-count runner))
    (newline)
    (display "skip: ")
    (display (test-runner-skip-count runner))
    (newline))
  (if (> (test-runner-fail-count runner) 0) (exit 1) (exit 0)))

;; setup test runner

(define runner (test-runner-null))
(test-runner-current runner)

(test-runner-on-group-begin! runner print-group-begin)
(test-runner-on-test-end! runner print-test-result)
(test-runner-on-final! runner test-on-final)

(when filter
  (test-skip (lambda (runner)
               (not (string-prefix? filter (test-runner-test-name runner))))))

;; run the tests

(test-begin "advent-of-code")

(run-test "day01-part1-example" "example/day01.txt" total-distance 1100)
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

(test-end "advent-of-code")
