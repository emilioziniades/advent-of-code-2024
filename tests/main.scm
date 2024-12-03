(add-to-load-path "src")
(add-to-load-path "tests")

(use-modules
  (srfi srfi-64)
  (util fetch)
  (util io)
  (test-runner)
  (day01)
  (day02)
  (day03))

(define* (run-test test-name filename test-fn expected #:key fetch-day) 
         (when fetch-day (fetch-input fetch-day))
         (define data (read-file filename))
         (define actual (test-fn data))
         (test-equal test-name expected actual))

(init-test-runner)
(unless (file-exists? "input") (mkdir "input"))

(test-begin "advent-of-code")

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

(test-end "advent-of-code")
