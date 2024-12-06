(add-to-load-path "src")
(add-to-load-path "tests")
(use-modules (srfi srfi-64)
             (util fetch)
             (util io)
             (test-runner)
             (day01)
             (day02)
             (day03)
             (day04)
             (day05))

(define* (run-test test-name filename test-fn expected #:key fetch-day)
         (when fetch-day
          (fetch-input fetch-day))
         (define data (read-file filename))
         (define actual (test-fn data))
         (test-equal test-name expected actual))

;; SETUP
(init-test-runner)
(unless (file-exists? "input")
 (mkdir "input"))
;; TESTS
(test-begin "advent-of-code")
;; day 1
(run-test "day01-part1-example" "example/day01.txt" total-distance 11)
(run-test "day01-part1-input"
          "input/day01.txt"
          total-distance
          2192892
          #:fetch-day
          1)
(run-test "day01-part2-example" "example/day01.txt" similarity-score 31)
(run-test "day01-part2-input"
          "input/day01.txt"
          similarity-score
          22962826
          #:fetch-day
          1)
;; day 2
(run-test "day02-part1-example" "example/day02.txt" count-safe-reports 2)
(run-test "day02-part1-input"
          "input/day02.txt"
          count-safe-reports
          356
          #:fetch-day
          2)
(run-test "day02-part2-example"
          "example/day02.txt"
          count-safe-reports-with-dampener
          4)
(run-test "day02-part2-input"
          "input/day02.txt"
          count-safe-reports-with-dampener
          413
          #:fetch-day
          2)
;; day 3
(run-test "day03-part1-example" "example/day03.txt" execute-muls 161)
(run-test "day03-part1-input"
          "input/day03.txt"
          execute-muls
          184511516
          #:fetch-day
          3)
(run-test "day03-part2-example"
          "example/day03-part2.txt"
          execute-muls-with-donts
          48)
(run-test "day03-part2-input"
          "input/day03.txt"
          execute-muls-with-donts
          90044227
          #:fetch-day
          3)
;; day 4
(run-test "day04-part1-example" "example/day04.txt" count-all-xmas 18)
(run-test "day04-part1-input"
          "input/day04.txt"
          count-all-xmas
          2603
          #:fetch-day
          4)
(run-test "day04-part2-example" "example/day04.txt" count-all-x-mas 9)
(run-test "day04-part2-input"
          "input/day04.txt"
          count-all-x-mas
          1965
          #:fetch-day
          4)
;; day 5
(run-test "day05-part1-example" "example/day05.txt" count-correct-updates 143)
(run-test "day05-part1-input"
          "input/day05.txt"
          count-correct-updates
          5639
          #:fetch-day
          5)
(run-test "day05-part2-example"
          "example/day05.txt"
          count-incorrect-updates
          123)
(run-test "day05-part2-input"
          "input/day05.txt"
          count-incorrect-updates
          5273
          #:fetch-day
          5)
(test-end "advent-of-code")
