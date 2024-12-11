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
             (day05)
             (day06)
             (day07)
             (day08)
             (day09)
             (day10)
             (day11))

(define* (run-test test-name filename test-fn expected #:key fetch-day)
         (when fetch-day
          (fetch-input fetch-day))
         (test-equal test-name expected (test-fn (read-file filename))))

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
;; day 6
(run-test "day06-part1-example" "example/day06.txt" count-guard-positions 41)
(run-test "day06-part1-input"
          "input/day06.txt"
          count-guard-positions
          5551
          #:fetch-day
          6)
(run-test "day06-part2-example" "example/day06.txt" count-good-obstructions 6)
(run-test "day06-part2-input"
          "input/day06.txt"
          count-good-obstructions
          1939
          #:fetch-day
          6)
;; day 7
(run-test "day07-part1-example"
          "example/day07.txt"
          (count-possible-equations is-true-equation?)
          3749)
(run-test "day07-part1-input"
          "input/day07.txt"
          (count-possible-equations is-true-equation?)
          21572148763543
          #:fetch-day
          7)
(run-test "day07-part2-example"
          "example/day07.txt"
          (count-possible-equations is-true-equation-with-concat?)
          11387)
(run-test "day07-part2-input"
          "input/day07.txt"
          (count-possible-equations is-true-equation-with-concat?)
          581941094529163
          #:fetch-day
          7)
;; day 8
(run-test "day08-part1-example" "example/day08.txt" count-antinodes 14)
(run-test "day08-part1-input"
          "input/day08.txt"
          count-antinodes
          295
          #:fetch-day
          8)
(run-test "day08-part2-example"
          "example/day08.txt"
          count-harmonic-antinodes
          34)
(run-test "day08-part2-input"
          "input/day08.txt"
          count-harmonic-antinodes
          1034
          #:fetch-day
          8)
;; day 9
(run-test "day09-part1-example" "example/day09.txt" filesystem-checksum 1928)
(run-test "day09-part1-input"
          "input/day09.txt"
          filesystem-checksum
          6216544403458
          #:fetch-day
          9)
(run-test "day09-part2-example"
          "example/day09.txt"
          filesystem-checksum-block
          2858)
(run-test "day09-part2-input"
          "input/day09.txt"
          filesystem-checksum-block
          6237075041489
          #:fetch-day
          9)
;; day 10
(run-test "day10-part1-example" "example/day10.txt" trailhead-scores 36)
(run-test "day10-part1-input"
          "input/day10.txt"
          trailhead-scores
          825
          #:fetch-day
          10)
(run-test "day10-part2-example" "example/day10.txt" trailhead-ratings 81)
(run-test "day10-part2-input"
          "input/day10.txt"
          trailhead-ratings
          1805
          #:fetch-day
          10)
;; day 11
(run-test "day11-part1-example" "example/day11.txt" count-stones 55312)
(run-test "day11-part1-input"
          "input/day11.txt"
          count-stones
          188902
          #:fetch-day
          11)
(test-end "advent-of-code")
