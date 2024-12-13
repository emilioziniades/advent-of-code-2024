(define-module (day13) #:export (fewest-tokens))

(use-modules (util io) (util input) (srfi srfi-1) (ice-9 string-fun))

;; part 1
;; Each claw machine can be viewed as a pair of linear equations. Example:
;; Button A: X+94, Y+34
;; Button B: X+22, Y+67
;; Prize: X=8400, Y=5400
;; We are trying to find the number of presses for a and b that result in the
;; correct change in x and y position of the claw machine. The question speaks about 
;; finding the smallest solution, but there can either be 1 or 0 solutions, assuming
;; the problem can be expressed as a system of linear equations. So, to do that,
;; let a = presses of button a, and b = presses of button b, we are trying to find (a, b)
;; such that both equations are true:
;; 94a + 22b = 8400
;; 34a + 67b = 5400
(define (fewest-tokens file)
 (apply
  +
  (map
   (lambda (lst)
    (+ (* 3 (first lst)) (second lst)))
   (filter
    (lambda (lst)
     (and lst
          (every integer? lst)))
    (map
     (lambda (lst)
      (let ((a-x (first lst))
            (a-y (second lst))
            (b-x (third lst))
            (b-y (fourth lst))
            (prize-x (fifth lst))
            (prize-y (sixth lst)))
       (solve-linear-equation-pair a-x b-x (- prize-x) a-y b-y (- prize-y))))
     (parse-input file))))))

;; Use the cross-multiplication method to find x and y for the following two equations:
;; a1*x + b1*y + c1 = 0
;; a2*x + b2*y + c2 = 0
;; Returns false if there are no possible solutions (because the denominator is 0)
(define (solve-linear-equation-pair a1 b1 c1 a2 b2 c2)
 (define denominator (- (* b2 a1) (* b1 a2)))
 (if (= denominator 0)
  #f
  (list (/ (- (* b1 c2) (* b2 c1)) denominator)
        (/ (- (* c1 a2) (* c2 a1)) denominator))))

;; input parsing
(define (parse-input file)
 (map
  (lambda (x)
   (filter-map
    string->number
    (string-split (string-filter (string->char-set "0123456789 ") x) #\space)))
  (string-split (string-replace-substring file "\n\n" "|") #\|)))
