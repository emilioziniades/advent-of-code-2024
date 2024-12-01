(define-module (day01) #: export (total-distance))

(use-modules (io) (ice-9 textual-ports) (srfi srfi-1) (srfi srfi-11))

;; input parsing

(define (split-spaces s) (string-split s #\space))

(define (drop-null-strings l) (filter (compose not string-null?) l))

(define (strings->numbers l) (map string->number l))

(define (lines s) (string-split s #\newline))


(define (parse-input file) 
  (unzip2 
    (map strings->numbers 
         (filter (compose not null?) 
                 (map drop-null-strings 
                      (map split-spaces 
                           (lines file)))))))

;; part 1

(define (distance lst) (abs (- (first lst) (second lst))))

(define (list-sum lst) (fold + 0 lst))

(define (total-distance file) 
  (let-values 
    (((left-list right-list) (parse-input file))) 
    (list-sum (map distance 
         (zip 
           (sort-list left-list <)
           (sort-list right-list <))))))
