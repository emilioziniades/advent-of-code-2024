;; some functions useful for input parsing
(define-module (util input)
               #:export
               (split-spaces string-split-on
                             drop-null-strings
                             strings->numbers
                             lines
                             list-sum
                             make-grid))

(use-modules (ice-9 textual-ports)
             (ice-9 curried-definitions)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-13))

(define (drop-null-strings lst)
 (filter (compose not string-null?) lst))

(define (lines str)
 (string-split (string-trim-both str #\newline) #\newline))

(define ((string-split-on sep) str)
 (string-split str sep))

(define (split-spaces str)
 ((string-split-on #\space) str))

(define (strings->numbers lst)
 (map string->number lst))

(define (list-sum lst)
 (fold + 0 lst))

(define (make-grid str)
 (list->array 2 (map string->list (lines str))))
