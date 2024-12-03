;; some functions useful for input parsing
(define-module (util input) #: export (split-spaces drop-null-strings strings->numbers lines))

(use-modules (ice-9 textual-ports) (srfi srfi-1) (srfi srfi-11) (srfi srfi-13))

(define (split-spaces str) (string-split str #\space))

(define (drop-null-strings lst) (filter (compose not string-null?) lst))

(define (strings->numbers lst) (map string->number lst))

(define (lines str) (string-split (string-trim-both str #\newline) #\newline))

