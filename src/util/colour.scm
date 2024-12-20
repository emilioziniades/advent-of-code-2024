;; print colour to the terminal using ANSI escape sequences
(define-module (util colour) #:export (display-colour grey red green))

(define (ansi-escape-sequence n)
 (let ((escape (string #\escape)))
  (string-append escape "[" (number->string n) "m")))

(define reset (ansi-escape-sequence 0))
(define grey (ansi-escape-sequence 30))
(define red (ansi-escape-sequence 31))
(define green (ansi-escape-sequence 32))
(define (display-colour colour str)
 (display (string-append colour str reset)))
