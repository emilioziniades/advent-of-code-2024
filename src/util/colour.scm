;; print colour to the terminal using ANSI escape sequences
(define-module (util colour) #: export (display-colour grey red green))

(define (ansi-escape-sequence n)
  (let ((escape (string #\x1b)))
    (string-append escape "[" (number->string n) "m")))

(define reset (ansi-escape-sequence 0))
(define grey (ansi-escape-sequence 30))
(define red (ansi-escape-sequence 31))
(define green (ansi-escape-sequence 32))

(define (display-colour color text)
  (display (string-append color text reset)))

;; (display-colour red "This is red text.")
;; (newline)
;; (display "and it is reset")
;; (newline)
;; (display-colour green "This is green text.")
;; (newline)
;; (display "and it is reset")
;; (newline)
;; (display-colour grey "This is grey text.")
;; (newline)
;; (display "and it is reset")

;; (map (lambda (n) (display-colour (ansi-escape-sequence n) "This is some text") (newline)) (iota 49))
