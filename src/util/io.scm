;; module for high-level filesystem io
(define-module (util io) #:export (read-file write-file))

(use-modules (ice-9 textual-ports))

(define (read-file filename)
 (call-with-input-file filename get-string-all))

(define (write-file filename text)
 (with-output-to-file filename
                      (lambda ()
                       (display text))))
