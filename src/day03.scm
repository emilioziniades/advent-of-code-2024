(define-module (day03) #: export (execute-multiplications))

(use-modules (io) (ice-9 regex))

;; part 1

(define pattern "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")

(define (execute-multiplications file) 
  (fold-matches pattern file 0
                (lambda (match count)
                  (+ count 
                     (* (string->number (match:substring match 1)) 
                        (string->number (match:substring match 2)))))))

