(define-module (day22) #:export (sum-secrets))

(use-modules (util input))

; part 1
(define (sum-secrets file)
 (let ((secrets (map string->number (lines file))))
  (apply +
         (map (lambda (s)
               (nth-next-secret s 2000))
              secrets))))

(define (next-secret s)
 (let* ((s1 (modulo (logxor s (* s 64)) 16777216))
        (s2 (modulo (logxor s1 (floor-quotient s1 32)) 16777216))
        (s3 (modulo (logxor s2 (* s2 2048)) 16777216)))
  s3))

(define (nth-next-secret s n)
 (if (= n 0)
  s
  (nth-next-secret (next-secret s) (1- n))))
