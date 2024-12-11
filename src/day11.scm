(define-module (day11) #:export (count-stones))

(use-modules (util input))

;; part 1
(define (count-stones file)
 (length
  (blink-n (strings->numbers (split-spaces (string-trim-both file #\newline)))
           25)))

(define (blink lst)
 (if (null? lst)
  '()
  (let ((head (car lst))
        (tail (cdr lst)))
   (cond
    ((equal? head 0)
     (cons 1 (blink tail)))
    ((even-digits? head)
     (append (split-digits head) (blink tail)))
    (else
     (cons (* head 2024) (blink tail)))))))

(define (blink-n lst n)
 (if (equal? 0 n)
  lst
  (blink-n (blink lst) (1- n))))

(define (even-digits? n)
 (equal? 0 (remainder (string-length (number->string n)) 2)))

(define (split-digits n)
 (let* ((digits (number->string n))
        (len (string-length digits))
        (midpoint (floor (/ (string-length digits) 2)))
        (n1 (string->number (substring digits 0 midpoint)))
        (n2 (string->number (substring digits midpoint))))
  (list n1 n2)))
