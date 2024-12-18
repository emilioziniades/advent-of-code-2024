(define-module (day17) #:export (get-program-output make-quine-program))

(use-modules (util io) (srfi srfi-1) (srfi srfi-9 gnu) (ice-9 string-fun))

;; part 1
(define-immutable-record-type
 <computer>
 (make-computer program pointer a-register b-register c-register output)
 computer?
 (program get-program)
 (pointer get-pointer set-pointer)
 (a-register get-a-register set-a-register)
 (b-register get-b-register set-b-register)
 (c-register get-c-register set-c-register)
 (output get-output set-output))

(define (get-program-output file)
 (string-join
  (map number->string (reverse (get-output (execute (parse-input file)))))
  ","))

(define (execute computer)
 (if (> (get-pointer computer) (1- (length (get-program computer))))
  computer
  (execute (step computer))))

(define (step computer)
 (case (get-opcode computer)
  ((0)
   (adv computer))
  ((1)
   (bxl computer))
  ((2)
   (bst computer))
  ((3)
   (jnz computer))
  ((4)
   (bxc computer))
  ((5)
   (out computer))
  ((6)
   (bdv computer))
  ((7)
   (cdv computer))))

;; part 2
;; TODO: obviously going from 0 to infinity and checking every program isn't
;; going to work. You will have to find some optimizations here. Like searching
;; through only a selected number of possible options. Some things that stand out
;; about this problem: there is lots of modulo 8 happening, maybe every eighth has 
;; some common behaviour? Look at the output for the first few 100 values 
;; and see if there are some patterns. Another idea is to try the computation in reverse.
(define (make-quine-program file)
 (define (helper computer n)
  (display n)
  (newline)
  (if (is-quine-program (set-a-register computer n))
   n
   (helper computer (1+ n))))

 (helper (parse-input file) 0))

(define (is-quine-program computer)
 (let* ((output (get-output computer))
        (program (get-program computer))
        (len-program (length program))
        (len-output (length output))
        (rev-output (reverse output))
        (pointer (get-pointer computer)))
  (cond
   ((> pointer (1- len-program))
    (equal? program rev-output))
   ((> len-output len-program)
    #f)
   ((not (list-starts-with? program rev-output))
    #f)
   ((equal? program rev-output)
    #t)
   (else
    (is-quine-program (step computer))))))

;; does lst1 start with lst2?
(define (list-starts-with? lst1 lst2)
 (equal? (take lst1 (length lst2)) lst2))

;; opcodes
(define (adv computer)
 (dv computer set-a-register))

(define (bdv computer)
 (dv computer set-b-register))

(define (cdv computer)
 (dv computer set-c-register))

(define (dv computer register-setter)
 (let ((numerator (get-a-register computer))
       (denominator (expt 2 (get-combo-operand computer))))
  (step-pointer
   (register-setter computer (floor-quotient numerator denominator)))))

(define (bxl computer)
 (step-pointer
  (set-b-register computer
                  (logxor (get-b-register computer) (get-operand computer)))))

(define (bst computer)
 (step-pointer
  (set-b-register computer (modulo (get-combo-operand computer) 8))))

(define (bxc computer)
 (step-pointer
  (set-b-register computer
                  (logxor (get-b-register computer) (get-c-register computer)))))

(define (jnz computer)
 (if (= 0 (get-a-register computer))
  (step-pointer computer)
  (set-pointer computer (get-operand computer))))

(define (out computer)
 (step-pointer (append-output computer (modulo (get-combo-operand computer) 8))))

;; utility functions
(define (get-opcode computer)
 (list-ref (get-program computer) (get-pointer computer)))

(define (get-operand computer)
 (list-ref (get-program computer) (1+ (get-pointer computer))))

(define (step-pointer computer)
 (set-pointer computer (+ 2 (get-pointer computer))))

(define (append-output computer n)
 (set-output computer (cons n (get-output computer))))

(define (get-combo-operand computer)
 (case (get-operand computer)
  ((0)
   0)
  ((1)
   1)
  ((2)
   2)
  ((3)
   3)
  ((4)
   (get-a-register computer))
  ((5)
   (get-b-register computer))
  ((6)
   (get-c-register computer))
  ((7)
   (raise-exception "7 should never be a combo operand"))
  (else
   (raise-exception "invalid combo operand"))))

;; input parsing
(define (parse-input file)
 (let ((ns (filter-map
            string->number
            (string-split
             (string-filter (string->char-set "0123456789 ")
                            (string-replace-substring file "," " "))
             #\space))))
  (make-computer (cdddr ns) 0 (first ns) (second ns) (third ns) '())))
