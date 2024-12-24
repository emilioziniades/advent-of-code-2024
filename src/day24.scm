(define-module (day24) #:export (get-z-wire-number))

(use-modules (util io)
             (util input)
             (srfi srfi-1)
             (srfi srfi-11)
             (ice-9 hash-table))

; part 1
(define (get-z-wire-number file)
 (let*-values (((wires gates) (parse-input file))
               ((processed-wires) (process-gates wires gates))
               ((z-wires) (sort (filter (lambda (key)
                                         (string-prefix? "z" key))
                                        (hash-map-keys processed-wires))
                                string<))
               ((z-bits) (reverse (map (lambda (z-wire)
                                        (hash-ref processed-wires z-wire))
                                       z-wires))))
              (bit-list->number z-bits)))

(define (process-gates wires gates)
 ; one pass through all gates
 (define (helper gs)
  (if (null? gs)
   '()
   (let* ((g (car gs))
          (src1 (first g))
          (src2 (third g))
          (op (second g))
          (dst (fifth g))
          (src1-value (hash-ref wires src1))
          (src2-value (hash-ref wires src2)))
    (if (and src1-value
             src2-value)
     (begin
      (hash-set! wires dst (evaluate-gate src1-value src2-value op))
      (helper (cdr gs)))
     (cons (car gs) (helper (cdr gs)))))))

 ; multiple passes through all gates until processed all
 (define (helper2 gs)
  (let ((next-gs (helper gs)))
   (if (null? next-gs)
    wires
    (helper2 next-gs))))

 (helper2 gates))

(define (evaluate-gate src1 src2 op)
 (cond
  ((equal? op "AND")
   (logand src1 src2))
  ((equal? op "OR")
   (logior src1 src2))
  ((equal? op "XOR")
   (logxor src1 src2))
  (else
   (raise-exception "unrecognized op"))))

(define (bit-list->number lst)
 (if (null? lst)
  0
  (+ (* (car lst) (expt 2 (1- (length lst)))) (bit-list->number (cdr lst)))))

; input parsing
(define (parse-input file)
 (let* ((file-lines (lines file))
        (blank-line-index (list-index (lambda (line)
                                       (equal? line ""))
                                      file-lines))
        (wires-raw (take file-lines blank-line-index))
        (wires (alist->hash-table
                (map (lambda (line)
                      (cons (car line) (string->number (cadr line))))
                     (map (lambda (line)
                           (string-split (string-delete #\: line) #\space))
                          wires-raw))))
        (gates-raw (cdr (drop file-lines blank-line-index)))
        (gates (map (lambda (line)
                     (string-split line #\space))
                    gates-raw)))
  (values wires gates)))

; workspace 
(parse-input (read-file "example/day24.txt"))
