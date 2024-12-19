(define-module (day19) #:export (count-possible-designs))

(use-modules (util input)
             (srfi srfi-1)
             (srfi srfi-11)
             (ice-9 string-fun)
             (ice-9 hash-table))

; part 1
(define (count-possible-designs file)
 (let-values (((patterns designs) (parse-input file)))
             (length (filter (lambda (design)
                              (is-possible? design patterns))
                             designs))))

(define (is-possible? design patterns)
 (or (null? design)
     (any (lambda (n)
           (and (hash-ref patterns (take design n))
                (is-possible? (drop design n) patterns)))
          (iota (length design) 1))))

; input parsing
(define (parse-input file)
 (let* ((file-lines (lines file))
        (raw-patterns (car file-lines))
        (raw-designs (cddr file-lines)))
  (values
   (list->hash-set
    (map string->list (string-split (string-delete #\, raw-patterns) #\space)))
   (map string->list raw-designs))))
