(define-module (day19) #:export (count-possible-designs count-design-ways))

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

;; part 2
(define (count-design-ways file)
 (define cache (make-hash-table))
 (let-values (((patterns designs) (parse-input file)))
             (apply +
                    (map (lambda (design)
                          (count-ways design patterns cache))
                         designs))))

(define (count-ways design patterns cache)
 (let ((cached-ways (hash-ref cache design)))
  (or cached-ways
      (if (null? design)
       1
       (apply +
              (map (lambda (n)
                    (if (hash-ref patterns (take design n))
                     (let* ((new-design (drop design n))
                            (ways (count-ways new-design patterns cache)))
                      (begin
                       (hash-set! cache new-design ways)
                       ways))
                     0))
                   (iota (length design) 1)))))))

; input parsing
(define (parse-input file)
 (let* ((file-lines (lines file))
        (raw-patterns (car file-lines))
        (raw-designs (cddr file-lines)))
  (values
   (list->hash-set
    (map string->list (string-split (string-delete #\, raw-patterns) #\space)))
   (map string->list raw-designs))))
