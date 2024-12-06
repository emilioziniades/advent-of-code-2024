(define-module (day05)
               #:export
               (count-correct-updates count-incorrect-updates))

(use-modules (util input)
             (srfi srfi-1)
             (ice-9 receive)
             (ice-9 curried-definitions)
             (ice-9 hash-table))

;; part 1
(define (count-correct-updates file)
 (let* ((data (parse-input file))
        (rules (rules->set (first data)))
        (updates (second data)))
  (list-sum
   (strings->numbers (map list-middle
                          (filter (lambda (update)
                                   (sorted? update (rules-less? rules)))
                                  updates))))))

;; part 2
(define (count-incorrect-updates file)
 (let* ((data (parse-input file))
        (rules (rules->set (first data)))
        (updates (second data)))
  (list-sum (strings->numbers
             (map list-middle
                  (map (lambda (update)
                        (sort update (rules-less? rules)))
                       (filter (lambda (update)
                                (not (sorted? update (rules-less? rules))))
                               updates)))))))

;; common to both parts
(define ((rules-less? rules) s1 s2)
 (cond
  ((hash-ref rules (list s1 s2) #f)
   #t)
  ((hash-ref rules (list s2 s1) #f)
   #f)
  (else
   (raise-exception "cannot sort pair based on available rules"))))

(define (rules->set rules)
 (alist->hash-table (map (lambda (rule)
                          (cons rule #t))
                         rules)))

(rules->set '(("a" "A")))
(define (list-middle lst)
 (list-ref lst (truncate (/ (length lst) 2))))

;; input parsing
(define (parse-input file)
 (receive (rules updates)
  (let* ((file-lines (lines file))
         (blank-line-index (list-index string-null? file-lines)))
   (split-at file-lines blank-line-index))
  (list (map (string-split-on #\|) rules)
        (map (string-split-on #\,) (cdr updates)))))
