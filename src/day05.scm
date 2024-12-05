(define-module (day05) #:export (count-correct-updates))

(use-modules (util io)
             (util input)
             (srfi srfi-1)
             (ice-9 receive)
             (ice-9 curried-definitions)
             (oop goops))

;; part 1
(define (count-correct-updates file)
 (let* ((data (parse-input file))
        (rules (first data))
        (updates (second data)))
  (list-sum
   (strings->numbers (map list-middle
                          (filter (lambda (update)
                                   (sorted? update (rules-less? rules)))
                                  updates))))))

(define ((rules-less? rules) s1 s2)
 (cond
  ((member (list s1 s2) rules)
   #t)
  ((member (list s2 s1) rules)
   #f)
  (#t
   (raise-exception "cannot sort pair based on available rules"))))

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
