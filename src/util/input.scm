;; some functions useful for input parsing
(define-module (util input)
               #:export
               (split-spaces string-split-on
                             drop-null-strings
                             strings->numbers
                             lines
                             list-sum
                             make-grid
                             combinations-2
                             parse-int
                             list-distinct
                             neighbours))

(use-modules (ice-9 textual-ports)
             (ice-9 curried-definitions)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-13))

(define (drop-null-strings lst)
 (filter (compose not string-null?) lst))

(define (lines str)
 (string-split (string-trim-both str #\newline) #\newline))

(define ((string-split-on sep) str)
 (string-split str sep))

(define (split-spaces str)
 ((string-split-on #\space) str))

(define (strings->numbers lst)
 (map string->number lst))

(define (list-sum lst)
 (fold + 0 lst))

(define (make-grid str)
 (list->array 2 (map string->list (lines str))))

(define (combinations-2 lst1 lst2)
 (append-map (lambda (i)
              (map (lambda (j)
                    (list i j))
                   lst1))
             lst2))

(define (parse-int char)
 (- (char->integer char) 48))

(define (list-distinct lst)
 (define set (make-hash-table))
 (for-each (lambda (x)
            (hash-set! set x #t))
           lst)
 (hash-map->list (lambda (k v)
                  k)
                 set))

(define (neighbours x y)
 (list (list (1- x) y) (list (1+ x) y) (list x (1- y)) (list x (1+ y))))
