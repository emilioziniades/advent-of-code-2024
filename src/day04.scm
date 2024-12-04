(define-module (day04) #:export (count-all-xmas))

(use-modules (util io) (util input) (srfi srfi-1) (ice-9 curried-definitions))

;; part 1
(define (count-all-xmas file)
 (let* ((data (parse-input file))
        (size (array-length data)))
  (apply + (map (count-xmas data) (combinations-2 (iota size) (iota size))))))

(define ((count-xmas arr) pos)
 (let* ((x (first pos))
        (y (second pos)))
  (length (filter-map (is-xmas? arr)
                      (list ; right
                            (map (lambda (i)
                                  (list x (+ y i)))
                                 (iota 4))
                            ; left
                            (map (lambda (i)
                                  (list x (- y i)))
                                 (iota 4))
                            ; down
                            (map (lambda (i)
                                  (list (+ x i) y))
                                 (iota 4))
                            ; up
                            (map (lambda (i)
                                  (list (- x i) y))
                                 (iota 4))
                            ; right-down
                            (map (lambda (i)
                                  (list (+ x i) (+ y i)))
                                 (iota 4))
                            ; right-up
                            (map (lambda (i)
                                  (list (- x i) (+ y i)))
                                 (iota 4))
                            ; left-down
                            (map (lambda (i)
                                  (list (+ x i) (- y i)))
                                 (iota 4))
                            ; left-up
                            (map (lambda (i)
                                  (list (- x i) (- y i)))
                                 (iota 4)))))))

(define ((is-xmas? arr) lst)
 (if (all-in-bounds? arr lst)
  (equal? (string->list "XMAS")
          (map (lambda (pos)
                (apply array-ref (cons arr pos)))
               lst))
  #f))

;; common to part 1 and 2
(define (all-in-bounds? arr lst)
 (every (lambda (pos)
         (apply array-in-bounds? (cons arr pos)))
        lst))

;; input parsing
(define (parse-input file)
 (list->array 2 (map string->list (lines file))))

;; utility functions
(define (combinations-2 lst1 lst2)
 (append-map (lambda (i)
              (map (lambda (j)
                    (list i j))
                   lst1))
             lst2))

;; workspace
(define file (read-file "example/day04.txt"))
(define arr (parse-input file))
