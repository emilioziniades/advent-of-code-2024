(define-module (day04) #:export (count-all-xmas count-all-x-mas))

(use-modules (util input) (srfi srfi-1) (ice-9 curried-definitions))

;; part 1
(define (count-all-xmas file)
 (let* ((data (make-grid file))
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

;; part 2
(define (count-all-x-mas file)
 (let* ((data (make-grid file))
        (size (array-length data)))
  (length
   (filter-map (is-x-mas? data) (combinations-2 (iota size) (iota size))))))

(define ((is-x-mas? arr) pos)
 (let* ((x (first pos))
        (y (second pos))
        (diagonals (list ; up left
                         (list (- x 1) (- y 1))
                         ; up right
                         (list (- x 1) (+ y 1))
                         ; down left
                         (list (+ x 1) (- y 1))
                         ; down right
                         (list (+ x 1) (+ y 1))))
        (char (apply array-ref (cons arr pos)))
        (char-diagonals (delay (map (lambda (p)
                                     (apply array-ref (cons arr p)))
                                    diagonals))))
  (and (all-in-bounds? arr diagonals)
       (equal? #\A char)
       (member (list->string (force char-diagonals))
               (list "MMSS" "SSMM" "MSMS" "SMSM")))))

;; common to part 1 and 2
(define (all-in-bounds? arr lst)
 (every (lambda (pos)
         (apply array-in-bounds? (cons arr pos)))
        lst))
