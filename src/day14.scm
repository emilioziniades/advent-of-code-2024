(define-module (day14) #:export (safety-factor ticks-until-xmas-tree))

(use-modules (util input)
             (srfi srfi-1)
             (ice-9 curried-definitions)
             (ice-9 string-fun))

;; part 1
(define ((safety-factor height width ticks) file)
 (define (count-quadrants robots)
  (let ((mid-x (floor (/ width 2)))
        (mid-y (floor (/ height 2))))
   (fold (lambda (robot prev)
          (let ((x (first robot))
                (y (second robot)))
           (cond
            ((and (< x mid-x)
                  (< y mid-y))
             (hash-increment prev 'top-left))
            ((and (> x mid-x)
                  (< y mid-y))
             (hash-increment prev 'top-right))
            ((and (< x mid-x)
                  (> y mid-y))
             (hash-increment prev 'bottom-left))
            ((and (> x mid-x)
                  (> y mid-y))
             (hash-increment prev 'bottom-right))
            (else
             prev))))
         (make-hash-table)
         robots)))

 (apply
  *
  (hash-map-values
   (count-quadrants (map (lambda (robot)
                          (move-robot robot height width ticks))
                         (parse-input file))))))

;; part 2
(define ((ticks-until-xmas-tree height width) file)
 (define (helper robots ticks)
  (if (is-xmas-tree? robots)
   (begin
    ;; (display-robots robots height width)
    ticks)
   (helper (map (lambda (robot)
                 (move-robot robot height width 1))
                robots)
           (1+ ticks))))

 (helper (parse-input file) 0))

(define (display-robots robots height width)
 (let ((hash-map (fold
                  (lambda (robot prev)
                   (hash-set! prev (list (first robot) (second robot)) "#")
                   prev)
                  (make-hash-table)
                  robots)))
  (for-each (lambda (y)
             (for-each (lambda (x)
                        (display (hash-ref hash-map (list x y) " ")))
                       (iota width))
             (newline))
            (iota height))))

; NOTE: after visually inspecting the output for a few ticks, I suspected
; that the picture of the christmas tree would have a border. So, this function
; checks for grids that have a lot of robots on the same row or column.
; I tweaked the row count until it excluded all false-positives but included
; the actual christmas tree, and ended up with 35 in a column and 30 in a row.
(define (is-xmas-tree? robots)
 (define row-counts (make-hash-table))
 (define column-counts (make-hash-table))
 (for-each (lambda (robot)
            (hash-increment row-counts (second robot))
            (hash-increment column-counts (first robot)))
           robots)
 (and ; more than 30 elements in a single row
      (any (lambda (row-count)
            (>= row-count 30))
           (hash-map-values row-counts))
      ; more than 35 elements in a single column
      (any (lambda (column-count)
            (>= column-count 35))
           (hash-map-values column-counts))))

;; common to both parts
(define (move-robot robot height width ticks)
 (let ((x (first robot))
       (y (second robot))
       (dx (third robot))
       (dy (fourth robot)))
  (list (modulo (+ x (* dx ticks)) width)
        (modulo (+ y (* dy ticks)) height)
        dx
        dy)))

;; input parsing
(define (parse-input file)
 (map (lambda (x)
       (strings->numbers
        (string-split
         (string-delete (string->char-set "pv=")
                        (string-replace-substring x "," " "))
         #\space)))
      (lines file)))
