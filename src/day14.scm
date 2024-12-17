;; remove (util io) once done REPLing
(define-module (day14) #:export (safety-factor))

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
