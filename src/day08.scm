(define-module (day08) #:export (count-antinodes count-harmonic-antinodes))

(use-modules (util io) (util input) (ice-9 curried-definitions) (srfi srfi-1))

;; part 1
(define (count-antinodes file)
 (let* ((arr (make-grid file))
        (antennas (get-antennas arr))
        (antenna-groups (hash-map-values antennas)))
  (length
   (filter
    (lambda (pos)
     (apply array-in-bounds? arr pos))
    (list-distinct
     (append-map (get-group-antinodes get-antinodes) antenna-groups))))))

(define (get-antinodes p1 p2)
 (let* ((x1 (first p1))
        (y1 (second p1))
        (x2 (first p2))
        (y2 (second p2))
        (dx (- x2 x1))
        (dy (- y2 y1)))
  (list (list (- x1 dx) (- y1 dy)) (list (+ x2 dx) (+ y2 dy)))))

;; part 2
(define (count-harmonic-antinodes file)
 (let* ((arr (make-grid file))
        (arr-size (array-length arr))
        (antennas (get-antennas arr))
        (antenna-groups (hash-map-values antennas)))
  (length
   (filter (lambda (pos)
            (apply array-in-bounds? arr pos))
           (list-distinct
            (append-map (get-group-antinodes (get-harmonic-antinodes arr-size))
                        antenna-groups))))))

(define ((get-harmonic-antinodes n) p1 p2)
 (let* ((x1 (first p1))
        (y1 (second p1))
        (x2 (first p2))
        (y2 (second p2))
        (dx (- x2 x1))
        (dy (- y2 y1)))
  (append-map (lambda (i)
               (list (list (- x1 (* i dx)) (- y1 (* i dy)))
                     (list (+ x2 (* i dx)) (+ y2 (* i dy)))))
              (iota n))))

;; common to both parts
(define ((get-group-antinodes get-antinode-fn) antenna-group)
 (list-distinct
  (append-map (lambda (antenna-pair)
               (if (equal? (first antenna-pair) (second antenna-pair))
                '()
                (apply get-antinode-fn antenna-pair)))
              (combinations-2 antenna-group antenna-group))))

(define (get-antennas arr)
 (let* ((size (array-length arr))
        (antennas (make-hash-table)))
  (for-each (lambda (pos)
             (let ((cell (apply array-ref (cons arr pos))))
              (unless (equal? cell #\.)
               (hash-append! antennas cell pos))))
            (combinations-2 (iota size) (iota size)))
  antennas))
