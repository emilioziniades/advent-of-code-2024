(define-module (day20) #:export (count-good-cheats))

(use-modules (util io)
             (util input)
             (util queue)
             (srfi srfi-1)
             (srfi srfi-11)
             (ice-9 hash-table)
             (ice-9 curried-definitions))

; part 1
(define ((count-good-cheats minimum-save) file)
 (let* ((arr (make-grid file))
        (all-positions (get-grid-positions arr))
        (start (find (lambda (pos)
                      (equal? #\S (array-ref-nd arr pos)))
                     all-positions))
        (end (find (lambda (pos)
                    (equal? #\E (array-ref-nd arr pos)))
                   all-positions))
        (costs (djikstra start end arr))
        (possible-cheats (all-possible-cheats arr)))
  (count (lambda (n)
          (>= n minimum-save))
         (map (lambda (cheat)
               (test-cheat cheat costs))
              possible-cheats))))

(define (get-grid-positions arr)
 (let ((size (array-length arr)))
  (combinations-2 (iota size) (iota size))))

(define (all-possible-cheats arr)
 (let* ((all-positions (get-grid-positions arr))
        (valid-starts (filter (lambda (pos)
                               (not (equal? #\# (array-ref-nd arr pos))))
                              all-positions)))
  (append-map (lambda (start)
               (filter-map (lambda (end)
                            (if (and (array-in-bounds-nd? arr end)
                                     (not (equal? #\# (array-ref-nd arr end))))
                             (list start end)
                             #f))
                           (possible-cheat-ends start)))
              valid-starts)))

(define (possible-cheat-ends start)
 (let ((y (first start))
       (x (second start)))
  (list ;; right 2
        (list y (+ x 2))
        ;; left 2
        (list y (- x 2))
        ;; down 2
        (list (+ y 2) x)
        ;; up 2
        (list (- y 2) x)
        ;; down-right 1
        (list (+ y 1) (+ x 1))
        ;; down-left 1
        (list (+ y 1) (- x 1))
        ;; up-right 1
        (list (- y 1) (+ x 1))
        ;; up-left 1
        (list (- y 1) (- x 1)))))

;; pretty much a copy-paste from a-star in day18, without the heuristic.
(define (djikstra start end arr)
 (define frontier (push '() start 0))
 (define cost-so-far (alist->hash-table (list (cons start 0))))
 (define (get-neighbours pos)
  (filter (lambda (p)
           (and (array-in-bounds-nd? arr p)
                (not (equal? #\# (array-ref-nd arr p)))))
          (apply neighbours pos)))

 (define (helper frontier cost-so-far)
  (if (or (null? frontier)
          (equal? (caar (pop frontier)) end))
   cost-so-far
   (let-values
    (((current rest) (pop frontier)))
    (apply helper
           (fold (lambda (next prev)
                  (let* ((f (first prev))
                         (c (second prev))
                         (new-cost (1+ (hash-ref cost-so-far (car current))))
                         (next-cost (hash-ref cost-so-far next))
                         (priority new-cost))
                   (if (or (not next-cost)
                           (< new-cost next-cost))
                    (list (push f next priority)
                          (begin
                           (hash-set! c next new-cost)
                           (values c)))
                    prev)))
                 (list rest cost-so-far)
                 (get-neighbours (car current)))))))

 (helper frontier cost-so-far))

;; This is the core of the algorithm. Once we have run djikstra
;; for the original racetrack, we obtain a costs hash-table which is
;; a mapping from position -> cost (in this case time). We can evaluate
;; a cheat by considering how much cost would be saved by moving from
;; cheat-start to cheat-end, accounting for the time it takes to travel
;; from cheat-start to cheat-end, which is 2 in this puzzle.
(define (test-cheat cheat costs)
 (let ((start (first cheat))
       (end (second cheat)))
  (- (hash-ref costs end) (hash-ref costs start) 2)))
