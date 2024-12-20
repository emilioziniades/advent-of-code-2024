(define-module (day20) #:export (count-good-cheats))

(use-modules (util io)
             (util input)
             (util queue)
             (srfi srfi-1)
             (srfi srfi-11)
             (ice-9 hash-table)
             (ice-9 curried-definitions))

; part 1 and part 2
(define ((count-good-cheats minimum-save min-cheat-length max-cheat-length) file)
 (let* ((arr (make-grid file))
        (all-positions (get-grid-positions arr))
        (start (find (lambda (pos)
                      (equal? #\S (array-ref-nd arr pos)))
                     all-positions))
        (end (find (lambda (pos)
                    (equal? #\E (array-ref-nd arr pos)))
                   all-positions))
        (costs (djikstra start end arr))
        (possible-cheats (all-possible-cheats arr
                                              min-cheat-length
                                              max-cheat-length)))
  (count (lambda (n)
          (>= n minimum-save))
         (map (lambda (cheat)
               (test-cheat cheat costs))
              possible-cheats))))

(define (get-grid-positions arr)
 (let ((size (array-length arr)))
  (combinations-2 (iota size) (iota size))))

(define (all-possible-cheats arr min-cheat-length max-cheat-length)
 (let* ((all-positions (get-grid-positions arr))
        (valid-positions (filter (lambda (pos)
                                  (not (equal? #\# (array-ref-nd arr pos))))
                                 all-positions)))
  (append-map
   (lambda (start)
    (filter-map (lambda (end)
                 (if (and (array-in-bounds-nd? arr end)
                          (not (equal? #\# (array-ref-nd arr end))))
                  (list start end)
                  #f))
                (possible-cheat-ends start min-cheat-length max-cheat-length)))
   valid-positions)))

(define (possible-cheat-ends start min-length max-length)
 (let ((cheat-lengths (iota (1+ (- max-length min-length)) min-length)))
  (append-map (lambda (n)
               (manhattan-positions n start))
              cheat-lengths)))

;; pretty much a copy-paste from a-star in day18, without the heuristic. I could
;; have probably just used DFS, but since the graph is so simple (each node only has
;; two edges), I didn't bother simplifying this further. The real bottleneck in this
;; solution, particularly for part 2, was figuring out the possible (cheat-start, cheat-end)
;; pairs.
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
;; from cheat-start to cheat-end
(define (test-cheat cheat costs)
 (let ((start (first cheat))
       (end (second cheat)))
  (- (hash-ref costs end)
     (hash-ref costs start)
     (apply manhattan (append start end)))))

;; all the n1, n2 such that n1 + n2 = number
(define (unsum number)
 (map (lambda (n)
       (list n (- number n)))
      (iota number)))

;; returns all the coordinates with a manhattan distance of
;; `distance` away from `position`.
(define (manhattan-positions distance position)
 (let ((y (first position))
       (x (second position)))
  (list-distinct (append-map (lambda (lst)
                              (let ((n1 (first lst))
                                    (n2 (second lst)))
                               (list (list (+ y n1) (+ x n2))
                                     (list (+ y n1) (- x n2))
                                     (list (- y n1) (+ x n2))
                                     (list (- y n1) (- x n2))
                                     (list (+ y n2) (+ x n1))
                                     (list (+ y n2) (- x n1))
                                     (list (- y n2) (+ x n1))
                                     (list (- y n2) (- x n1)))))
                             (unsum distance)))))
