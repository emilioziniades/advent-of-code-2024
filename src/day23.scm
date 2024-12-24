(define-module (day23) #:export (count-t-triplets find-lan-party-password))

(use-modules (util io) (util input) (srfi srfi-1) (ice-9 hash-table))

; part 1
(define (count-t-triplets file)
 (let* ((hashmap (parse-input file))
        (triplets (get-3-cliques hashmap))
        (t-triplets (filter (lambda (triplet)
                             (any (lambda (pc)
                                   (string-prefix? "t" pc))
                                  triplet))
                            triplets)))
  (length t-triplets)))

(define (get-3-cliques hashmap)
 (define (helper n cliques)
  (if (= n 0)
   cliques
   (helper (1- n) (grow-cliques hashmap cliques))))

 (helper 3 '(())))

; TODO: this second part is really slow - it takes several minutes to run. Consider making it faster.
; part 2
(define (find-lan-party-password file)
 (string-join (find-biggest-clique (parse-input file)) ","))

(define (find-biggest-clique hashmap)
 (define (helper cliques)
  (if (and (= 1 (length cliques))
           (> (length (car cliques)) 0))
   (car cliques)
   (helper (grow-cliques hashmap cliques))))

 (helper '(())))

(define (grow-cliques hashmap cliques)
 (display (length cliques))
 (newline)
 (list-distinct
  (sort-lists
   (append-map (lambda (clique)
                (filter-map (lambda (next)
                             (if (is-fully-connected? hashmap next clique)
                              (cons next clique)
                              #f))
                            (missing-members hashmap clique)))
               cliques)
   string<)))

(define (is-fully-connected? hashmap node others)
 (every (lambda (other)
         (member other (hash-ref hashmap node)))
        others))

(define (missing-members hashmap clique)
 (filter (lambda (next)
          (not (member next clique)))
         (hash-map-keys hashmap)))

(define (unordered-pairs lst)
 (if (< (length lst) 2)
  '()
  (append (map (lambda (x)
                (cons (car lst) x))
               (cdr lst))
          (unordered-pairs (cdr lst)))))

(define (sort-lists lsts less)
 (map (lambda (lst)
       (sort lst less))
      lsts))

; input parsing
(define (parse-input file)
 (define hashmap (make-hash-table))
 (let ((pairs (map (lambda (line)
                    (string-split line #\-))
                   (lines file))))
  (for-each (lambda (pair)
             (hash-append! hashmap (car pair) (cadr pair))
             (hash-append! hashmap (cadr pair) (car pair)))
            pairs)
  hashmap))
