(define-module (day23) #:export (count-t-triplets))

(use-modules (util io) (util input) (srfi srfi-1) (ice-9 hash-table))

; part 1
(define (count-t-triplets file)
 (let* ((hashmap (parse-input file))
        (triplets (find-all-triplet-groups hashmap))
        (t-triplets (filter (lambda (triplet)
                             (any (lambda (pc)
                                   (string-prefix? "t" pc))
                                  triplet))
                            triplets)))
  (length t-triplets)))

(define (find-all-triplet-groups hashmap)
 (list-distinct (map (lambda (lst)
                      (sort lst string<))
                     (append-map (lambda (key)
                                  (find-triplet-group hashmap key))
                                 (hash-map-keys hashmap)))))

(define (find-triplet-group hashmap key)
 (let ((possible-triplets (apply append (extend-group hashmap (list key) 2))))
  ; does member 3 connect back to member 1
  (filter (lambda (triplet)
           (member (car triplet) (hash-ref hashmap key)))
          possible-triplets)))

(define (extend-group hashmap group n)
 (if (= n 0)
  group
  (filter-map (lambda (next)
               (if (member next group)
                #f
                (extend-group hashmap (cons next group) (1- n))))
              (hash-ref hashmap (car group)))))

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
