(define-module (day25) #:export (count-lock-key-pairs))

(use-modules (util input) (srfi srfi-1) (srfi srfi-11) (ice-9 string-fun))

(define (count-lock-key-pairs file)
 (let*-values (((lock-schematics key-schematics) (parse-input file))
               ((locks) (map schematic->heights lock-schematics))
               ((keys) (map schematic->heights key-schematics)))
              (count identity
                     (map (lambda (pair)
                           (apply test-lock-key-pair pair))
                          (combinations-2 locks keys)))))

(define (test-lock-key-pair lock key)
 (every (lambda (l k)
         (<= (+ l k) 5))
        lock
        key))

(define (schematic->heights lst)
 (map (lambda (x)
       (1- (count (lambda (xx)
                   (equal? xx #\#))
                  x)))
      (apply zip lst)))

(define (parse-input file)
 (partition (lambda (x)
             (every (lambda (xx)
                     (equal? xx #\#))
                    (car x)))
            (map (lambda (x)
                  (map string->list (lines x)))
                 (string-split (string-replace-substring file "\n\n" "|") #\|))))
