(define-module (day09)
               #:export
               (filesystem-checksum filesystem-checksum-block))

(use-modules (util input) (srfi srfi-1))

;; part 1
(define (filesystem-checksum file)
 (let* ((disk-map (parse-input file))
        (expanded-disk-map (expand-disk-map disk-map)))
  (calculate-checksum (compact-disk-map expanded-disk-map))))

(define (compact-disk-map arr)
 (define (recursive-compact-disk-map arr pointer1 pointer2)
  (cond
   ((>= pointer1 pointer2)
    arr)
   ((array-ref arr pointer1)
    (recursive-compact-disk-map arr (1+ pointer1) pointer2))
   ((not (array-ref arr pointer2))
    (recursive-compact-disk-map arr pointer1 (1- pointer2)))
   (else
    (begin
     (array-swap! arr pointer1 pointer2)
     (recursive-compact-disk-map arr (1+ pointer1) (1- pointer2))))))

 (recursive-compact-disk-map arr 0 (1- (array-length arr))))

(define (calculate-checksum arr)
 (apply +
        (map (lambda (i)
              (* i
                 (or (array-ref arr i)
                     0)))
             (iota (1+ (array-index-right arr identity))))))

;; part 2
(define (filesystem-checksum-block file)
 (let* ((disk-map (parse-input file))
        (expanded-disk-map (expand-disk-map disk-map)))
  (calculate-checksum (compact-disk-map-block expanded-disk-map))))

;; Find the biggest file ID.
;; For each file ID from biggest to 0:
;;   Find a space to its left big enough.
;;     If found:
;;       Swap with the beginning of that space.
;;       Otherwise move on the the next file.
(define (compact-disk-map-block dm)
 (define (recursive-compact-disk-map-block id id-start)
  (if (< id 0)
   dm
   (let* ((id-end (file-end-index dm id id-start))
          (size (1+ (- id-start id-end)))
          (free-space-start (find-free-space dm size)))
    (when (and free-space-start
               (< free-space-start id-end))
     (begin
      (for-each (lambda (i)
                 (array-swap! dm (+ i id-end) (+ i free-space-start)))
                (iota size))))
    (recursive-compact-disk-map-block (1- id)
                                      (array-index-right dm
                                                         (lambda (x)
                                                          (equal? x (1- id)))
                                                         id-end)))))

 (let* ((file-id-start (array-index-right dm identity))
        (file-id (array-ref dm file-id-start)))
  (recursive-compact-disk-map-block file-id file-id-start)))

(define (file-end-index arr file-id id-start)
 (define (recursive-file-end-index idx)
  (if (and (array-in-bounds? arr idx)
           (equal? file-id (array-ref arr idx)))
   (recursive-file-end-index (1- idx))
   (1+ idx)))

 (recursive-file-end-index id-start))

;; TODO: this function makes the whole part 2 algorithm O(N^2), because
;; for every file, we have to scan the whole array again.
(define (find-free-space arr size)
 (define (find-free-space-recursive idx)
  (cond
   ((not (array-in-bounds? arr (+ idx (1- size))))
    #f)
   ((every not
           (map (lambda (i)
                 (array-ref arr i))
                (iota size idx)))
    idx)
   (else
    (find-free-space-recursive (1+ idx)))))

 (find-free-space-recursive 0))

;; utility functions
(define (expand-disk-map disk-map)
 (define (expand-files lst)
  (let ((arr (list->array 1 lst)))
   (map (lambda (i)
         (make-list (parse-int (array-ref arr i)) i))
        (iota (array-length arr)))))

 (define (expand-free-space lst)
  (map (lambda (n)
        (make-list (parse-int n) #f))
       lst))

 (let* ((files (even-items disk-map))
        (free-space (odd-items disk-map))
        (files-expanded (expand-files files))
        (free-space-expanded (expand-free-space free-space)))
  (list->array 1 (flatten (interleave files-expanded free-space-expanded)))))

(define (even-items arr)
 (map (lambda (i)
       (array-ref arr i))
      (iota (ceiling (/ (array-length arr) 2)) 0 2)))

(define (odd-items arr)
 (map (lambda (i)
       (array-ref arr i))
      (iota (floor (/ (array-length arr) 2)) 1 2)))

(define (interleave lst1 lst2)
 (if (null? lst1)
  lst2
  (cons (car lst1) (interleave lst2 (cdr lst1)))))

(define (array-index arr pred)
 (define (recursive-array-index arr idx)
  (cond
   ((not (array-in-bounds? arr idx))
    #f)
   ((pred (array-ref arr idx))
    idx)
   (else
    (recursive-array-index arr (1+ idx)))))

 (recursive-array-index arr 0))

(define* (array-index-right arr pred #:optional start)
         (define (recursive-array-index-right arr idx)
          (cond
           ((not (array-in-bounds? arr idx))
            #f)
           ((pred (array-ref arr idx))
            idx)
           (else
            (recursive-array-index-right arr (1- idx)))))

         (recursive-array-index-right arr
                                      (or start
                                          (1- (array-length arr)))))

(define (array-swap! arr idx1 idx2)
 (let ((n1 (array-ref arr idx1))
       (n2 (array-ref arr idx2)))
  (begin
   (array-set! arr n1 idx2)
   (array-set! arr n2 idx1))))

(define (flatten lst)
 (append-map identity lst))

;; input parsing
(define (parse-input file)
 (list->array 1 (string->list (string-trim-right file #\newline))))
