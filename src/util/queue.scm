;; priority queue implementation. push is O(n) and pop is O(1)
(define-module (util queue) #:export (queue push pop))

(use-modules (srfi srfi-1))

(define (queue)
 (list))

(define (push queue item priority)
 (cond
  ((null? queue)
   (list (cons item priority)))
  ((<= priority (cdar queue))
   (cons (cons item priority) queue))
  (else
   (cons (car queue) (push (cdr queue) item priority)))))

(define (pop queue)
 (car+cdr queue))
