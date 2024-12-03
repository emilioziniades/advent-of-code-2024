;; a very simple test runner
(define-module (test-runner) #:export (init-test-runner))

(use-modules (ice-9 getopt-long)
             (ice-9 format)
             (ice-9 regex)
             (srfi srfi-64)
             (util colour))

(define (init-test-runner)
 (define runner (test-runner-null))
 (test-runner-on-test-end! runner on-test-end)
 (test-runner-on-group-begin! runner on-group-begin)
 (test-runner-on-group-end! runner on-group-end)
 (test-runner-on-final! runner on-final)
 (test-runner-current runner)
 (set-test-filter-from-command-line))

(define (on-final runner)
 (begin
  (newline)
  (display "pass: ")
  (display (test-runner-pass-count runner))
  (newline)
  (display "fail: ")
  (display (test-runner-fail-count runner))
  (newline)
  (display "skip: ")
  (display (test-runner-skip-count runner))
  (newline))
 (if (> (test-runner-fail-count runner) 0)
  (exit 1)
  (exit 0)))

(define (on-group-begin runner suite-name _count)
 (begin
  (newline)
  (display "START ")
  (display suite-name)
  (newline)
  (newline)))

(define (on-group-end runner)
 (begin
  (newline)
  (display "END")
  (newline)))

(define (on-test-end runner)
 (let ((test-name (test-result-ref runner 'test-name #f))
       (actual-value (object->string (test-result-ref runner 'actual-value #f)))
       (expected-value (object->string
                        (test-result-ref runner 'expected-value #f)))
       (result-kind (symbol->string (test-result-ref runner 'result-kind #f))))
  (begin
   (cond
    ((equal? result-kind "skip")
     (begin
      (display-colour grey (right-pad (string-append test-name ":")))
      (display-colour grey result-kind)))
    ((equal? result-kind "pass")
     (begin
      (display-colour green (right-pad (string-append test-name ":")))
      (display-colour green result-kind)))
    ((equal? result-kind "fail")
     (begin
      (display-colour red (right-pad (string-append test-name ": ")))
      (display-colour red result-kind)
      (display "\t")
      (display-colour red "expected: ")
      (display-colour red expected-value)
      (display-colour red ", actual: ")
      (display-colour red actual-value)))
    (#t
     (raise-exception (string-append "unexpected result kind: " result-kind))))
   (newline))))

(define (right-pad s)
 (format #f "~30a" s))

(define (set-test-filter-from-command-line)
 (define option-spec '((filter (value #t))))
 (define options (getopt-long (command-line) option-spec))
 (define filter (option-ref options 'filter #f))
 (when filter
  (test-skip (lambda (runner)
              (not (string-match filter (test-runner-test-name runner)))))))
