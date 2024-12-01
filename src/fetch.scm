(use-modules
  (web client)
  (web response)
  (ice-9 format)
  (srfi srfi-11))

(define (success? response)
  (eq? 200 (response-code response)))

(define (write-file filename text)
  (with-output-to-file filename (lambda () (display text))))

(define (fetch-input day)
  (let
    ((filename (string-append "input/day" (format #f "~2,'0d" day) ".txt")))
    (unless
      (file-exists? filename)
      (let*-values
        (((url) (string-join `("https://adventofcode.com" "2024" "day" ,(number->string day) "input") "/"))
         ((cookie) (getenv "AOC_COOKIE"))
         ((response response-body) (http-get url #:headers `((Cookie . ,(string-append "session=" cookie))))))
        (if (success? response)
          (write-file filename response-body)
          (raise-exception response-body))))))
