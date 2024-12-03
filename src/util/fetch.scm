;; function to fetch puzzle input for a specified day
(define-module (util fetch) #:export (fetch-input))
(use-modules (util io)
             (web client)
             (web response)
             (ice-9 format)
             (srfi srfi-11))
(define (fetch-input day)
 (let ((filename (string-append "input/day" (format #f "~2,'0d" day) ".txt")))
  (unless (file-exists? filename)
   (let*-values
    (((url)
      (string-join
       `("https://adventofcode.com" "2024" "day" ,(number->string day) "input")
       "/"))
     ((cookie) (getenv "AOC_COOKIE"))
     ((response response-body)
      (http-get url
                #:headers
                `((Cookie unquote (string-append "session=" cookie))))))
    (if (success? response)
     (write-file filename response-body)
     (raise-exception response-body))))))

(define (success? response)
 (eq? 200 (response-code response)))
