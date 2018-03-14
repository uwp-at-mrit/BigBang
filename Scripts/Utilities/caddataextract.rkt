#lang racket/gui

(define argn (vector-length (current-command-line-arguments)))

(define vertices.txt
  (cond [(zero? argn) (format "vertices.~a.txt" (current-milliseconds))]
        [else (vector-ref (current-command-line-arguments) 0)]))

(define memory (make-hash))

(displayln (format "~a:" vertices.txt))
(call-with-output-file* vertices.txt #:exists 'truncate/replace
  (λ [/dev/stdout]
    (let watch-clipboard ()
      (define datum (send the-clipboard get-clipboard-string 0))
      (unless (regexp-match? #px"^0+$" datum)
        (for ([line (in-lines (open-input-string datum))] #:unless (hash-has-key? memory (string-trim line)))
          (define raw (regexp-match #px"X = ([-0-9.]+), Y = ([-0-9.]+)" line))
          (when (pair? raw)
            (define pt (make-rectangular (string->number (cadr raw)) (string->number (caddr raw))))
            (hash-set! memory (string-trim line) #true)
            (displayln pt /dev/stdout)
            (displayln pt)))
        (sleep 1)
        (watch-clipboard)))))
(displayln (format "[Output to ~a]" vertices.txt))
