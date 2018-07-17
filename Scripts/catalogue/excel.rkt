#lang racket/gui

(provide read-xlsx)

(require racket/pretty)
(require simple-xlsx)

(define (read-xlsx src.xlsx)
  (define metrics (make-hasheq))
  
  (when (path? src.xlsx)
    (with-input-from-xlsx-file src.xlsx
      (lambda [data.xlsx]
        (for ([sheet (in-list (get-sheet-names data.xlsx))])
          (printf "exporting ~a...~n" sheet)
          (load-sheet sheet data.xlsx)
          (hash-set! metrics (string->symbol sheet) (get-sheet-rows data.xlsx))))))

  metrics)

(module+ main
  (define src.xlsx (get-file))
  (define dest.rktl (path-replace-extension src.xlsx ".rktl"))

  (with-output-to-file dest.rktl #:exists 'truncate/replace
    (thunk (pretty-write (read-xlsx src.xlsx)))))
