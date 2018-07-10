#lang racket/gui

(require racket/pretty)
(require simple-xlsx)

(define src.xlsx (get-file))

(when (path? src.xlsx)
  (define dest.rktl (path-replace-extension src.xlsx ".rktl"))

  (define metrics (make-hasheq))
  
  (with-input-from-xlsx-file src.xlsx
    (lambda [data.xlsx]
      (for ([sheet (in-list (get-sheet-names data.xlsx))])
        (printf "exporting ~a...~n" sheet)
        (load-sheet sheet data.xlsx)
        (hash-set! metrics (string->symbol sheet) (get-sheet-rows data.xlsx)))))

  (with-output-to-file dest.rktl #:exists 'truncate/replace
    (thunk (pretty-write metrics))))
