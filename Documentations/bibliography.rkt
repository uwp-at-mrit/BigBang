#lang racket

(provide (all-defined-out))
(provide (all-from-out scriblib/autobib))

(require scribble/base)
(require scriblib/autobib)

(require syntax/location)

(require (for-syntax syntax/parse))

(define tamer-story (make-parameter #false))
(define tamer-cite (make-parameter void))
(define tamer-cites (make-parameter void))
(define tamer-reference (make-parameter void))

(define-syntax (define-bib stx)
  (syntax-parse stx #:literals []
    [(_ id bib-args ...)
     #'(define id (in-bib (make-bib bib-args ...) (format ":~a" 'id)))]))

(define ~cite
  (lambda [bib #:same-author? [same? #false] . bibs]
    (if (false? same?)
        (apply (tamer-cites) bib bibs)
        (apply (tamer-cite) bib bibs))))

(define-syntax (tamer-story->tag stx)
  (syntax-case stx []
    [(_ modpath)
     #'(path->string (path-replace-extension (file-name-from-path modpath) ""))]))

(define-syntax (handbook-story stx)
  (syntax-parse stx #:literals []
    [(_ (~optional (~seq #:style s:expr)) contents ...)
     #`(begin (tamer-story (quote-source-file))
              (define-cite ~cite ~cites ~reference #:style number-style)
              (tamer-reference ~reference)
              (tamer-cites ~cites)
              (tamer-cite ~cite)
              (title #:tag (tamer-story->tag (tamer-story))
                     #:style #,(attribute s)
                     contents ...))]))

(define handbook-references
  (lambda []
    (list (tamer-story)
          ((tamer-reference) #:sec-title "参考文献"
                             #:tag (format "~a:reference" (path-replace-extension (tamer-story->tag (tamer-story)) "")))
          (tamer-story #false))))
