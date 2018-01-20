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

(define tamer-story->modpath
  (lambda [story-path]
    `(file ,story-path)))

(define-syntax (tamer-story->tag stx)
  (syntax-case stx []
    [(_ story-sexp)
     #'(let ([modpath (with-handlers ([exn? (const (quote-source-file))]) (cadr story-sexp))])
         (path->string (path-replace-extension (file-name-from-path modpath) "")))]))

(define-syntax (define-bib stx)
  (syntax-parse stx #:literals []
    [(_ id bib-args ...)
     #'(define id (in-bib (make-bib bib-args ...) (format ":~a" 'id)))]))

(define-syntax (handbook-start stx)
  (syntax-case stx [scribble +]
    [(_)
     #'(let ([modpath (quote-module-path)])
         (cond [(path? modpath) (tamer-story (tamer-story->modpath modpath))]
               [else (let ([story (tamer-story->modpath (cadr modpath))])
                       (tamer-story story))]))]))

(define ~cite
  (lambda [bib #:same-author? [same? #false] . bibs]
    (if (false? same?)
        (apply (tamer-cites) bib bibs)
        (apply (tamer-cite) bib bibs))))

(define-syntax (handbook-story stx)
  (syntax-parse stx #:literals []
    [(_ (~optional (~seq #:style s:expr)) contents ...)
     #`(begin (handbook-start)
              (define-cite ~cite ~cites ~reference #:style number-style)
              (tamer-reference ~reference)
              (tamer-cites ~cites)
              (tamer-cite ~cite)
              (title #:tag (tamer-story->tag (tamer-story))
                     #:style #,(attribute s)
                     contents ...))]))

(define handbook-references
  (lambda []
    (list ((tamer-reference) #:sec-title "参考文献"
                             #:tag (format "~a:reference" (path-replace-extension (tamer-story->tag (tamer-story)) "")))
          (tamer-story #false))))
