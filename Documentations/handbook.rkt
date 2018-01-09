#lang racket

(provide (all-defined-out))
(provide (all-from-out scriblib/autobib))

(require scribble/core)
(require scribble/manual)
(require scriblib/autobib)

(define handbook-smart-table
  (lambda []
    (table-of-contents)))

(define handbook-appendix
  (let ([entries (list (bib-entry #:key      "Racket"
                                  #:title    "Reference: Racket"
                                  #:author   (authors "Matthew Flatt" "PLT")
                                  #:date     "2010"
                                  #:location (techrpt-location #:institution "PLT Design Inc." #:number "PLT-TR-2010-1")
                                  #:url      "https://racket-lang.org/tr1")
                       (bib-entry #:key      "Scribble"
                                  #:title    "The Racket Documentation Tool"
                                  #:author   (authors "Matthew Flatt" "Eli Barzilay")
                                  #:url      "https://docs.racket-lang.org/scribble/index.html"))])
    (lambda [#:index? [index? #true] . bibentries]
      (define appendix-style (make-style 'index '(grouper)))
      ((curry filter-not void?)
       (list (struct-copy part (apply bibliography #:tag "handtool-bibliography" (append entries bibentries))
                          [title-content (list "参考文献")])
             (unless (false? index?)
               (struct-copy part (index-section #:tag "handbook-index")
                            [title-content (list "关键词索引")])))))))
