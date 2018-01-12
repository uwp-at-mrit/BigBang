#lang racket

(provide (all-defined-out))
(provide (all-from-out "bibliography.rkt"))

(require scribble/core)
(require scribble/manual)
(require scribble/html-properties)

(require "bibliography.rkt")

(define preface-style (make-style 'index '(grouper unnumbered)))

(define sln-root (make-parameter #false))

(define document-root
  (lambda []
    (simplify-path (build-path (sln-root) "Documentations"))))

(define handbook-root
  (lambda []
    (build-path (document-root) "Handbook")))

(define handbook-entry
  (lambda [[proj-root (current-directory)]]
    (define-values (parent projname _) (split-path proj-root))
    (simplify-path (build-path proj-root (path-add-extension projname #".scrbl")))))

(define handbook-style
  (lambda [[proj-root (current-directory)]]
    (define-values (parent projname _) (split-path proj-root))
    (simplify-path (build-path proj-root (path-add-extension projname #".css")))))

(define find-solution-root-dir
  (lambda [[dir (current-directory)]]
    (cond [(ormap (curry regexp-match? #px"[.]sln$") (directory-list dir)) dir]
          [else (let-values ([(parent dirname _) (split-path dir)])
                  (and parent (find-solution-root-dir parent)))])))

(define handbook-title
  (lambda [#:authors authors . pre-contents]
    (parameterize ([sln-root (find-solution-root-dir)])
      (define styles (filter file-exists? (list (build-path (document-root) "handbook.css") (handbook-style))))
      (list (title #:style (make-style #false (map make-css-addition styles)) #:tag "handbook"
                   (if (pair? pre-contents) pre-contents (list (literal "开发手册"))))
            (apply author authors)))))
  
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
      ((curry filter-not void?)
       (list (struct-copy part (apply bibliography #:tag "handbook::bibliography" (append entries bibentries))
                          [title-content (list "参考文献")])
             (unless (false? index?)
               (struct-copy part (index-section #:tag "handbook::index")
                            [title-content (list "关键词索引")])))))))
