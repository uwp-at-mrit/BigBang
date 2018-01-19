#lang racket

(provide (all-defined-out))
(provide (all-from-out "bibliography.rkt"))
(provide (all-from-out "graphviz.rkt"))

(require (except-in scribble/core table))
(require scribble/manual)
(require scribble/html-properties)

(require "bibliography.rkt")
(require "graphviz.rkt")
(require "statistics.rkt")

(define preface-style (make-style 'index '(grouper unnumbered)))
(define file-color (make-style 'tt (list (make-color-property (list #x58 #x60 #x69)))))
(define insertion-color (make-style 'tt (list (make-color-property (list #x28 #xA7 #x45)))))
(define deletion-color (make-style 'tt (list (make-color-property (list #xCB #x24 #x31)))))

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

(define handbook-statistics
  (lambda [#:gitstat-width [git-width #false] #:gitstat-height [git-height #false] #:ignore [excludes null] . argl]
    (define languages (filter vector? argl))
    (when (pair? languages)
      (make-delayed-block
       (λ [render% pthis infobase]
         (define-values (lang-statistics file-count) (language-statistics (find-solution-root-dir) languages excludes))
         (define-values (loc-statistics insertions deletions) (git-numstat languages excludes))
         (define-values (lang-source loc-source)
           (for/fold ([lang-src null] [loc-src null])
                     ([l.px.clr (in-list (remove-duplicates languages eq? #:key (λ [l.px.clr] (vector-ref l.px.clr 0))))])
             (let ([lang (vector-ref l.px.clr 0)]
                   [color (let ([c (vector-ref l.px.clr 1)]) (if (symbol? c) (symbol->string c) c))])
               (values (cons (vector lang color (hash-ref lang-statistics lang (λ [] 0))) lang-src)
                       (cons (vector lang color (hash-ref loc-statistics lang (λ [] null))) loc-src)))))
         (define-values (language-pie loc-series)
           (let* ([pie-height (or git-height 200)]
                  [pie-width pie-height]
                  [series-height (or git-height 200)]
                  [series-width (or git-width (* series-height 2.4))])
             (values (pie-chart #:radian0 0.618 pie-width pie-height (reverse lang-source))
                     (git-loc-series series-width series-height (reverse loc-source)))))
         (nested (filebox (elem #:style file-color (~loc file-count) (superscript "files")
                                ~ (elem #:style insertion-color (~loc (apply + (hash-values insertions))) (superscript "++"))
                                ~ (elem #:style deletion-color (~loc (apply + (hash-values deletions))) (superscript (literal "--"))))
                          (tabular #:sep (hspace 1) #:column-properties '(left right)
                                   (list (list language-pie loc-series))))))))))

(define handbook-table
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
                                  #:url      "https://docs.racket-lang.org/scribble/index.html")
                       (bib-entry #:key      "LP:WEB"
                                  #:title    "Literate Programming"
                                  #:author   (authors "Donald E. Knuth")
                                  #:date     "1984"
                                  #:location (journal-location "The Computer Journal" #:number "10.1093/comjnl/27.2.97")
                                  #:url      "http://www.literateprogramming.com/knuthweb.pdf")
                       (bib-entry #:key      "LP:Issues"
                                  #:title    "Literate Programming - Issues and Problems"
                                  #:author   (authors "Kurt Nørmark")
                                  #:date     "1998"
                                  #:location (dissertation-location #:institution "Department of Computer Science Aalborg University" #:degree "Lektor")
                                  #:url      "http://people.cs.aau.dk/~normark/litpro/issues-and-problems.html"))])
    (lambda [#:index? [index? #true] . bibentries]
      ((curry filter-not void?)
       (list (struct-copy part (apply bibliography #:tag "handbook::bibliography" (append entries bibentries))
                          [title-content (list "参考文献")])
             (unless (false? index?)
               (struct-copy part (index-section #:tag "handbook::index")
                            [title-content (list "关键词索引")])))))))
