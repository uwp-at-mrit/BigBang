#lang racket

(provide (all-defined-out))
(provide (all-from-out "bibliography.rkt"))
(provide (all-from-out "graphviz.rkt"))

(require scribble/core)
(require scribble/manual)
(require scribble/html-properties)

(require "graphviz.rkt")
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

(define handbook-statistics
  (lambda [#:ignore [excludes null] #:width [width #false] #:height [height #false] . argl]
    (define languages (filter vector? argl))
    (when (pair? languages)
      (make-delayed-block
       (λ [render% pthis infobase]
         (parameterize ([sln-root (find-solution-root-dir)])
           (define statistics (language-statistics languages excludes))
           (nested #:style (make-style "boxed" null)
                   (filebox (literal (path->string (sln-root)))
                            (let* ([pie-width (or width 256)]
                                   [pie-height (or height (* pie-width 0.618))])
                              (pie-chart pie-width pie-height
                                         (for/list ([l.px.clr (in-list languages)])
                                           (let ([language (vector-ref l.px.clr 0)])
                                             (vector language
                                                     (lang-stat-bytes (hash-ref statistics language (λ [] lang-stat-identity)))
                                                     (vector-ref l.px.clr 2))))))
                            (tabular #:style 'boxed #:column-properties '(left left right)
                                     (for/list ([l.px.color (in-list languages)])
                                       (define language (vector-ref l.px.color 0))
                                       (define stat (hash-ref statistics language (λ [] lang-stat-identity)))
                                       (list (racket #,language)
                                             (racket #,(lang-stat-lines stat))
                                             (racket #,(lang-stat-bytes stat)))))))))))))

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
                                  #:url      "https://docs.racket-lang.org/scribble/index.html"))])
    (lambda [#:index? [index? #true] . bibentries]
      ((curry filter-not void?)
       (list (struct-copy part (apply bibliography #:tag "handbook::bibliography" (append entries bibentries))
                          [title-content (list "参考文献")])
             (unless (false? index?)
               (struct-copy part (index-section #:tag "handbook::index")
                            [title-content (list "关键词索引")])))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct lang-stat (lines bytes) #:prefab)
(define lang-stat-identity (lang-stat 0 0))

(define lang-stat++
  (lambda [s0 sr]
    (lang-stat (+ (lang-stat-lines s0) (lang-stat-lines sr))
               (+ (lang-stat-bytes s0) (lang-stat-bytes sr)))))

(define language-statistics
  (lambda [languages excludes]
    (define (use-dir? dir)
      (not (ormap (curryr regexp-match? dir)
                  (cons #px"/(.git|compiled)$" excludes))))
    (parameterize ([sln-root (find-solution-root-dir)])
      (define statistics (make-hasheq))
      (for ([path (in-directory (find-solution-root-dir) use-dir?)]
            #:when (file-exists? path))
        (define language
          (for/or ([l.px.clr (in-list languages)])
            (and (regexp-match? (vector-ref l.px.clr 1) path)
                 (vector-ref l.px.clr 0))))
        (when (symbol? language)
          (define /dev/srcin (open-input-file path))
          (port-count-lines! /dev/srcin)
          (define-values (line col position)
            (let wc ()
              (cond [(string? (read-line /dev/srcin)) (wc)]
                    [else (port-next-location /dev/srcin)])))
          (close-input-port /dev/srcin)
          (hash-set! statistics language
                     (lang-stat++ (hash-ref statistics language (λ [] lang-stat-identity))
                                  (lang-stat line (file-size path))))))
      statistics)))
