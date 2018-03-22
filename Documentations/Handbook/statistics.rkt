#lang racket

(provide (all-defined-out))

(define git:%at "--pretty=format:%at")

(define detect-language
  (lambda [languages path]
    (for/or ([l.px.clr (in-list languages)])
      (and (regexp-match? (vector-ref l.px.clr 2) path)
           (vector-ref l.px.clr 0)))))

(define detect-midnight
  (lambda [ts]
    (define the-day (seconds->date ts))
    (- ts (+ #;(* (date-week-day the-day) 86400)
             (* (date-hour the-day) 3600)
             (* (date-minute the-day) 60)
             (date-second the-day)))))

(define language-statistics
  (lambda [sln-root languages excludes]
    (define &count (box 0))
    (define (use-dir? dir)
      (not (ormap (curryr regexp-match? dir)
                  (cons #px"/(.git|compiled)/?$" excludes))))
    (define statistics (make-hasheq))
    (for ([path (in-directory sln-root use-dir?)]
          #:when (file-exists? path))
      (define language (detect-language languages path))
      (when (symbol? language)
        (set-box! &count (add1 (unbox &count)))
        (hash-set! statistics language
                   (+ (hash-ref statistics language (λ [] 0))
                      (file-size path)))))
    (values statistics (unbox &count))))

(define git-numstat
  (lambda [languages excludes]
    (define statistics (make-hasheq))
    (define insertions (make-hasheq))
    (define deletions (make-hasheq))
    (define git (find-executable-path "git"))
    (unless (not git)
      (define (use-dir? dir) (not (ormap (curryr regexp-match? dir) excludes)))
      (define (stat++ ts stats)
        (define midnight (detect-midnight ts))
        (for ([this-stat (in-list stats)])
          (define path (caddr this-stat))
          (define language (detect-language languages path))
          (when (and (symbol? language) (use-dir? path))
            (define-values (insertion deletion) (values (car this-stat) (cadr this-stat)))
            (hash-set! insertions language (+ insertion (hash-ref insertions language (λ [] 0))))
            (hash-set! deletions language (+ deletion (hash-ref deletions language (λ [] 0))))
            (hash-set! statistics language
                       (let ([lang-stats (hash-ref statistics language (λ [] (list (cons midnight (cons 0 0)))))])
                         (if (= midnight (caar lang-stats))
                             (cons (cons midnight (cons (+ insertion (cadar lang-stats)) (+ deletion (cddar lang-stats))))
                                   (cdr lang-stats))
                             (cons (cons midnight (cons insertion deletion)) lang-stats)))))))
      (parameterize ([current-custodian (make-custodian)]
                     [current-subprocess-custodian-mode 'kill])
        (let-values ([(git-diff /dev/diffin _do _de) (subprocess #false #false #false 'new git "diff" "--numstat")]
                     [(/dev/spin) (open-input-string (format "~n"))]
                     [(git-log /dev/login _lo _le) (subprocess #false #false #false 'new git "log" git:%at "--numstat" "--no-merges")])
          (define /dev/gitin (input-port-append #true /dev/diffin /dev/spin /dev/login))
          (let pretty-numstat ([timestamp (number->string (current-seconds))])
            (when (string? timestamp)
              (let numstat ([stats null])
                (define +-path (read-line /dev/gitin))
                (define tokens (if (eof-object? +-path) null (string-split +-path)))
                (cond [(>= (length tokens) 3) ; e.g. `59 53 {src.ext => renamed.ext}`
                       (let ([insertion (string->number (car tokens))]
                             [deletion (string->number (cadr tokens))])
                         (numstat (if (and insertion deletion) (cons (list insertion deletion (caddr tokens)) stats) stats)))]
                      [else (stat++ (string->number timestamp) (reverse stats))
                            (pretty-numstat (read-line /dev/gitin))])))))
        (custodian-shutdown-all (current-custodian))))

    (values statistics insertions deletions)))
