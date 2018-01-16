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
    (- ts (+ ;(* (date-week-day the-day) 86400)
             (* (date-hour the-day) 3600)
             (* (date-minute the-day) 60)
             (date-second the-day)))))

(define language-statistics
  (lambda [sln-root languages excludes]
    (define (use-dir? dir)
      (not (ormap (curryr regexp-match? dir)
                  (cons #px"/(.git|compiled)/?$" excludes))))
    (define statistics (make-hasheq))
    (for ([path (in-directory sln-root use-dir?)]
          #:when (file-exists? path))
      (define language (detect-language languages path))
      (when (symbol? language)
        (hash-set! statistics language
                   (+ (hash-ref statistics language (λ [] 0))
                      (file-size path)))))
    statistics))

(define git-log-shortstat
  (lambda []
    (define statistics (make-hasheq))
    (define-values (&insertion &deletion) (values (box 0) (box 0)))
    (define git (find-executable-path "git"))
    (with-handlers ([exn? void])
      (define (stat++ ts tokens)
        (define midnight (detect-midnight ts))
        (define-values ($insertion $deletion)
          (cond [(= (length tokens) 4) (values (car tokens) (caddr tokens))]
                [(char-ci=? (string-ref (cadr tokens) 0) #\i) (values (car tokens) "0")]
                [else (values "0" (car tokens))]))
        (define-values (insertion deletion) (values (string->number $insertion) (string->number $deletion)))
        (set-box! &insertion (+ insertion (unbox &insertion)))
        (set-box! &deletion (+ deletion (unbox &deletion)))
        (hash-set! statistics midnight
                   (let ([stat0 (hash-ref statistics midnight (λ [] (cons 0 0)))])
                     (cons (+ (car stat0) insertion) (+ (cdr stat0) deletion)))))
      (parameterize ([current-custodian (make-custodian)]
                     [current-subprocess-custodian-mode 'kill])
        (let-values ([(git-log /dev/login _lo _le) (subprocess #false #false #false 'new git "log" git:%at "--shortstat" "--no-merges")]
                     [(/dev/datin) (open-input-string (format "~n~a~n" (current-seconds)))]
                     [(git-diff /dev/diffin _do _de) (subprocess #false #false #false 'new git "diff" "--shortstat")])
          (define /dev/gitin (input-port-append #true /dev/login /dev/datin /dev/diffin))
          (let shortstat ()
            (define timestamp (read-line /dev/gitin))
            (when (string? timestamp)
              (define stat-line (read-line /dev/gitin))
              (define tokens (if (eof-object? stat-line) null (string-split stat-line)))
              (when (pair? tokens)
                (read-line /dev/gitin) ; skip-empty-line 
                (stat++ (string->number timestamp) (cdddr tokens))
                (shortstat)))))
        (custodian-shutdown-all (current-custodian))))

    (values statistics (unbox &insertion) (unbox &deletion))))

(git-log-shortstat)

(define git-log-numstat
  (lambda [languages excludes]
    (define statistics (make-hasheq))
    (define insertions (make-hasheq))
    (define deletions (make-hasheq))
    (define git (find-executable-path "git"))
    (with-handlers ([exn? void])
      (define (use-dir? dir) (not (ormap (curryr regexp-match? dir) excludes)))
      (define (stat++ ts stats)
        (define midnight (detect-midnight ts))
        (for ([stat (in-list stats)])
          (define path (caddr stat))
          (define language (detect-language languages path))
          (when (and (symbol? language) (use-dir? path))
            (define lang-stats (hash-ref! statistics language (λ [] (make-hasheq))))
            (define-values (insertion deletion) (values (car stat) (cadr stat)))
            (hash-set! insertions language (+ insertion (hash-ref insertions language (λ [] 0))))
            (hash-set! deletions language (+ deletion (hash-ref deletions language (λ [] 0))))
            (hash-set! lang-stats midnight
                       (let ([stat0 (hash-ref lang-stats midnight (λ [] (cons 0 0)))])
                         (cons (+ (car stat0) insertion) (+ (cdr stat0) deletion)))))))
      (parameterize ([current-custodian (make-custodian)]
                     [current-subprocess-custodian-mode 'kill])
        (let-values ([(git-log /dev/login _lo _le) (subprocess #false #false #false 'new git "log" git:%at "--numstat" "--no-merges")]
                     [(/dev/datin) (open-input-string (format "~n~a~n" (current-seconds)))]
                     [(git-diff /dev/diffin _do _de) (subprocess #false #false #false 'new git "diff" "--numstat")])
          (define /dev/gitin (input-port-append #true /dev/login /dev/datin /dev/diffin))
          (let pretty-numstat ()
            (define timestamp (read-line /dev/gitin))
            (let numstat ([stats null])
              (define +-path (read-line /dev/gitin))
              (define tokens (if (eof-object? +-path) null (string-split +-path)))
              (cond [(= (length tokens) 3)
                     (let ([insertion (string->number (car tokens))]
                           [deletion (string->number (cadr tokens))])
                       (numstat (cond [(and insertion deletion) (cons (list insertion deletion (caddr tokens)) stats)]
                                      [else stats])))]
                    [else (stat++ (string->number timestamp) (reverse stats))
                          (pretty-numstat)]))))
        (custodian-shutdown-all (current-custodian))))

    (values statistics insertions deletions)))
