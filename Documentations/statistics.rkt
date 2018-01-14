#lang racket

(provide (all-defined-out))

(require racket/date)

(define detect-language
  (lambda [languages path]
    (for/or ([l.px.clr (in-list languages)])
      (and (regexp-match? (vector-ref l.px.clr 1) path)
           (vector-ref l.px.clr 0)))))

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
    (define git (find-executable-path "git"))
    (with-handlers ([exn? void])
      (define (stat++ ts tokens)
        (define the-day (seconds->date ts))
        (define midnight (find-seconds 0 0 0 (date-day the-day) (date-month the-day) (date-year the-day)))
        (define-values (insertion deletion)
          (cond [(= (length tokens) 4) (values (car tokens) (caddr tokens))]
                [(char-ci=? (string-ref (cadr tokens) 0) #\i) (values (car tokens) "0")]
                [else (values "0" (car tokens))]))
        (hash-set! statistics midnight
                   (let ([stat0 (hash-ref statistics midnight (λ [] (cons 0 0)))])
                     (cons (+ (car stat0) (string->number insertion))
                           (+ (cdr stat0) (string->number deletion))))))
      (parameterize ([current-custodian (make-custodian)]
                     [current-subprocess-custodian-mode 'kill])
        (define-values (git-log /dev/gitin _out _err)
          (subprocess #false #false #false 'new
                      git "log" "--pretty=format:%at" "--shortstat"))
        (with-handlers ([exn? void])
          (let shortstat ([timestamp (read-line /dev/gitin)])
            (define tokens (string-split (read-line /dev/gitin)))
            (cond [(= (length tokens) 1) (shortstat (car tokens)) #|merged commits|#]
                  [else (let ([skip-empty-line (read-line /dev/gitin)])
                          (stat++ (string->number timestamp) (cdddr tokens))
                          (shortstat (read-line /dev/gitin)))])))
        (custodian-shutdown-all (current-custodian))))
    statistics))

(define git-log-numstat
  (lambda [languages excludes]
    (define statistics (make-hasheq))
    (define git (find-executable-path "git"))
    (with-handlers ([exn? void])
      (define (use-dir? dir) (not (ormap (curryr regexp-match? dir) excludes)))
      (define (stat++ ts stats)
        (define the-day (seconds->date ts))
        (define midnight (find-seconds 0 0 0 (date-day the-day) (date-month the-day) (date-year the-day)))
        (for ([stat (in-list stats)])
          (define path (caddr stat))
          (when (use-dir? path)
            (define language (detect-language languages path))
            (when (symbol? language)
              (define lang-stats (hash-ref! statistics midnight (λ [] (make-hasheq))))
              (hash-set! lang-stats language
                         (let ([stat0 (hash-ref lang-stats language (λ [] (cons 0 0)))])
                           (cons (+ (car stat0) (car stat))
                                 (+ (cdr stat0) (cadr stat)))))))))
      (parameterize ([current-custodian (make-custodian)]
                     [current-subprocess-custodian-mode 'kill])
        (define-values (git-log /dev/gitin _out _err)
          (subprocess #false #false #false 'new
                      git "log" "--pretty=format:%at" "--numstat"))
        (with-handlers ([exn? void])
          (let pretty-numstat ([timestamp (read-line /dev/gitin)])
            (define tokens (string-split (read-line /dev/gitin)))
            (let numstat ([stats null])
              (define tokens (string-split (read-line /dev/gitin)))
              (case (length tokens)
                [(1) (pretty-numstat (car tokens)) #|merged commits|#]
                [(3) (let ([stat (list (string->number (car tokens)) (string->number (cadr tokens)) (caddr tokens))])
                       (numstat (cond [(and (car stat) (cadr stat)) (cons stat stats)]
                                      [else stats])))]
                [else (stat++ (string->number timestamp) (reverse stats))
                      (pretty-numstat (read-line /dev/gitin))]))))
        (custodian-shutdown-all (current-custodian))))
    statistics))
