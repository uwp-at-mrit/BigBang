#lang racket

(provide (all-defined-out))

(require racket/date)

(define language-statistics
  (lambda [sln-root languages excludes]
    (define (use-dir? dir)
      (not (ormap (curryr regexp-match? dir)
                  (cons #px"/(.git|compiled)$" excludes))))
    (define statistics (make-hasheq))
    (for ([path (in-directory sln-root use-dir?)]
          #:when (file-exists? path))
      (define language
        (for/or ([l.px.clr (in-list languages)])
          (and (regexp-match? (vector-ref l.px.clr 1) path)
               (vector-ref l.px.clr 0))))
      (when (symbol? language)
        (hash-set! statistics language
                   (+ (hash-ref statistics language (λ [] 0))
                      (file-size path)))))
    statistics))

(define git-log
  (lambda []
    (define statistics (make-hasheq))
    (define git (find-executable-path "git"))
    (unless (not git)
      (define (stat++ ts tokens)
        (define the-day (seconds->date ts))
        (define midnight (find-seconds 0 0 0 (date-day the-day) (date-month the-day) (date-year the-day)))
        (define stat0 (hash-ref statistics midnight (λ [] (cons 0 0))))
        (define-values (insertion deletion)
          (cond [(= (length tokens) 4) (values (car tokens) (caddr tokens))]
                [(char-ci=? (string-ref (cadr tokens) 0) #\i) (values (car tokens) "0")]
                [else (values "0" (car tokens))]))
        (hash-set! statistics midnight
                   (cons (+ (car stat0) (string->number insertion))
                         (+ (cdr stat0) (string->number deletion)))))
      (parameterize ([current-custodian (make-custodian)])
        (define-values (/dev/gitin /dev/gitout) (make-pipe))
        (dynamic-wind
         (thunk (thread (thunk (parameterize ([current-output-port /dev/gitout])
                                 (system*/exit-code git "log" "--pretty=format:%at" "--shortstat")
                                 (close-output-port /dev/gitout)))))
         (thunk (with-handlers ([exn? void])
                  (let git-log-stat ([timestamp (read-line /dev/gitin)])
                    (cond [(string=? timestamp "") (git-log-stat (read-line /dev/gitin))]
                          [else (let ([tokens (string-split (read-line /dev/gitin))])
                                  (cond [(= (length tokens) 1) (git-log-stat (car tokens)) #|merged commits|#]
                                        [else (let ([skip-empty-line (read-line /dev/gitin)])
                                                (stat++ (string->number timestamp) (cdddr tokens))
                                                (git-log-stat (read-line /dev/gitin)))]))]))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    statistics))
