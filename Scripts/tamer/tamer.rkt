#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out rackunit))

(require rackunit)
(require syntax/location)

(require "format.rkt")

(require (for-syntax syntax/parse))

(define-syntax (define-tamer-suite stx)
  (syntax-parse stx
    [(_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) units ...)
     #`(tamer-record-story 'varid (test-suite name
                                              #:before #,(or (attribute setup) #'void)
                                              #:after #,(or (attribute teardown) #'void)
                                              units ...))]))

(define-syntax (define-tamer-case stx)
  (syntax-parse stx
    [(_ varid name bodys ...)
     #'(tamer-record-story 'varid (delay-test (test-spec name bodys ...)))]))

(define-syntax (test-spec stx)
  (syntax-parse stx
    [(_ name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...)
     #`(test-case name (around (#,(or (attribute setup) #'void))
                               checks ...
                               (#,(or (attribute teardown) #'void))))]))

(define-syntax (tamer-taming-start stx)
  (syntax-case stx [scribble +]
    [(_ scribble)
     #'(let ([modpath (quote-module-path)])
         (tamer-story (tamer-story->modpath (if (path? modpath) modpath (cadr modpath)))))]
    [(_)
     #'(begin (tamer-taming-start scribble)
              (module+ main (void (tamer-prove))))]))

(define tamer-prove
  (lambda []
    (define suite (if (module-path? (tamer-story))
                      (let ([htag (tamer-story->tag (tamer-story))])
                        (and (dynamic-require (tamer-story) #false)
                             (hash-has-key? handbook-stories htag)
                             (make-test-suite htag (reverse (map cdr (hash-ref handbook-stories htag))))))
                      (or (zero? (hash-count handbook-stories)) ; no story ==> no :books:
                          (let ([href (curry hash-ref handbook-stories)])
                            (make-test-suite "Behaviors and Features"
                                             (for/list ([unit (in-list (reverse (href books#)))])
                                               (make-test-suite unit (reverse (map cdr (href unit))))))))))
    (or (and (test-suite? suite)
             (let-values ([(brief-box cpu0 real0 gc0) (time-apply prove (list suite))])
               (define-values (success failure error skip todo real cpu-gc gc cpu)
                 (apply values (list* (summary-success (car brief-box))
                                      (summary-failure (car brief-box))
                                      (summary-error (car brief-box))
                                      (summary-skip (car brief-box))
                                      (summary-todo (car brief-box))
                                      (map (compose1 (curry ~r #:precision '(= 3)) (curry * 0.001))
                                           (list real0 (- cpu0 gc0) gc0 cpu0)))))
               (define population (+ success failure error skip todo))
               (and (positive? population)
                    (let ([echo (curry echof #:fgcolor 'lightcyan)])
                      (echo "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)." real cpu-gc gc cpu)
                      (echo "~n~a, ~a, ~a, ~a, ~a, ~a% Okay.~n" @~n_w[population]{example} @~n_w[failure]{failure}
                            @~n_w[error]{error} @~n_w[skip]{skip} @~n_w[todo]{TODO}
                            (~r  #:precision '(= 2) (/ (* (+ success skip) 100) population)))
                      (+ failure error)))))
        (and (echof #:fgcolor 'darkcyan "~nNo particular example!~n") 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama racket
  (provide (all-defined-out))
  (provide (all-from-out "emoji.rkt"))
  (provide (all-from-out "echo.rkt"))
  
  (require rackunit)
  (require racket/undefined)
  (require syntax/location)
  
  (require "monad.rkt")
  (require "emoji.rkt")
  (require "echo.rkt")
  
  (define tamer-zone (make-parameter #false))

  (struct test-skip test-result (result #| : String |#))
  (struct test-todo test-result (result #| : String |#))

  (define skip
    (lambda [fmt . arglist]
      (raise (exn:fail:unsupported (apply format fmt arglist) (current-continuation-marks)))))

  (define todo
    (lambda [fmt . arglist]
      (raise (exn:fail:unsupported (apply format fmt arglist) (current-continuation-marks)))))
  
  (define tamer-story (make-parameter #false))

  (define-syntax (tamer-story->tag stx)
    (syntax-case stx []
      [(_ story-sexp)
       #'(let ([modpath (with-handlers ([exn? (const (quote-source-file))]) (cadr story-sexp))])
           (if (string? modpath) modpath (path->string modpath)))]))

  (define tamer-story->modpath
    (lambda [story-path]
      `(submod ,story-path story)))
  
  (struct summary (success failure error skip todo) #:prefab)

  (define summary++
    (lambda [summary0 result]
      (summary (+ (summary-success summary0) (if (test-success? result) 1 0))
               (+ (summary-failure summary0) (if (test-failure? result) 1 0))
               (+ (summary-error summary0) (if (test-error? result) 1 0))
               (+ (summary-skip summary0) (if (test-skip? result) 1 0))
               (+ (summary-todo summary0) (if (test-todo? result) 1 0)))))
  
  ;;; Tamer Monad
  (struct tamer-seed (datum brief namepath exns) #:mutable)

  (define make-tamer-monad
    (lambda []
      (monad (void) (tamer-seed (void) (summary 0 0 0 0 0) null null))))

  (define (monad-return value)
    (lambda [tamer-monad]
      (set-monad-value! tamer-monad value)
      tamer-monad))

  (define (monad-put seed-set! val)
    (lambda [tamer-monad]
      (seed-set! (monad-state tamer-monad) val)
      tamer-monad))

  (define (monad-get seed-ref)
    (lambda [tamer-monad]
      (let ([val (seed-ref (monad-state tamer-monad))])
        (set-monad-value! tamer-monad val)
        tamer-monad)))
  ;;; End Tamer Monad

  (define-values (handbook-stories handbook-records) (values (make-hash) (make-hash)))

  (define tamer-record-story
    (lambda [name unit]
      (define htag (tamer-story->tag (tamer-story)))
      (define units (hash-ref handbook-stories htag null))
      (unless (dict-has-key? units name)
        (hash-set! handbook-stories htag (cons (cons name unit) units)))
      (let ([books (hash-ref handbook-stories books# null)])  ;;; Readme.md needs it stay here
        (unless (member htag books) (hash-set! handbook-stories books# (cons htag books))))))

  (define tamer-record-handbook
    (lambda [name:case«suites action]
      (define case-name (car name:case«suites))
      (hash-ref! (hash-ref! handbook-records (tamer-story) make-hash)
                 (string-join name:case«suites " « ")
                 (thunk (let/ec return
                          (parameterize ([current-error-port (open-output-string '/dev/case/stderr)]
                                         [exit-handler (λ [v] (let* ([errmsg (string-trim (get-output-string (current-error-port)))]
                                                                     [routine (thunk (with-check-info (('exitcode v)) (fail errmsg)))])
                                                                (return (run-test-case case-name routine))))])
                            (return (let ([result (run-test-case case-name action)])
                                      (cond [(and (test-error? result) (test-error-result result))
                                             => (λ [?] (cond [(false? (exn:fail:unsupported? ?)) result]
                                                             [(let ([stack (continuation-mark-set->context (exn-continuation-marks ?))])
                                                                (and (false? (null? stack)) (eq? (caar stack) 'todo)))
                                                              (test-todo case-name (exn-message ?))]
                                                             [else (test-skip case-name (exn-message ?))]))]
                                            [else result])))))))))

  (define ~result
    (lambda [result]
      (case (object-name result)
        [(test-error) (string bomb#)]
        [(test-success) (string green-heart#)]
        [(test-failure) (string broken-heart#)]
        [(test-skip) (string arrow-heart#)]
        [(test-todo) (string growing-heart#)])))

  (define ~fgcolor
    (lambda [result]
      (define rslt (if (string? result) result (~result result)))
      (cond [(string=? rslt (~result test-error)) 'darkred]
            [(string=? rslt (~result test-success)) 'lightgreen]
            [(string=? rslt (~result test-failure)) 'lightred]
            [(string=? rslt (~result test-skip)) 'lightblue]
            [(string=? rslt (~result test-todo)) 'lightmagenta])))

  (define exn->test-case
    (lambda [name e]
      (delay-test (test-case (format "(~a ⧴ ~a)" name (object-name e))
                             (raise e) #| no thunk, make test-error |#))))
  
  (define display-failure
    (lambda [result [color 'darkred] #:indent [headspace ""]]
      (define echo (curry eechof #:fgcolor color "~a»» ~a: ~s~n" headspace))
      (define recho (curry eechof #:fgcolor color "~a»»» ~a~a~n" headspace))
      (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
        (case (check-info-name info)
          [(params) (for ([param (in-list (check-info-value info))]
                          [index (in-naturals 1)])
                      (echo (format "param:~a" index) param))]
          [(message) (let ([messages (call-with-input-string (check-info-value info) port->lines)])
                       (echo "message" (car messages))
                       (for-each (curry recho (~a #:min-width 8)) (cdr messages)))]
          [else (echo (check-info-name info)
                      (case (check-info-name info)
                        [(location) (srcloc->string (apply srcloc (check-info-value info)))]
                        [(exception-message) (check-info-value info)]
                        [else (check-info-value info)]))]))))
  
  (define display-error
    (lambda [result [color 'darkred] #:indent [headspace0 ""]]
      (define errobj (test-error-result result))
      (define messages (call-with-input-string (exn-message errobj) port->lines))
      (eechof #:fgcolor color #:attributes '(inverse) "~a»» name: ~a~n" headspace0 (object-name errobj))
      (unless (null? messages)
        (define msghead " message: ")
        (define msgspace (~a #:min-width (sub1 (string-length msghead))))
        (eechof #:fgcolor color #:attributes '(inverse) "~a»»~a~a~n" headspace0 msghead (car messages))
        (for-each (curry eechof #:fgcolor color #:attributes '(inverse) "~a»»»~a~a~n" headspace0 msgspace) (cdr messages)))
      (for ([stack (in-list (continuation-mark-set->context (exn-continuation-marks errobj)))])
        (when (cdr stack)
          (define srcinfo (srcloc->string (cdr stack)))
          (unless (or (false? srcinfo) (regexp-match? #px"^/" srcinfo))
            (eechof #:fgcolor 'darkgrey "~a»»»» ~a: ~a~n" headspace0
                    srcinfo (or (car stack) 'λ)))))))

  (define display-skip
    (lambda [result [color 'darkblue] #:indent [headspace0 ""]]
      (define reason (test-skip-result result))
      (define messages (call-with-input-string reason port->lines))
      (unless (null? messages)
        (define msghead " SKIP: ")
        (define msgspace (~a #:min-width (sub1 (string-length msghead))))
        (eechof #:fgcolor color "~a»»~a~a~n" headspace0 msghead (car messages))
        (for-each (curry eechof #:fgcolor color "~a»»»~a~a~n" headspace0 msgspace) (cdr messages)))))

  (define display-todo
    (lambda [result [color 'darkmagenta] #:indent [headspace0 ""]]
      (define reason (test-todo-result result))
      (define messages (call-with-input-string reason port->lines))
      (unless (null? messages)
        (define msghead " TODO: ")
        (define msgspace (~a #:min-width (sub1 (string-length msghead))))
        (eechof #:fgcolor color "~a»»~a~a~n" headspace0 msghead (car messages))
        (for-each (curry eechof #:fgcolor color "~a»»»~a~a~n" headspace0 msgspace) (cdr messages)))))
  
  (define fold-test-suite
    (lambda [seed:datum testsuite #:fdown fdown #:fup fup #:fhere fhere]
      (parameterize ([current-custodian (make-custodian)]) ;;; Prevent test routines from shutting down the custodian by accident.
        (monad-value ((monad-get tamer-seed-brief)
                      (foldts-test-suite (λ [testsuite name pre-action post-action seed]
                                           (define $exn (make-parameter undefined))
                                           (with-handlers ([void $exn]) ;; catch all
                                             (call-with-values pre-action void))
                                           ((>=> (>>= (monad-get tamer-seed-datum)
                                                      (λ [seed:datum] (monad-put set-tamer-seed-datum! (fdown name seed:datum))))
                                                 (>>= (monad-get tamer-seed-namepath)
                                                      (λ [seed:namepath] (monad-put set-tamer-seed-namepath! (cons name seed:namepath))))
                                                 (>>= (monad-get tamer-seed-exns)
                                                      (λ [seed:exns] (monad-put set-tamer-seed-exns! (cons ($exn) seed:exns)))))
                                            seed))
                                         (λ [testsuite name pre-action post-action seed children-seed]
                                           (with-handlers ([exn? (compose1 display-error (curry make-test-error (format "#:after ~a" name)))])
                                             (call-with-values post-action void))
                                           ((>=> (>>= (monad-get tamer-seed-datum)
                                                      (λ [children:datum] (monad-put set-tamer-seed-datum! (fup name children:datum children:datum))))
                                                 (>>= (monad-get tamer-seed-namepath)
                                                      (λ [children:namepath] (monad-put set-tamer-seed-namepath! (cdr children:namepath))))
                                                 (>>= (monad-get tamer-seed-exns)
                                                      (λ [children:exns] (monad-put set-tamer-seed-exns! (cdr children:exns)))))
                                            children-seed #| monad is a stateful structure, so seed === children-seed |#))
                                         (λ [testcase name action seed]
                                           (define-values (fixed-name fixed-action)
                                             (cond [(findf (lambda [e] (not (eq? e undefined))) (monad-value ((monad-get tamer-seed-exns) seed)))
                                                    => (lambda [e] (values (format "#:before ~a" name)
                                                                           (thunk (raise e))))]
                                                   [(false? name)
                                                    (values (format "(⧴ ~a)" (object-name struct:exn:fail:user))
                                                            (thunk (raise-user-error "Unnamed Testcase!")))]
                                                   [else (values name action)]))
                                           (define fixed-namepath (cons fixed-name (monad-value ((monad-get tamer-seed-namepath) seed))))
                                           (define record (tamer-record-handbook fixed-namepath fixed-action))
                                           ((>=> (>>= (monad-get tamer-seed-datum)
                                                      (λ [seed:datum] (monad-put set-tamer-seed-datum! (fhere record seed:datum))))
                                                 (>>= (monad-get tamer-seed-brief)
                                                      (λ [seed:summary] (monad-put set-tamer-seed-brief! (summary++ seed:summary record))))
                                                 (monad-put set-tamer-seed-namepath! fixed-namepath))
                                            seed))
                                         ((monad-put set-tamer-seed-datum! seed:datum)
                                          (make-tamer-monad))
                                         testsuite))))))
  
  (define prove
    (lambda [unit]
      (fold-test-suite #:fdown (λ [name seed:ordered]
                                 (cond [(null? seed:ordered) (echof #:fgcolor 'darkgreen #:attributes '(dim underline) "λ ~a~n" name)]
                                       [else (echof #:fgcolor 'yellow
                                                    "~aλ~a ~a~n" (~a #:min-width (* (length seed:ordered) 2))
                                                    (string-join (map number->string (reverse seed:ordered)) ".") name)])
                                 (cons 1 seed:ordered))
                       #:fup   (λ [name maybe-children-if-monad children:ordered]
                                 (cond [(< (length children:ordered) 2) null]
                                       [else (cons (add1 (cadr children:ordered))
                                                   (cddr children:ordered))]))
                       #:fhere (λ [result seed:ordered]
                                 (define headline (format "~a~a  ~a - " (~a #:min-width (* (length seed:ordered) 2))
                                                          (~result result) (if (null? seed:ordered) 1 (car seed:ordered))))
                                 (define headspace (~a #:min-width (string-length headline)))
                                 (echof #:fgcolor (~fgcolor result) "~a~a~n" headline (test-result-test-case-name result))
                                 (cond [(test-success? result) (void)]
                                       [(test-failure? result) (display-failure result #:indent headspace)]
                                       [(test-error? result) (display-error result #:indent headspace)]
                                       [(test-skip? result) (display-skip result #:indent headspace)]
                                       [(test-todo? result) (display-todo result #:indent headspace)]
                                       [else (error "RackUnit has new test result type supported!")])
                                 (if (null? seed:ordered) null (cons (add1 (car seed:ordered)) (cdr seed:ordered))))
                       null ; seed:datum
                       unit))))

(require (submod "." digitama))
