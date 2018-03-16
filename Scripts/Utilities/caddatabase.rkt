#lang racket/gui

(require syntax/readerr)

(define cad-string->string
  (lambda [raw]
    (cond [(not (regexp-match? #px"\\\\U[+]" raw)) (substring raw 1 (- (string-length raw) 2))]
          [else (read (open-input-string (string-replace raw #px"\\\\U[+]" "\\U")))])))

(define read-log-line
  (lambda [/dev/stdin]
    (define line (read-line /dev/stdin 'any))
    (cond [(eof-object? line) (raise-read-eof-error "End of File" /dev/stdin #false #false #false #false)]
          [(not (regexp-match? #px"^Press ENTER to continue:\\s*$" line)) line]
          [else (read-log-line /dev/stdin)])))

(define read-log-line*
  (lambda [/dev/stdin]
    (define line (read-log-line /dev/stdin))
    (when (string-prefix? line "Command:")
      (raise-read-eof-error "End of Command" /dev/stdin #false #false #false #false))
    line))

(define read-log-line**
  (lambda [/dev/stdin [px.blank #px"^\\s*$"]]
    (define line (read-log-line* /dev/stdin))
    (cond [(regexp-match? px.blank line) #false]
          [else line])))

(define read-unused-object
  (lambda [/dev/stdin self]
    (let skip-property ()
      (when (read-log-line** /dev/stdin)
        (skip-property)))))

(define read-lwpolyline-object
  (lambda [/dev/stdin self]
    (let read-lwpolyline-property ()
      (when (read-log-line** /dev/stdin)
        (read-lwpolyline-property)))
    (let read-lwpolyline-point-property ()
      (when (read-log-line** /dev/stdin)
        (read-lwpolyline-point-property)))
    #;(let read-point ([points null])
      (define at-point (read-log-line** /dev/stdin))
      (cond [(not at-point) (hash-set! self 'points (reverse points))]
            [else (let ([xy (regexp-match #px"X=([-0-9.]+)\\s+Y=([-0-9.]+)" at-point)])
                    (read-point (cons (make-rectangular (string->number (cadr xy)) (string->number (caddr xy)))
                                      points)))]))))

(define read-spline-object
  (lambda [/dev/stdin self]
    (let read-spine-property ()
      (when (read-log-line** /dev/stdin #px"Number of control points")
        (read-spine-property)))
    (let read-control-point ([points null])
      (define point (read-log-line** /dev/stdin))
      (cond [(not point) (hash-set! self 'control-points (reverse points))]
            [else (let ([ptxy (regexp-match #px"X = ([-0-9.]+),\\s+Y = ([-0-9.]+)" point)])
                    (cond [ptxy (read-control-point (cons (make-rectangular (string->number (cadr ptxy))
                                                                            (string->number (caddr ptxy)))
                                                          points))]
                          [else (let read-control-point-property ()
                                  (cond [(read-log-line** /dev/stdin) (read-control-point-property)]
                                        [else (read-control-point points)]))]))]))))

(define read-dwg-log
  (lambda [dwg.log]
    (define cadbase (make-hasheq))
    (call-with-input-file* dwg.log
      (lambda [/dev/stdin]
        (with-handlers ([exn:fail:read:eof? void])
          (let skip-log ()
            (define line (read-log-line /dev/stdin))
            (cond [(not (string-prefix? line "Command: LIST")) (skip-log)]
                  [else (read-log-line /dev/stdin) (displayln line)]))
          (let read-object ()
            (define tokens (string-split (read-log-line* /dev/stdin) #px"\\s+Layer:\\s+"))
            (define typename (string->symbol (string-downcase (string-trim (car tokens)))))
            (define self (make-hasheq))
            (hash-set! cadbase typename (cons self (hash-ref cadbase typename (thunk null))))
            (case typename
              [(spline) (read-spline-object /dev/stdin self)]
              [(lwpolyline) (read-lwpolyline-object /dev/stdin self)]
              [(line arc circle ellipse) (read-unused-object /dev/stdin self)]
              [(text mtext leader) (read-unused-object /dev/stdin self)]
              [(|block reference| |attribute definition| |end sequence| attribute) (read-unused-object /dev/stdin self)]
              [else (raise-user-error 'read-object "unknown type: ~a" (string-trim (car tokens)))])
            (read-object)))))
    cadbase))

(module+ main
  (define argc (vector-length (current-command-line-arguments)))
  (when (= argc 0)
    (raise-user-error "Please tell me an AutoCAD log file with objects properties"))

  (define database (read-dwg-log (vector-ref (current-command-line-arguments) 0)))
  (length (hash-ref database 'spline)))
