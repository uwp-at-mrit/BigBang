#lang racket

(require "message.rkt")
(require "../catalogue/tongue.rkt")
(require "../../ch6000m3/stone/tongue/alarm.resw.rkt")

(require racket/tcp)
(require syntax/location)

(define memory (make-bytes #x1264))
(define master-ipv4 (vector-ref (current-command-line-arguments) 0))

(define db4 4122)
(define db205 4322)

(define set-digital-input
  (case-lambda
    [(offset index)
     (define-values (q r) (quotient/remainder (- index 1) 8))
     (set-digital-input offset q r)]
    [(offset index bindex)
     (bytes-set! memory (+ offset index)
                 (bitwise-ior (bytes-ref memory (+ offset index))
                              (arithmetic-shift #x1 bindex)))]))

(define clear-digital-input
  (case-lambda
    [(offset index)
     (define-values (q r) (quotient/remainder (- index 1) 8))
     (clear-digital-input offset q r)]
    [(offset index bindex)
     (bytes-set! memory (+ offset index)
                 (bitwise-and (bytes-ref memory (+ offset index))
                              (bitwise-not (arithmetic-shift #x1 bindex))))]))

(define refresh-memory
  (lambda [alarms4 alarms205]
    (bytes-fill! memory 0)

    ;;; DB2
    (for ([i (in-range 1 176)]) ;; don't change the tidemark
      (real->floating-point-bytes (+ 2.0 (random)) 4 #true memory (+ 3418 (* i 4))))
    
    ;;; DB4
    (for ([i (in-range 124)])
      (unless (< 40 i 50) ;; winches and gantries
        (define state (arithmetic-shift #x1 (random 8)))
        (bytes-set! memory (+ db4 i) state)))

    ;; gantries
    (set-digital-input db4 42 4)
    (set-digital-input db4 48 5)
    
    ;; alarms
    (for ([idx (in-list alarms4)])
      (clear-digital-input db4 (add1 idx)))

    (set-digital-input db4 (add1 (car alarms4)))
    
    ;;; DB203
    (for ([i (in-range 280)])
      (real->floating-point-bytes (+ 203.0 (random)) 4 #true memory (+ 1120 (* i 4))))

    ;;; DB205
    ;; gantries, ps trunnion - sb draghead
    (for ([dbx (in-range 163 169)])
      (set-digital-input db205 dbx (random 3)))
    
    ;; winches
    (for ([dbx (in-range 169 174)])
      (set-digital-input db205 dbx (random 8)))
    (set-digital-input db205 174 4)
    (set-digital-input db205 174 5)
    
    ;; alarms
    (for ([idx (in-list alarms205)])
      (clear-digital-input db205 (add1 idx)))))

(define-values (alarms4 alarms205)
  (let-values ([(classname data) (alarm-tongues)])
    (for/fold ([a4 null] [a205 null])
              ([tongue-index (map tongue-index data)])
      (define-values (db idx) (alarm-db-index tongue-index))
      (cond [(= db 4) (values (cons idx a4) a205)]
            [else (values a4 (cons idx a205))]))))

(with-handlers ([exn:break? void])
  (let connect-send-wait-loop ()
    (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
      (parameterize ([current-custodian (make-custodian)])
        (dynamic-wind
         (thunk (void))
         (thunk (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break master-ipv4 2008)])
                  (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                  (printf "[connected to ~a:~a]~n" remote rport)

                  (let wait-read-response-loop ()
                    (define-values (signature data) (read-mrmsg /dev/tcpin 40))
                    (define-values (db addr0 addrn) (values (mrmsg-block signature) (mrmsg-addr0 signature) (mrmsg-addrn signature)))

                    (case (mrmsg-code signature)
                      [(#x41) (refresh-memory alarms4 alarms205)
                              (printf ">> [sent ~a bytes to ~a:~a]~n"
                                      (write-mrmsg /dev/tcpout (mrmsg-code signature) (mrmsg-block signature) addr0 addrn memory)
                                      remote rport)]
                      [else (void)])
                    (wait-read-response-loop))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))
