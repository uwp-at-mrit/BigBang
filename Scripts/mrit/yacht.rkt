#lang racket

(require "message.rkt")

(require racket/tcp)

(define memory (make-bytes 1000))
(define master-ipv4 (vector-ref (current-command-line-arguments) 0))

(define refresh-memory
  (lambda []
    ;;; DB28
    (for ([i (in-range 200)])
      (define state (arithmetic-shift #x1 (random 8)))
      (bytes-set! memory i state))
    
    ;;; DB4
    (for ([i (in-range 400)])
      (define fx (random 0 (add1 #xFFFF)))
      (integer->integer-bytes fx 2 #false #true memory (+ 200 (* i 2))))))

(with-handlers ([exn:break? void])
  (let connect-send-wait-loop ()
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break master-ipv4 2000)])
                (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                (printf "[connected to ~a:~a]~n" remote rport)
                
                (let sleep-push-loop ()
                  (refresh-memory)
                  
                  (printf ">> [sent ~a bytes to ~a:~a]~n" (write-old-mrmsg /dev/tcpout #x31 memory) remote rport)
                  
                  (sleep 1)
                  (sleep-push-loop))))
       (thunk (custodian-shutdown-all (current-custodian)))))
  
    (sleep 1)
    (connect-send-wait-loop)))
