#lang racket

(require "message.rkt")

(require racket/tcp)

(define memory (make-bytes 1000))
(define master-ipv4 (vector-ref (current-command-line-arguments) 0))
(define master-port (with-handlers ([exn:fail? (λ [e] #false)]) (string->number (vector-ref (current-command-line-arguments) 1))))

(define powers (list 3 14 24 35))
(define speeds (list 4 15 25 36))
(define currents (list 5 16 26 37))
(define voltages (list 6 17 27 38 73 74 75 85))
(define temperatures (append (range 7 14) (range 18 24) (range 28 35) (range 39 46)))
(define percentages (list 117))

(define refresh-memory
  (lambda []
    ;;; DB28
    (for ([i (in-range 200)])
      (define hint (random 16))
      (when (< hint 8)
        (define state (arithmetic-shift #b1 hint))
        (bytes-set! memory i state)))
    
    ;;; DB4
    (for ([i (in-range 400)])
      (define line_no (add1 i))
      (define-values (fx0 fxn)
        (cond [(memv line_no powers) (values 1000 2000)]
              [(memv line_no voltages) (values 110 220)]
              [(memv line_no currents) (values 1 8)]
              [(memv line_no speeds) (values 1000 10000)]
              [(memv line_no percentages) (values 0 100)]
              [(memv line_no temperatures) (values 20 100)]
              [(<= 101 line_no 115) (values 0 80)]         ;;; air conditioner
              [(<= 75 line_no 94) (values 100 1000)]       ;;; generator metrics
              [else (values 0 #xFFFF)]))
      (define fx (random fx0 (add1 fxn)))
      (integer->integer-bytes fx 2 #false #true memory (+ 200 (* i 2))))))

(with-handlers ([exn:break? void])
  (let connect-send-wait-loop ()
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break master-ipv4 (or master-port 2000))])
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
