#lang racket

(provide (all-defined-out))
(provide (all-from-out typed/racket/date))
(provide (all-from-out racket/format))
(provide (all-from-out racket/pretty))

(require racket/format)
(require racket/flonum)
(require racket/pretty)

(require typed/racket/date)

(define plural
  (lambda [n word]
    (define dict #hash(("story" . "stories") ("Story" . "Stories")))
    (cond [(= n 1) word]
          [else (hash-ref dict word (λ _ (string-append word "s")))])))

(define ~n_w
  (lambda [count word]
    (format "~a ~a" count (plural count word))))

(define ~w=n
  (lambda [count word]
    (format "~a=~a" (plural count word) count)))

(define ~hex
  (lambda [n]
    (~r n #:base 16)))

(define ~%
  (lambda [% #:precision [prcs '(= 2)]]
    (string-append (~r (fl* 100.0 %) #:precision prcs) "%")))

(define ~uptime
  (let ([~t (λ [n] (if (< n 10) (string-append "0" (number->string n)) (number->string n)))])
    (lambda [s]
      (let*-values ([(d s) (quotient/remainder s 86400)]
                    [(h s) (quotient/remainder s 3600)]
                    [(m s) (quotient/remainder s 60)])
        (cond [(zero? d) (format "~a:~a:~a" (~t h) (~t m) (~t s))]
              [else (format "~a+~a:~a:~a" d (~t h) (~t m) (~t s))])))))

(define ~gctime
  (lambda [ms]
    (let*-values ([(s ms) (quotient/remainder ms 1000)]
                  [(m s) (quotient/remainder s 60)])
      (define padding (cond [(< ms 10) "00"] [(< ms 100) "0"] [else ""]))
      (cond [(zero? m) (format "~a.~a~a" s padding ms)]
            [else (format "~a:~a.~a~a" m s padding ms)]))))
