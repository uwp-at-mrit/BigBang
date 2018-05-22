#lang racket

(provide (all-defined-out))

(define &linebreak
  (lambda [[count 1]]
    (let nbsp ([c count])
      (when (> c 0)
        (newline)
        (nbsp (sub1 c))))))

(define &hspace
  (lambda [[count 1]]
    (let hsp ([c count])
      (when (> c 0)
        (display #\space)
        (hsp (sub1 c))))))

(define &brace
  (lambda [indent #:semicolon? [semicolon? #false]]
    (&hspace (* indent 4))
    (display #\})

    (when (and semicolon?)
      (display #\;))
    (&linebreak)))

(define &include
  (lambda headers
    (for ([header (in-list headers)])
      (cond [(string? header) (printf "#include ~s~n" header)]
            [else (printf "#include <~a>~n" header)]))
    (&linebreak 1)))

(define &namespace
  (lambda [ns λbody]
    (printf "namespace ~a {~n" ns)
    (λbody 1)
    (&brace 0)))

(define &using-namespace
  (lambda namespaces
    (for ([ns (in-list namespaces)])
      (printf "using namespace ~a;~n" ns))
    (&linebreak 1)))

(define &struct
  (lambda [name fields types indent]
    (&hspace (* indent 4))
    (printf "private struct ~a {~n" name)

    (for ([field (in-list fields)]
          [type (in-list types)])
      (&hspace (* (add1 indent) 4))
      (printf "~a ~a;~n" type field))
    
    (&brace indent #:semicolon? #true)))
