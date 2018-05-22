#lang racket

(provide (all-defined-out))

(define dictionary (make-hasheq))

(define name->sql
  (lambda [name]
    (hash-ref! dictionary name
               (thunk (cond [(eq? name '||) (symbol->string (gensym '_))]
                            [else (let* ([patterns '([#rx"(.*)\\?$" "is_\\1"]
                                                     [#rx"[^A-Za-z0-9_]" "_"])]
                                         [name (regexp-replaces (symbol->string name) patterns)])
                                    (if (string? name) name (bytes->string/utf-8 name)))])))))

(define type->sql
  (lambda [type]
    (case type
      [(Symbol String) "VARCHAR"]
      [(Natural Integer) "BIGINT"]
      [(Fixnum Index) "INTEGER"]
      [(Byte One Zero) "SMALLINT"]
      [(Flonum Float Real) "FLOAT"]
      [(Bytes) "BLOB"]
      [else (let ([r (string-downcase (symbol->string type))])
              (cond [(regexp-match? #px"integer" r) "BIGINT"]
                    [(regexp-match? #px"fixnum|index" r) "INTEGER"]
                    [(regexp-match? #px"flonum|float|inexact|real" r) "FLOAT"]
                    [(regexp-match? #px"rational" r) "NUMERIC"]
                    [else "VARCHAR"]))])))

(define id->sql
  (lambda [<id> [type 'name]]
    (define datum (syntax-e <id>))
    (datum->syntax <id> (case type
                          [(type) (type->sql datum)]
                          [(name) (name->sql datum)]
                          [else (symbol->string datum)]))))
