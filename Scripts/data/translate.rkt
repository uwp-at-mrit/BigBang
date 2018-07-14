#lang racket

(provide (all-defined-out))

(struct enum (id value en-US zh-CN) #:transparent)

(define make-enum-class
  (lambda [classname data #:/dev/stdout [/dev/stdout (current-output-port)]]
    (fprintf /dev/stdout "#pragma once~n")
    
    (fprintf /dev/stdout "~nnamespace WarGrey::SCADA {~n")
    (fprintf /dev/stdout "    private enum class ~a {~n" classname)
    
    (for ([e (in-list data)])
      (fprintf /dev/stdout "        ~a = ~a, /* ~a(~a) */~n" (enum-id e) (enum-value e) (enum-en-US e) (enum-zh-CN e)))

    (fprintf /dev/stdout "    };~n")
    (fprintf /dev/stdout "}~n~n")))
