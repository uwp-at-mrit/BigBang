#lang racket

(provide (all-defined-out))

(require "virtual-sql.rkt")
(require "syntax.rkt")
(require "shadow.rkt")
(require "cpp.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))

(define-syntax (define-table stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ table #:as Table #:with primary-key ([field : DataType constraints ...] ...))
     (with-syntax* ([(rowid ...) (parse-primary-key #'primary-key)]
                    [([view? table-rowid ...]
                      [(:field table-field MaybeType on-update [defval ...] field-examples field-guard not-null unique) ...]
                      [cat-table.hpp cat-table.cpp table.hpp table.cpp table-columns force-create force-insert check-record]
                      [create-table insert-table delete-table update-table in-table select-table seek-table])
                     (let ([pkids (let ([pk (syntax->datum #'primary-key)]) (if (list? pk) pk (list pk)))]
                           [tablename (syntax-e #'table)])
                       (define-values (sdleif sdiwor)
                         (for/fold ([sdleif null] [sdiwor null])
                                   ([stx (in-syntax #'([field DataType constraints ...] ...))])
                           (define-values (maybe-pkref field-info) (parse-field-definition tablename pkids stx))
                           (values (cons field-info sdleif) (if maybe-pkref (cons maybe-pkref sdiwor) sdiwor))))
                       (list (cons (< (length sdiwor) (length pkids)) (reverse sdiwor))
                             (reverse sdleif)
                             (for/list ([fmt (in-list (list "cat-~a.hpp" "cat-~a.cpp" "~a.hpp" "~a.cpp" "~a_columns"
                                                            "create-~a-if-not-exists" "insert-~a-or-replace" "check-~a-rowid"))])
                               (format-id #'table fmt tablename))
                             (for/list ([prefix (in-list (list 'create 'insert 'delete 'update 'in 'select 'seek))])
                               (format-id #'table "~a_~a" prefix tablename))))])
       #'(begin (define cat-table.hpp
                  (lambda [[/dev/stdout (current-output-port)]]
                    (parameterize ([current-output-port /dev/stdout])
                      (&pragma 'once)
                      (&include 'optional)
                      (&include "dbsystem.hpp")

                      (&namespace 'WarGrey::SCADA
                                  (λ [indent]
                                    (&struct 'Table '(field ...) '(MaybeType ...) indent)
                                    (&create-table.hpp 'create-table 'table indent))))))

                (define cat-table.cpp
                  (lambda [[/dev/stdout (current-output-port)]]
                    (parameterize ([current-output-port /dev/stdout])
                      (&include (symbol->string 'table.hpp))
                      (&include "dbsystem.hpp" "dbtypes.hpp")
                      (&using-namespace 'WarGrey::SCADA)
                      (&table-column-info 'table-columns '(rowid ...) '(field ...) '(DataType ...) '(not-null ...) '(unique ...))
                      (&create-table.cpp 'create-table 'table-columns '(rowid ...)))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define-table event #:as AlarmEvent #:with [uuid name]
    ([uuid     : Integer           #:default (pk64:timestamp launch-time)]
     [type     : Text              #:default 'table #:not-null #:% 'table]
     [name     : Text              #:not-null #:unique]
     [ctime    : Integer           #:default (current-milliseconds)]
     [mtime    : Integer           #:auto (current-milliseconds)]))

  (printf "> cat ~a~n" 'event.hpp)
  (cat-event.hpp)
  
  (&linebreak 3)
  
  (printf "> cat ~a~n" 'event.cpp)
  (cat-event.cpp))
