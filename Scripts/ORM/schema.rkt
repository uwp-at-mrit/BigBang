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
    [(_ table #:as Table #:with primary-key ([field : DataType constraints ...] ...)
        (~or (~optional (~seq #:include addition-hpps) #:name "#:include" #:defaults ([addition-hpps #'[]]))) ...)
     (with-syntax* ([(rowid ...) (parse-primary-key #'primary-key)]
                    [Table_pk (format-id #'Table "~a_pk" (syntax-e #'Table))]
                    [([view? RowidType ...]
                      [(MaybeType on-update [defval ...] not-null unique) ...]
                      [cat-table.hpp cat-table.cpp table.hpp table.cpp table-rowids table-columns]
                      [create-table insert-table delete-table update-table select-table seek-table drop-table restore-table])
                     (let ([pkids (let ([pk (syntax->datum #'primary-key)]) (if (list? pk) pk (list pk)))]
                           [tablename (syntax-e #'table)])
                       (define-values (sdleif sdiwor)
                         (for/fold ([sdleif null] [sdiwor null])
                                   ([stx (in-syntax #'([field DataType constraints ...] ...))])
                           (define-values (maybe-pktype field-info) (parse-field-definition tablename pkids stx))
                           (values (cons field-info sdleif) (if maybe-pktype (cons maybe-pktype sdiwor) sdiwor))))
                       (list (cons (< (length sdiwor) (length pkids)) (reverse sdiwor))
                             (reverse sdleif)
                             (for/list ([fmt (in-list (list "cat-~a.hpp" "cat-~a.cpp" "~a.hpp" "~a.cpp" "~a_rowids" "~a_columns"))])
                               (format-id #'table fmt tablename))
                             (for/list ([prefix (in-list (list 'create 'insert 'delete 'update 'select 'seek 'drop 'restore))])
                               (format-id #'table "~a_~a" prefix tablename))))]
                    [([header ...] ...) #'addition-hpps])
       #'(begin (define cat-table.hpp
                  (lambda [[/dev/stdout (current-output-port)]]
                    (parameterize ([current-output-port /dev/stdout])
                      (&pragma 'once)
                      (&include 'list 'optional)
                      (&include "dbsystem.hpp")
                      (&include 'header ...) ...

                      (&namespace 'WarGrey::SCADA
                                  (λ [indent]
                                    (&primary-key 'Table_pk '(rowid ...) '(RowidType ...) indent)
                                    (&struct 'Table '(field ...) '(MaybeType ...) indent)
                                    (&restore-table 'restore-table 'Table indent)
                                    (&create-table 'create-table indent)
                                    (&insert-table 'insert-table 'Table indent)
                                    (&select-table 'select-table 'Table indent)
                                    (&drop-table 'drop-table indent))))))

                (define cat-table.cpp
                  (lambda [[/dev/stdout (current-output-port)]]
                    (parameterize ([current-output-port /dev/stdout])
                      (&include (symbol->string 'table.hpp))
                      (&include "dbsystem.hpp" "dbtypes.hpp")
                      (&using-namespace 'WarGrey::SCADA)
                      (&table-column-info 'table-columns 'table-rowids '(rowid ...) '(field ...) '(DataType ...) '(not-null ...) '(unique ...))
                      (&restore-table 'restore-table 'Table '(field ...) '(DataType ...) '(not-null ...) '(rowid ...))
                      (&create-table 'create-table 'table 'table-columns 'table-rowids)
                      (&insert-table 'insert-table 'Table 'table '(field ...) 'table-columns)
                      (&select-table 'select-table 'Table 'table 'restore-table 'table-columns)
                      (&drop-table 'drop-table 'table 'table-columns))))))]))

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
