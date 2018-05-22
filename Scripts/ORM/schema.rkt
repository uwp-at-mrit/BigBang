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
    [(_ tbl #:as Table #:with primary-key ([field : DataType constraints ...] ...))
     (with-syntax* ([([table dbtable] #%Table) (list (parse-table-name #'tbl) (format-id #'Table "#%~a" #'Table))]
                    [([rowid dbrowid] ...) (parse-primary-key #'primary-key)]
                    [([view? table-rowid ...]
                      [(:field table-field FieldType MaybeNull on-update [defval ...] field-examples dbfield DBType field-guard not-null unique) ...]
                      [touch-table.hpp touch-table.cpp table.hpp table.cpp force-create force-insert check-record]
                      [unsafe-table make-table remake-table create-table insert-table delete-table update-table in-table select-table seek-table])
                     (let ([pkids (let ([pk (syntax->datum #'primary-key)]) (if (list? pk) pk (list pk)))]
                           [tablename (syntax-e #'table)])
                       (define-values (sdleif sdiwor)
                         (for/fold ([sdleif null] [sdiwor null])
                                   ([stx (in-syntax #'([field DataType constraints ...] ...))])
                           (define-values (maybe-pkref field-info) (parse-field-definition tablename pkids stx))
                           (values (cons field-info sdleif) (if maybe-pkref (cons maybe-pkref sdiwor) sdiwor))))
                       (list (cons (< (length sdiwor) (length pkids)) (reverse sdiwor))
                             (reverse sdleif)
                             (for/list ([fmt (in-list (list "touch-~a.hpp" "touch-~a.cpp" "~a.hpp" "~a.cpp"
                                                            "create-~a-if-not-exists" "insert-~a-or-replace" "check-~a-rowid"))])
                               (format-id #'table fmt tablename))
                             (for/list ([prefix (in-list (list 'unsafe 'make 'remake 'create 'insert 'delete 'update 'in 'select 'seek))])
                               (format-id #'table "~a-~a" prefix tablename))))]
                    [([mkargs ...] [reargs ...])
                     (for/fold ([syns (list null null)])
                               ([:fld (in-syntax #'(:field ...))]
                                [mkarg (in-syntax #'([field : (U FieldType MaybeNull) defval ...] ...))]
                                [rearg (in-syntax #'([field : (U FieldType MaybeNull Void) on-update] ...))])
                       (list (cons :fld (cons mkarg (car syns)))
                             (cons :fld (cons rearg (cadr syns)))))]
                    [table-rowid-body (if (syntax-e #'view?)
                                          #'(throw exn:fail:unsupported '#%table "temporary view has no primary keys")
                                          #'(vector (racket->sql-pk (table-rowid self)) ...))])
       #'(begin (define touch-table.hpp
                  (lambda [[/dev/stdout (current-output-port)]]
                    (parameterize ([current-output-port /dev/stdout])
                      (&include "sqlite3.hpp")

                      (&namespace 'WarGrey::SCADA
                                  (λ [indent]
                                    (&struct 'Table '(field ...) '(DBType ...) indent))))))

                (define touch-table.cpp
                  (lambda [[/dev/stdout (current-output-port)]]
                    (parameterize ([current-output-port /dev/stdout])
                      (&include (symbol->string 'table.hpp))
                      (&using-namespace 'WarGrey::SCADA))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define-table master #:as Master #:with [uuid name]
    ([uuid     : Integer       #:default (pk64:timestamp launch-time)]
     [type     : Symbol        #:default 'table #:not-null #:% 'table]
     [name     : String        #:not-null #:unique]
     [ctime    : Fixnum        #:default (current-milliseconds)]
     [mtime    : Fixnum        #:auto (current-milliseconds)]))

  (printf "> cat master.hpp~n")
  (touch-master.hpp)
  
  (&linebreak 3)
  
  (printf "> cat master.cpp~n")
  (touch-master.cpp))
