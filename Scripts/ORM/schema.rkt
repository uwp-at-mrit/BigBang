#lang racket

(provide (all-defined-out))

(require "syntax.rkt")
(require "cpp.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))

(define-syntax (define-table stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ table #:as Table #:with primary-key
        (~optional (~seq #:order-by order-by) #:defaults ([order-by #'#false]))
        ([field : DataType constraints ...] ...)
        (~or (~optional (~seq #:include addition-hpps) #:name "#:include" #:defaults ([addition-hpps #'[]]))
             (~optional (~seq #:namespace addition-nses) #:name "#:namespace" #:defaults ([addition-nses #'[]]))) ...)
     (with-syntax* ([(rowid ...) (parse-primary-key #'primary-key)]
                    [order_by (parse-order-by #'order-by)]
                    [Table-pk (format-id #'Table "~a_pk" (syntax-e #'Table))]
                    [([view? RowidType ...]
                      [(MaybeType defval autoval not-null unique) ...]
                      [cat-table.hpp cat-table.cpp table.hpp table.cpp table-rowids table-columns table-id]
                      [create-table insert-table delete-table update-table select-table seek-table drop-table
                                    make-table default-table refresh-table store-table restore-table list-table])
                     (let ([pkids (let ([pk (syntax->datum #'primary-key)]) (if (list? pk) pk (list pk)))]
                           [tablename (syntax-e #'table)])
                       (define-values (sdleif sdiwor)
                         (for/fold ([sdleif null] [sdiwor null])
                                   ([<stx> (in-syntax #'([field DataType constraints ...] ...))])
                           (define-values (maybe-pktype field-info) (parse-field-definition tablename pkids <stx>))
                           (values (cons field-info sdleif) (if maybe-pktype (cons maybe-pktype sdiwor) sdiwor))))
                       (list (cons (< (length sdiwor) (length pkids)) (reverse sdiwor))
                             (reverse sdleif)
                             (for/list ([fmt (in-list (list "cat-~a.hpp" "cat-~a.cpp" "~a.hpp" "~a.cpp"
                                                            "~a_rowids" "~a_columns" "~a_identity"))])
                               (format-id #'table fmt tablename))
                             (for/list ([prefix (in-list (list 'create 'insert 'delete 'update 'select 'seek 'drop
                                                               'make 'default 'refresh 'store 'restore 'list))])
                               (format-id #'table "~a_~a" prefix tablename))))]
                    [([header ...] ...) #'addition-hpps]
                    [([ns ...] ...) #'addition-nses])
       #'(begin (when (and view?)
                  (raise-user-error 'table "primary keys must be defined explicitly"))

                (define cat-table.hpp
                  (lambda [[/dev/stdout (current-output-port)]]
                    (parameterize ([current-output-port /dev/stdout])
                      (&pragma 'once)
                      (&include 'list 'optional)
                      (&include "dbsystem.hpp")
                      
                      (&namespace 'WarGrey::SCADA
                                  (λ [indent]
                                    (&primary-key 'Table-pk '(rowid ...) '(RowidType ...) indent)
                                    (&struct 'Table '(field ...) '(MaybeType ...) indent)

                                    (&#%table 'table-id 'Table 'Table-pk indent)

                                    (&linebreak 1)
                                    (&make-table 'make-table 'Table '(field ...) '(DataType ...) '(defval ...) indent)
                                    (&default-table 'default-table 'Table '(field ...) '(DataType ...) '(defval ...) indent)
                                    (&refresh-table 'refresh-table 'Table indent)
                                    (&store-table 'store-table 'Table indent)
                                    (&restore-table 'restore-table 'Table indent)

                                    (&linebreak 1)
                                    (&create-table 'create-table indent)
                                    (&insert-table 'insert-table 'Table indent)
                                    (&list-table 'list-table 'Table-pk 'order_by indent)
                                    (&select-table 'select-table 'Table 'order_by indent)
                                    (&seek-table 'seek-table 'Table 'Table-pk indent)
                                    (&update-table 'update-table 'Table indent)
                                    (&delete-table 'delete-table 'Table-pk indent)
                                    (&drop-table 'drop-table indent)

                                    (&linebreak 1)
                                    (&template-insert 'insert-table 'Table indent)
                                    (&template-update 'update-table 'Table indent)
                                    (&template-delete 'delete-table 'Table-pk indent))))))
                
                (define cat-table.cpp
                  (lambda [[/dev/stdout (current-output-port)]]
                    (parameterize ([current-output-port /dev/stdout])
                      (&include (symbol->string 'table.hpp))
                      (&include "dbsystem.hpp" "dbtypes.hpp")
                      (&include 'header ...) ...
                      (&using-namespace 'WarGrey::SCADA)
                      (&using-namespace 'ns ...) ...
                      (&table-column-info 'table-columns 'table-rowids '(rowid ...) '(field ...) '(DataType ...) '(not-null ...) '(unique ...))

                      (&separator)
                      (&#%table 'table-id 'Table 'Table-pk '(rowid ...))
                      (&make-table 'make-table 'Table '(field ...) '(DataType ...) '(defval ...) 'default-table)
                      (&default-table 'default-table 'Table '(field ...) '(DataType ...) '(defval ...))
                      (&refresh-table 'refresh-table 'Table '(field ...) '(autoval ...))
                      (&store-table 'store-table 'Table '(field ...))
                      (&restore-table 'restore-table 'Table '(field ...) '(DataType ...) '(not-null ...) '(rowid ...))

                      (&separator)
                      (&create-table 'create-table 'table 'table-columns 'table-rowids)
                      (&insert-table 'insert-table 'Table 'table 'store-table 'table-columns)
                      (&list-table 'list-table 'Table-pk 'table '(rowid ...) '(RowidType ...) 'table-rowids 'table-columns)
                      (&select-table 'select-table 'Table 'table 'restore-table 'table-columns)
                      (&seek-table 'seek-table 'Table 'table 'restore-table 'table-columns 'Table-pk '(rowid ...) 'table-rowids)
                      (&update-table 'update-table 'Table 'table '(rowid ...) '(field ...) 'table-rowids 'table-columns 'refresh-table)
                      (&delete-table 'delete-table 'Table-pk 'table '(rowid ...) 'table-rowids 'table-columns)
                      (&drop-table 'drop-table 'table 'table-columns))))))]))
