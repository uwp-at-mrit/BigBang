#lang racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/bool)
(require racket/sequence)

(require db/base)

(require "virtual-sql.rkt")

(define do-create-table
  (lambda [func create maybe-force dbc dbtable rowid cols types not-nulls uniques]
    ;;(unless (not func) (throw exn:fail:unsupported func "cannot create a temporary view"))
    (define (mksql) (create-table.sql maybe-force dbtable rowid cols types not-nulls uniques))
    (query-exec dbc (hash-ref! sqls (or maybe-force create) mksql))))

(define do-insert-table
  (lambda [func insert maybe-replace dbtable cols dbc selves refs]
    (define (mksql) (insert-into.sql maybe-replace dbtable cols))
    ;;(unless (not func) (throw exn:fail:unsupported func "cannot insert records into a temporary view"))
    (define insert.sql (hash-ref! sqls (or maybe-replace insert) mksql))
    (for ([record selves])
      (define metrics (for/list ([ref (in-list refs)]) (racket->sql (ref record) dbc)))
      (apply query-exec dbc insert.sql metrics))))

(define do-delete-from-table
  (lambda [func view? dbtable rowid dbc selves pkrefs]
    (define (mksql) (delete-from.sql dbtable rowid))
    ;;(when view? (throw exn:fail:unsupported func "cannot delete records from a temporary view"))
    (define delete.sql (hash-ref! sqls func mksql))
    (for ([record selves])
      (apply query-exec dbc delete.sql
             (for/list ([ref (in-list pkrefs)])
               (racket->sql-pk (ref record)))))))

(define do-select-table
  (lambda [select-nowhere select-where dbtable where rowid cols mkrow dbc size]
    (define (mksql method) (λ [] (simple-select.sql method dbtable rowid cols)))
    (define rows
      (cond [(not where) (in-query dbc size (hash-ref! sqls select-nowhere (mksql 'nowhere)))]
            [(vector? where) (in-query dbc size (hash-ref! sqls select-where (mksql 'byrowid)) (vector->list where))]
            [else (in-query dbc size (make-ugly-sql dbtable where cols)
                            (for/list ([r (in-list (cdr where))])
                              (racket->sql r dbc)))]))
    (sequence-map (λ [raw] (with-handlers ([exn? (λ [e] e)]) (mkrow raw))) rows)))

(define do-seek-table
  (lambda [select-where dbtable where rowid cols mkrow dbc]
    (define (mksql method) (λ [] (simple-select.sql method dbtable rowid cols)))
    (define (mkugly fmt _) (ugly-select.sql dbtable fmt (length _) cols))
    (define maybe-raw
      (cond [(vector? where) (apply query-maybe-row dbc (hash-ref! sqls select-where (mksql 'byrowid)) (vector->list where))]
            [else (apply query-maybe-row dbc (make-ugly-sql dbtable where cols)
                         (for/list ([r (in-list (cdr where))])
                           (racket->sql r dbc)))]))
    (and maybe-raw (mkrow (vector->list maybe-raw)))))

(define do-update-table
  (lambda [func view? table maybe-chpk dbtable rowid cols dbc selves refs pkrefs]
    ;;(when view? (throw exn:fail:unsupported func "cannot update records of a temporary view"))
    (define (mkup) (update.sql dbtable rowid cols))
    (define (mkck) (simple-select.sql 'ckrowid dbtable rowid cols))
    (define up.sql (hash-ref! sqls func mkup))
    (define ck.sql (if maybe-chpk (hash-ref! sqls maybe-chpk mkck) up.sql))
    (for ([record selves])
      (define rowid (for/list ([ref (in-list pkrefs)]) (racket->sql-pk (ref record))))
      #;(when (and maybe-chpk (false? (apply query-maybe-value dbc ck.sql rowid)))
        (schema-throw [exn:schema 'norow `((struct . ,table) (record . ,(list->vector rowid)))]
                      func "no such record found in the table"))
      (define metrics (for/list ([ref (in-list refs)]) (racket->sql (ref record) dbc)))
      (apply query dbc up.sql (append metrics rowid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ugly-sqls (make-hash))
(define sqls (make-hasheq))

(define make-ugly-sql
  (lambda [dbtable where cols]
    (hash-ref! ugly-sqls
               (cons dbtable (car where))
               (λ [] (ugly-select.sql dbtable (car where) (length (cdr where)) cols)))))

(define check-default-value
  (lambda [func field defval]
    (when (void? defval) (error func "missing value for field '~a'" field))
    defval))

(define check-selected-row
  (lambda [func table table-row? fields guards]
    (define metrics (map sql->racket fields guards))
    (cond [(table-row? metrics) metrics]
          #;[else (schema-throw [exn:schema 'assertion `((struct . ,table) (got . ,metrics))]
                              func "maybe the database is penetrated")])))

(define check-example
  (lambda [example mkdefval]
    (cond [(null? example) (mkdefval)]
          [(list? example) example]
          [else (list example)])))

(define check-row
  (lambda [func metrics table-row? errfmt . errmsg]
    (cond [(table-row? metrics) metrics]
          [else (apply error func errfmt errmsg)])))

(define field-value
  (lambda [func field self table-field value mkdefval]
    (cond [(not (void? value)) value]
          [(not self) (check-default-value func field (mkdefval))]
          [else (table-field self)])))

(define dict->record
  (lambda [func src fields mkdefval table-row?]
    (define metrics
      (for/list ([field (in-list fields)]
                 [mkval (in-list mkdefval)])
        (hash-ref src field (λ [] (check-default-value func field (mkval))))))
    (check-row func metrics table-row? "mismatched source: ~a" metrics)))

(define make-dict
  (lambda [fields fvalues skip?]
    (cond [(not skip?) (make-immutable-hasheq (map cons fields fvalues))]
          [else (for/hasheq
                  ([key (in-list fields)]
                   [val (in-list fvalues)]
                   #:when val)
                  (values key val))])))
