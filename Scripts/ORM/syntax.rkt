#lang racket

(provide (all-from-out db/base))
(provide (for-syntax (all-defined-out)))

(require db/base)

(require (for-syntax racket/list))
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(begin-for-syntax 
  (define (parse-field-definition tablename rowids stx)
    (syntax-parse stx
      [(field Type (~or (~optional (~or (~seq #:default defval) (~seq #:auto generate)) #:name "#:default or #:auto")
                        (~optional (~seq (~and #:not-null not-null)) #:name "#:not-null")
                        (~optional (~seq (~and #:unique unique)) #:name "#:unique")
                        (~optional (~seq #:% comments) #:name "#:%")) ...)
       (define-values (primary? not-null?) (values (and (member (syntax-e #'field) rowids) #true) (attribute not-null)))
       (values (and primary? #'Type)
               (list (if (or primary? (attribute not-null)) #'Type (format-id #'Type "std::optional<~a>" (syntax-e #'Type)))
                     (cond [(attribute defval) #'defval]
                           [(attribute generate) #'generate]
                           [else #'#false])
                     (or (attribute generate) #'#false)
                     (and not-null? #'#true)
                     (and (attribute unique) #'#true)))]))
  
  (define (parse-primary-key stx)
    ; NOTE: primary keys may not contained in the defining struct in which case the struct is treated as a temporary view
    (syntax-parse stx
      [id:id (list #'id)]
      [(id0 id ...) stx])))
