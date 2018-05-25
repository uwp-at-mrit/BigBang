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
                        (~optional (~seq #:guard guard) #:name "#:guard")
                        (~optional (~seq (~and #:not-null not-null)) #:name "#:not-null")
                        (~optional (~seq (~and #:unique unique)) #:name "#:unique")
                        (~optional (~seq #:% comments) #:name "#:%")) ...)
       (define-values (primary? not-null?) (values (and (member (syntax-e #'field) rowids) #true) (attribute not-null)))
       (define table-field (format-id #'field "~a-~a" tablename (syntax-e #'field)))
       (values (and primary? table-field)
               (list (datum->syntax #'field (string->keyword (symbol->string (syntax-e #'field))))
                     table-field
                     (if (or primary? (attribute not-null)) #'Type (format-id #'Type "std::optional<~a>" (syntax-e #'Type)))
                     (or (attribute generate) #'(void))
                     (cond [(attribute defval) #'(defval)]
                           [(attribute generate) #'(generate)]
                           [(or primary? not-null?) #'()]
                           [else (syntax-case #'Type [Listof]
                                   [(Listof _) #'(null)]
                                   [_ #'(#false)])])
                     (or (attribute comments) #'null)
                     (or (attribute guard)
                         (syntax-case #'Type [String Symbol]
                           [String #'values]
                           [Symbol #'string->symbol]
                           [_ #'(λ [[sql : String]] (call-with-input-string sql read))]))
                     (and not-null? #'#true)
                     (and (attribute unique) #'#true)))]))
  
  (define (parse-primary-key stx)
    ; NOTE: primary keys may not contained in the defining struct in which case the struct is treated as a temporary view
    (syntax-parse stx
      [id:id (list #'id)]
      [(id0 id ...) stx])))
