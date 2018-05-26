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

(define &htab
  (lambda [[count 1]]
    (&hspace (* count 4))))

(define &brace
  (lambda [indent #:semicolon? [semicolon? #false]]
    (&htab indent)
    (display #\})

    (when (and semicolon?)
      (display #\;))
    (&linebreak)))

(define &pragma
  (lambda pragmas
    (for ([pragma (in-list pragmas)])
      (printf "#pragma ~a~n" pragma))
    (&linebreak 1)))

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

(define &primary-key
  (lambda [Table_pk rowids idtypes indent]
    (cond [(> (length rowids) 1) (&struct Table_pk rowids idtypes indent)]
          [else (&htab indent)
                (printf "typedef ~a ~a;~n" (car idtypes) Table_pk)
                (&linebreak 1)])))

(define &struct
  (lambda [name fields types indent]
    (&htab indent)
    (printf "private struct ~a {~n" name)

    (for ([field (in-list fields)]
          [type (in-list types)])
      (&htab (add1 indent))
      (printf "~a ~a;~n" type field))
    
    (&brace indent #:semicolon? #true)
    (&linebreak 1)))

(define &table-column-info
  (lambda [var_columns var_rowids rowids cols dbtypes not-nulls uniques]
    (printf "static Platform::String^ ~a[] = { ~a };~n" var_rowids
            (let strcat ([s (format "~s" (symbol->string (car rowids)))]
                         [r (cdr rowids)])
              (cond [(null? r) s]
                    [else (strcat (format "~a, ~s" s (symbol->string (car r)))
                                  (cdr r))])))
    (&linebreak 1)
    
    (printf "static TableColumnInfo ~a[] = {~n" var_columns)
    (for ([col (in-list cols)]
          [type (in-list dbtypes)]
          [nnil (in-list not-nulls)]
          [uniq (in-list uniques)])
      (&hspace 4)
      (printf "{ ~s, SDT::~a, nullptr, ~a | ~a | ~a },~n"
              (symbol->string col)
              (symbol->string type)
              (if (memq col rowids) 'DB_PRIMARY_KEY 0)
              (if (and nnil) 'DB_NOT_NULL 0)
              (if (and uniq) 'DB_UNIQUE 0)))
    (&brace 0 #:semicolon? #true)
    (&linebreak 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &create-table
  (case-lambda
    [(λname indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true);~n" λname)]
    [(λname tablename column_infos table_rowids)
     (printf "void WarGrey::SCADA::~a(IDBSystem* dbc, bool if_not_exists) {~n" λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a, sizeof(~a)/sizeof(TableColumnInfo));~n" column_infos column_infos)
     (&htab 1) (printf "Platform::String^ sql = vsql->create_table(~s, ~a, sizeof(~a)/sizeof(Platform::String^), if_not_exists);~n"
                       (symbol->string tablename) table_rowids table_rowids)
     (&linebreak)
     (&htab 1) (printf "dbc->exec(sql);~n")
     (&brace 0)
     (&linebreak 1)]))

(define &insert-table
  (case-lambda
    [(λname classname indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc, ~a* self, bool replace = false);~n" λname classname)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc, ~a* selves, size_t count, bool replace = false);~n" λname classname)]
    [(λname classname tablename fields column_infos)
     (printf "void WarGrey::SCADA::~a(IDBSystem* dbc, ~a* self, bool replace) {~n" λname classname)
     (&htab 1) (printf "~a(dbc, self, 1, replace);~n" λname)
     (&brace 0)
     (&linebreak 1)
     (printf "void WarGrey::SCADA::~a(IDBSystem* dbc, ~a* selves, size_t count, bool replace) {~n" λname classname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a, sizeof(~a)/sizeof(TableColumnInfo));~n" column_infos column_infos)
     (&htab 1) (printf "Platform::String^ sql = vsql->insert_into(~s, replace);~n" (symbol->string tablename))
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "for (size_t i = 0; i < count; i ++) {~n")
     (for ([field (in-list fields)]
           [idx (in-naturals)])
       (&htab 3) (printf "stmt->bind_parameter(~a, selves[i].~a);~n" idx field))
     (&linebreak 1)
     (&htab 3) (printf "dbc->exec(stmt);~n")
     (&htab 3) (printf "stmt->clear_bindings();~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&brace 0)
     (&linebreak 1)]))

(define &select-table
  (case-lambda
    [(λname classname indent)
     (&htab indent) (printf "std::list<~a> ~a(WarGrey::SCADA::IDBSystem* dbc, unsigned int limit = 0, unsigned int offset = 0);~n" classname λname)]
    [(λname classname tablename fields column_infos)
     (printf "std::list<~a> WarGrey::SCADA::~a(IDBSystem* dbc, unsigned int limit, unsigned int offset) {~n" classname λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a, sizeof(~a)/sizeof(TableColumnInfo));~n" column_infos column_infos)
     (&htab 1) (printf "Platform::String^ sql = vsql->select_from(~s, limit, offset);~n" (symbol->string tablename))
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&brace 0)
     (&linebreak 1)]))

(define &drop-table
  (case-lambda
    [(λname indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc);~n" λname)]
    [(λname tablename column_infos)
     (printf "void WarGrey::SCADA::~a(IDBSystem* dbc) {~n" λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a, sizeof(~a)/sizeof(TableColumnInfo));~n" column_infos column_infos)
     (&htab 1) (printf "Platform::String^ sql = vsql->drop_table(~s);~n" (symbol->string tablename))
     (&linebreak)
     (&htab 1) (printf "dbc->exec(sql);~n")
     (&brace 0)
     (&linebreak 1)]))
