#lang racket

(provide (all-defined-out))

(define cstring 'std::string)

(define &separator
  (lambda []
    (printf "/~a/~n" (make-string 98 #\*))))

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
    (when (> (length pragmas) 0)
      (&linebreak 1))))

(define &include
  (lambda headers
    (for ([header (in-list headers)])
      (cond [(string? header) (printf "#include ~s~n" header)]
            [else (printf "#include <~a>~n" header)]))
    (when (> (length headers) 0)
      (&linebreak 1))))

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
    (printf "static const char* ~a[] = { ~a };~n" var_rowids
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

(define &#%table
  (lambda [λname Table Table_pk indent/rowids]
    (cond [(number? indent/rowids)
           (&htab indent/rowids) (printf "WarGrey::SCADA::~a ~a(WarGrey::SCADA::~a& self);~n" Table_pk λname Table)]
          [else (printf "~a WarGrey::SCADA::~a(~a& self) {~n" Table_pk λname Table)
                (&htab 1)
                (if (= (length indent/rowids) 1)
                    (printf "return self.~a;~n" (car indent/rowids))
                    (printf "return { ~a };~n" (string-join (map (curry format "self.~a") indent/rowids) ", ")))
                (&brace 0)
                (&linebreak 1)])))

(define &make-table
  (lambda [λname Table fields types defvals indent/default]
    (cond [(number? indent/default)
           (&htab indent/default) (printf "WarGrey::SCADA::~a ~a(~a);~n" Table λname (make-arguments fields types defvals 'declare))]
          [else (printf "~a WarGrey::SCADA::~a(~a) {~n" Table λname (make-arguments fields types defvals 'define))
                (&htab 1) (printf "~a self;~n" Table)
                (&linebreak 1)
                (&htab 1) (printf "~a(self, ~a);~n" indent/default (make-arguments fields types defvals 'call))
                (&linebreak 1)
                (&htab 1) (printf "return self;~n")
                (&brace 0)
                (&linebreak 1)])))

(define &default-table
  (case-lambda
    [(λname Table fields types defvals indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::~a& self, ~a);~n" λname Table (make-arguments fields types defvals 'declare))]
    [(λname Table fields types defvals)
     (printf "void WarGrey::SCADA::~a(~a& self, ~a) {~n" λname Table (make-arguments fields types defvals 'define))
     (for ([field (in-list fields)]
           [val (in-list defvals)])
       (cond [(not val) (&htab 1) (printf "if (~a.has_value()) { self.~a = ~a.value(); }~n" field field field)]
             [else (&htab 1) (printf "self.~a = " field)
                   (cond [(symbol? val) (printf "~a();~n" val)]
                         [else (printf "((~a.has_value()) ? ~a.value() : ~s);~n" field field val)])]))
     (&brace 0)
     (&linebreak 1)]))

(define &refresh-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::~a& self);~n" λname Table)]
    [(λname Table fields autovals)
     (printf "void WarGrey::SCADA::~a(~a& self) {~n" λname Table)
     (for ([field (in-list fields)]
           [val (in-list autovals)])
       (when (and val)
         (&htab 1) (printf "self.~a = ~a();~n" field val)))
     (&brace 0)
     (&linebreak 1)]))

(define &store-table
  (lambda [λname Table indent/fields]
    (cond [(number? indent/fields)
           (&htab indent/fields) (printf "void ~a(WarGrey::SCADA::~a& self, WarGrey::SCADA::IPreparedStatement* stmt);~n" λname Table)]
          [else (printf "void WarGrey::SCADA::~a(~a& self, IPreparedStatement* stmt) {~n" λname Table)
                (for ([field (in-list indent/fields)]
                      [idx (in-naturals)])
                  (&htab 1) (printf "stmt->bind_parameter(~a, self.~a);~n" idx field))
                (&brace 0)
                (&linebreak 1)])))

(define &restore-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::~a& self, WarGrey::SCADA::IPreparedStatement* stmt);~n" λname Table)]
    [(λname Table fields types not-nulls rowids)
     (printf "void WarGrey::SCADA::~a(~a& self, IPreparedStatement* stmt) {~n" λname Table)
     (for ([field (in-list fields)]
           [type (in-list types)]
           [n-nil (in-list not-nulls)]
           [idx (in-naturals)])
       (&htab 1) (printf "self.~a = stmt->column~a~a(~aU);~n" field
                         (if (or (memq field rowids) n-nil) "_" "_maybe_")
                         (sql-type type)
                         idx))
     (&brace 0)
     (&linebreak 1)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &create-table
  (case-lambda
    [(λname indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true);~n" λname)]
    [(λname tablename column_infos table_rowids)
     (printf "void WarGrey::SCADA::~a(IDBSystem* dbc, bool if_not_exists) {~n" λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->create_table(~s, ~a, if_not_exists);~n" cstring (symbol->string tablename) table_rowids)
     (&linebreak 1)
     (&htab 1) (printf "dbc->exec(sql);~n")
     (&brace 0)
     (&linebreak 1)]))

(define &insert-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc, ~a& self, bool replace = false);~n" λname Table)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc, ~a* selves, size_t count, bool replace = false);~n" λname Table)]
    [(λname Table tablename store column_infos)
     (printf "void WarGrey::SCADA::~a(IDBSystem* dbc, ~a& self, bool replace) {~n" λname Table)
     (&htab 1) (printf "~a(dbc, &self, 1, replace);~n" λname)
     (&brace 0)
     (&linebreak 1)
     (printf "void WarGrey::SCADA::~a(IDBSystem* dbc, ~a* selves, size_t count, bool replace) {~n" λname Table)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->insert_into(~s, replace);~n" cstring (symbol->string tablename))
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "for (int i = 0; i < count; i ++) {~n")
     (&htab 3) (printf "~a(selves[i], stmt);~n" store)
     (&htab 3) (printf "dbc->exec(stmt);~n")
     (&htab 3) (printf "stmt->reset(true);~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&brace 0)
     (&linebreak 1)]))

(define &list-table
  (case-lambda
    [(λname Table_pk indent)
     (&htab indent) (printf "std::list<WarGrey::SCADA::~a> ~a(WarGrey::SCADA::IDBSystem* dbc, unsigned int limit = 0, unsigned int offset = 0);~n" Table_pk λname)]
    [(λname Table_pk tablename rowids rowidtypes table-rowids column_infos)
     (define rowcount (length rowids))
     (printf "std::list<~a> WarGrey::SCADA::~a(IDBSystem* dbc, unsigned int limit, unsigned int offset) {~n" Table_pk λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->select_from(~s, ~a, limit, offset);~n" cstring (symbol->string tablename) table-rowids)
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&htab 1) (printf "std::list<~a> queries;~n" Table_pk)
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (when (> rowcount 1)
       (&htab 2) (printf "~a self;~n" Table_pk)
       (&linebreak 1))
     (&htab 2) (printf "while(stmt->step()) {~n")
     (cond [(= rowcount 1) (&htab 3) (printf "queries.push_back(stmt->column_~a(0U));~n" (sql-type (car rowidtypes)))]
           [else (for ([id (in-list rowids)]
                       [type (in-list rowidtypes)]
                       [idx (in-naturals)])
                   (&htab 3) (printf "self.~a = stmt->column_~a(~aU);~n" id (sql-type type) idx))
                 (&linebreak 1)
                 (&htab 3) (printf "queries.push_back(self);~n")])
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&linebreak 1)
     (&htab 1) (printf "return queries;~n")
     (&brace 0)
     (&linebreak 1)]))

(define &select-table
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "std::list<WarGrey::SCADA::~a> ~a(WarGrey::SCADA::IDBSystem* dbc, unsigned int limit = 0, unsigned int offset = 0);~n" Table λname)]
    [(λname Table tablename restore column_infos)
     (printf "std::list<~a> WarGrey::SCADA::~a(IDBSystem* dbc, unsigned int limit, unsigned int offset) {~n" Table λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->select_from(~s, limit, offset);~n" cstring (symbol->string tablename))
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&htab 1) (printf "std::list<~a> queries;~n" Table)
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "~a self;~n" Table)
     (&linebreak 1)
     (&htab 2) (printf "while(stmt->step()) {~n")
     (&htab 3) (printf "~a(self, stmt);~n" restore)
     (&htab 3) (printf "queries.push_back(self);~n")
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&linebreak 1)
     (&htab 1) (printf "return queries;~n")
     (&brace 0)
     (&linebreak 1)]))

(define &seek-table
  (case-lambda
    [(λname Table Table_pk indent)
     (&htab indent) (printf "std::optional<WarGrey::SCADA::~a> ~a(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::~a where);~n" Table λname Table_pk)]
    [(λname Table tablename restore column_infos Table_pk rowids table-rowids)
     (printf "std::optional<~a> WarGrey::SCADA::~a(IDBSystem* dbc, ~a where) {~n" Table λname Table_pk)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->seek_from(~s, ~a);~n" cstring (symbol->string tablename) table-rowids)
     (&htab 1) (printf "IPreparedStatement* stmt = dbc->prepare(sql);~n")
     (&htab 1) (printf "std::optional<~a> query;~n" Table)
     (&linebreak 1)
     (&htab 1) (printf "if (stmt != nullptr) {~n")
     (&htab 2) (printf "~a self;~n" Table)
     (&linebreak 1)
     (cond [(= (length rowids) 1) (&htab 2) (printf "stmt->bind_parameter(0, ~a);~n" (car rowids))]
           [else (for ([rowid (in-list rowids)]
                       [idx (in-naturals)])
                   (&htab 2) (printf "stmt->bind_parameter(~a, where.~a);~n" idx rowid))])
     (&linebreak 1)
     (&htab 2) (printf "if (stmt->step()) {~n")
     (&htab 3) (printf "~a(self, stmt);~n" restore)
     (&htab 3) (printf "query = std::optional<~a>(self);~n" Table)
     (&brace 2)
     (&linebreak 1)
     (&htab 2) (printf "delete stmt;~n")
     (&brace 1)
     (&linebreak 1)
     (&htab 1) (printf "return query;~n")
     (&brace 0)
     (&linebreak 1)]))

(define &drop-table
  (case-lambda
    [(λname indent)
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc);~n" λname)]
    [(λname tablename column_infos)
     (printf "void WarGrey::SCADA::~a(IDBSystem* dbc) {~n" λname)
     (&htab 1) (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a);~n" column_infos)
     (&htab 1) (printf "~a sql = vsql->drop_table(~s);~n" cstring (symbol->string tablename))
     (&linebreak)
     (&htab 1) (printf "dbc->exec(sql);~n")
     (&brace 0)
     (&linebreak 1)]))

(define &template-insert
  (case-lambda
    [(λname Table indent)
     (&htab indent) (printf "template<size_t N>~n")
     (&htab indent) (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc, ~a (&selves)[N], bool replace = false) {~n" λname Table)
     (&htab (+ indent 1)) (printf "~a(dbc, selves, N, replace);~n" λname)
     (&brace 1)
     (&linebreak 1)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-arguments
  (lambda [fields types defvals hint]
    (string-join 
     (filter-map (λ [field type defval]
                   (and (not (symbol? defval))
                        (case hint
                          [(call) (symbol->string field)]
                          [(declare) (format "std::optional<~a> ~a = std::nullopt" type field)]
                          [else (format "std::optional<~a> ~a" type field)])))
                 fields types defvals)
     ", ")))

(define sql-type
  (lambda [type]
    (case type
      [(Integer) 'int64]
      [(Float) 'double]
      [else 'text])))
