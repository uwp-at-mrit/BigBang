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
  (lambda [var rowids cols dbtypes not-nulls uniques]
    (printf "static TableColumnInfo ~a[] = {~n" var)
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

(define &create-table.hpp
  (lambda [λname tablename indent]
    (&htab indent)
    (printf "void ~a(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true, Platform::String^ tablename = ~s);~n"
            λname (symbol->string tablename))))

(define &create-table.cpp
  (lambda [λname column_infos rowids]
    (printf "void WarGrey::SCADA::~a(IDBSystem* dbc, bool if_not_exists, Platform::String^ tablename) {~n" λname)
    (&htab 1)
    (printf "Platform::String^ rowids[] = { \"~a\"~a };~n"
            (car rowids)
            (let strcat ([s ""] [r (cdr rowids)]) (if (null? r) s (strcat (format "~a, \"~a\"" s (car r)) (cdr r)))))
    (&htab 1)
    (printf "IVirtualSQL* vsql = dbc->make_sql_factory(~a, sizeof(~a)/sizeof(TableColumnInfo));~n" column_infos column_infos)
    (&htab 1)
    (printf "Platform::String^ sql = vsql->create_table(tablename, rowids, sizeof(rowids)/sizeof(Platform::String^), if_not_exists);~n")
    (&linebreak)
    (&htab 1)
    (printf "dbc->exec(sql);~n")
    (&brace 0)
    (&linebreak 1)))
