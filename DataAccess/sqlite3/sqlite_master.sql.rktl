#lang racket

(require "../../Scripts/ORM/schema.rkt")

(define-table sqlite_master #:as SQLiteMaster #:with [rowid] #:order-by rowid
  ([rowid    : Integer #:not-null]
   [type     : Text    #:not-null]
   [name     : Text    #:not-null]
   [tbl_name : Text    #:not-null]
   [rootpage : Integer #:not-null]
   [sql      : Text]))

(module+ main
  (define master.sql.rktl (simplify-path (build-path (find-system-path 'orig-dir) (find-system-path 'run-file))))

  (define do-make-dao
    (lambda [cat schema]
      (printf "> cat ~a~n" (file-name-from-path schema))
      (call-with-output-file* schema #:exists 'truncate/replace cat)
      (call-with-input-file* schema (curryr copy-port (current-output-port)))))

  (define master.hpp (build-path (path-only master.sql.rktl) "sqlite_master.hpp"))
  (define master.cpp (build-path (path-only master.sql.rktl) "sqlite_master.cpp"))
  
  (do-make-dao cat-sqlite_master.hpp master.hpp)
  
  (newline)
  (newline)
  (newline)
  
  (do-make-dao cat-sqlite_master.cpp master.cpp))
