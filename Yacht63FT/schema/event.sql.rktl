#lang racket

(require "../../Scripts/ORM/schema.rkt")

(define-table event #:as AlarmEvent #:with uuid
  ([uuid     : Integer           #:default (pk64:timestamp launch-time)]
   [type     : Text              #:default 'table #:not-null #:% 'table]
   [name     : Text              #:not-null #:unique]
   [ctime    : Integer           #:default (current-milliseconds)]
   [mtime    : Integer           #:auto (current-milliseconds)]))
