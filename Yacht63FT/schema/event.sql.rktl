#lang racket

(require "../../Scripts/ORM/schema.rkt")

(define-table event #:as AlarmEvent #:with [uuid name]
  ([uuid     : Integer           #:default pk64_timestamp]
   [type     : Text              #:default "table" #:not-null]
   [name     : Text              #:not-null #:unique]
   [ctime    : Integer           #:default current_milliseconds]
   [mtime    : Integer           #:auto current_milliseconds])
  #:include [["dbmisc.hpp"]])
