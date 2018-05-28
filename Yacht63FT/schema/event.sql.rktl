#lang racket

(require "../../Scripts/ORM/schema.rkt")

(define-table event #:as AlarmEvent #:with [uuid]
  ([uuid     : Integer           #:default timestamp_pk64]
   [type     : Text              #:default "table" #:not-null]
   [name     : Text              #:not-null #:unique]
   [ctime    : Integer           #:default current_milliseconds]
   [mtime    : Integer           #:auto current_milliseconds]))
