#lang racket

(require "../../Scripts/ORM/schema.rkt")

(define-table event #:as AlarmEvent #:with [uuid] #:order-by timestamp
  ([uuid      : Integer       #:default pk64_timestamp]
   [name      : Integer       #:not-null] ; identities of names for localization
   [timestamp : Integer       #:not-null #:default current_milliseconds]
   [status    : Integer       #:not-null]
   [code      : Integer]
   [note      : Text])
  #:include [["dbmisc.hpp"]])
