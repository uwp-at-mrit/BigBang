#lang racket

(require bitmap/color)

(define c (rgb* 'DeepSkyBlue))
(string->symbol (number->string (flcolor->hex c) 16))
