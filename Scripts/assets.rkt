#lang racket

(require images/icons/symbol)
(require racket/runtime-path)

(define-runtime-path assets "../StartsWithPanel/Assets")

(define λicon (lambda-icon #:height 400))
(send λicon save-file (build-path assets "lambda400.png")'png)
