#lang racket

(require images/icons/symbol)
(require racket/runtime-path)

(define-runtime-path assets "../WinACS/Assets")

(define λicon (lambda-icon #:height 400))
(send λicon save-file (build-path assets "SplashScreen-400x400.png")'png)
