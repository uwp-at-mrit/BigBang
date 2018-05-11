#lang racket/gui

(require pict)

(define txt (get-text-from-user	"Content" "Please input some text"))

(for/list ([face (get-face-list)])
  (frame (text (format "~a: ~a" face txt) face 24)))
