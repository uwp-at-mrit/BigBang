#lang racket

(provide (all-defined-out))
(provide (all-from-out pict pict/snip))
(provide (all-from-out images/icons/misc))

(require images/icons/misc)

(require pict)
(require pict/snip)

(define (footnote p desc)
  (make-object pict-snip%
    (vc-append 8 p (textlet desc))))

(define (scale-by-width p width)
  (define factor (/ width (pict-width p)))
  (make-object pict-snip% (scale p factor)))

(define (scale-by-height p height)
  (define factor (/ height (pict-height p)))
  (make-object pict-snip% (scale p factor)))

(define (buttonlet content actual-width)
  (define label (text content))
  (define height (* (pict-height label) 3/2))
  (define width (+ (pict-width label) (* (pict-height label) 2)))
  (define body (filled-rounded-rectangle width height #:color "Burlywood" #:border-color "LightSkyBlue" #:border-width 1))
  (scale-by-width (cc-superimpose body label) actual-width))

(define (textlet content)
  (make-object pict-snip%
    (frame (text content))))

(define (alarmlet w)
  (define color "Purple")
  (define width (* w 3/4))
  (make-object pict-snip%
    (vc-append (lb-superimpose (disk width #:color color #:draw-border? #false)
                               (filled-rectangle width (* width 1/2) #:color color #:draw-border? #false))
               (filled-rectangle w (* w 1/8) #:color "DarkGray" #:draw-border? #false))))

(define (bitmaplet src)
  (make-object pict-snip%
    (bitmap src)))

(define shape.alarm (footnote (alarmlet 48) "基本图元: 报警器"))
(define shape.button (footnote (buttonlet "别按" 48) "基本图元: 按钮"))
(define shape.bitmap (footnote (bitmap (stopwatch-icon #:height 64)) "基本图元: 位图文件"))
