#lang racket

(require racket/draw)
(require math/number-theory)

; https://pomax.github.io/bezierinfo/

(define (bezier-bouding-box point others)
  (define-values (x0 y0) (values (real-part point) (imag-part point)))
  (let tr ([xmin x0] [xmax x0] [ymin y0] [ymax y0] [points others])
    (cond [(null? points) (values xmin ymin xmax ymax)]
          [else (let-values ([(x y) (values (real-part (car points)) (imag-part (car points)))])
                  (tr (min xmin x) (max xmax x)
                      (min ymin y) (max ymax y)
                      (cdr points)))])))

(define (make-bezier-function point others)
  (define n (length others))
  (lambda [t]
    (let sum ([x 0.0] [y 0.0] [i 0] [points (cons point others)])
      (cond [(null? points) (make-rectangular x y)]
            [else (let ([weight (car points)])
                    (define-values (C 1-t^ t^) (values (binomial n i) (expt (- 1.0 t) (- n i)) (expt t i)))
                    (sum (+ x (* C 1-t^ t^ (real-part weight)))
                         (+ y (* C 1-t^ t^ (imag-part weight)))
                         (+ i 1) (cdr points)))]))))

(define (make-point-transform x0 y0 xn yn width height margin)
  (define-values (x-length y-length) (values (- width margin margin) (- height margin margin)))
  (define-values (x-ratio y-ratio) (values (/ (- xn x0) x-length) (/ (- yn y0) y-length)))
  (define-values (x-start y-start) (values margin margin))
  (lambda [p]
    (cons (+ x-start (* (- (real-part p) x0) x-ratio))
          (+ y-start (* (- (imag-part p) y0) y-ratio)))))

(define (make-bezier-range samples)
  (define step (/ 1.0 samples))
  (range 0.0 (+ 1.0 step) step))

(define (draw-endpoint dc point diameter)
  (define radius (* diameter 0.5))
  (send dc draw-ellipse
        (- (car point) radius)
        (- (cdr point) radius)
        diameter diameter))

(define (bezier #:width [width 300] #:height [height 300] #:samples [samples 64] #:dot-diameter [diameter 4.0] point . others)
  (define-values (x0 y0 xn yn) (bezier-bouding-box point others))
  (define point-transform (make-point-transform x0 y0 xn yn width height diameter))
  (define make-intermediate-point (compose1 point-transform (make-bezier-function point others)))
  (define curve-nodes (map make-intermediate-point (make-bezier-range samples)))
  (define point-nodes (map point-transform (cons point others)))
  (define canvas (make-bitmap width height #:backing-scale 2.0))
  (define dc (send canvas make-dc))
  (send dc set-smoothing 'smoothed)
  (send dc set-pen (make-pen #:color "Gainsboro"))
  (send dc draw-lines point-nodes)
  (for ([control-point (in-list (take (cdr point-nodes) (sub1 (length others))))])
    (draw-endpoint dc control-point diameter))
  (send dc set-pen (make-pen #:color "Green"))
  (draw-endpoint dc (car point-nodes) diameter)
  (send dc set-pen (make-pen #:color "Red"))
  (draw-endpoint dc (last point-nodes) diameter)
  (send dc set-pen (make-pen #:color "Black"))
  (send dc draw-lines curve-nodes)
  canvas)

(time (bezier 100.0+75.0i  20.0+110.0i  70.0+155.0i))
(time (bezier 60.0+105.0i  75.0+30.0i   215.0+115.0i 140.0+160.0i))
(time (bezier 120.0+160.0i 35.0+200.0i  220.0+260.0i 220.0+40.0i))
(time (bezier 25.0+128.0i  102.4+230.4i 153.6+25.6i  230.4+128.0i))
(time (bezier 198.0+18.0i  34.0+57.0i   18.0+156.0i  221.0+90.0i
              186.0+177.0i 14.0+82.0i   12.0+236.0i  45.0+290.0i
              218.0+294.0i 248.0+188.0i))
