#lang racket/gui

(require "bezier.rkt")

(define-values (samples window-size) (values 64 512))

(define parts
  (for/list ([vertices.txt (in-list (list "deck_bottom.rktl" "deck_top.rktl"))])
    (call-with-input-file* vertices.txt read)))

(define yacht%
  (class canvas% (super-new)
    (init-field parts [diameter 4.0] [samples #false])
    
    (define-values (x0 y0 xn yn) (values 528467.29464027 -62041.37096839 571267.29464027 -42041.37096839))
    (define-values (width height) (values 1 1))
    (define image (send this make-bitmap width height))
    (define dc (send image make-dc))
    (define pt-transform (void))
    
    (define/override (on-paint)
      (send (send this get-dc) draw-bitmap image 0 0))

    (define/override (on-size w h)
      (set!-values (width height) (values w h))
      (set! pt-transform (make-point-transform x0 y0 xn yn width height diameter))
      (send (send this get-parent) set-label (format "Yacht(~a, ~a)" width height))

      (set! image (send this make-bitmap width height))
      (define dc (send image make-dc))
      (send dc set-smoothing 'smoothed)
      (thread (thunk (draw-bezier dc))))

    (define/private (draw-bezier dc)
      (for ([points (in-list parts)])
        (define-values (point others) (values (car points) (cdr points)))
        (define make-intermediate-point (make-bezier-function point others))
        (define sample-count (or samples (max width height)))
        (define curve-nodes (for/list ([t (in-bezier-domain sample-count)]) (pt-transform (make-intermediate-point t))))
        (define point-nodes (map pt-transform (cons point others)))
        
        (send dc set-pen (make-pen #:color "Gainsboro"))
        (for ([control-point (in-list (take (cdr point-nodes) (sub1 (length others))))])
          (draw-endpoint dc control-point diameter))
        (send dc set-pen (make-pen #:color "Green"))
        (draw-endpoint dc (car point-nodes) diameter)
        (send dc set-pen (make-pen #:color "Red"))
        (draw-endpoint dc (last point-nodes) diameter)
        (send dc set-pen (make-pen #:color "Black"))
        (send dc draw-lines curve-nodes)
        (send this refresh)))))

(define universe (new frame% [label "Yacht"] [min-width 512] [min-height 512]))
(define yacht (new yacht% [parent universe] [parts parts] [samples 32]))

(send* universe (center 'both) (show #true) (maximize #true))
