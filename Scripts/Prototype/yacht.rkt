#lang racket/gui

(require "bezier.rkt")

(define-values (samples window-size) (values 64 512))

(define points (call-with-input-file* "vertices.txt" (λ [/dev/stdin] (sequence->list (in-port read /dev/stdin)))))

(define yacht%
  (class canvas% (super-new)
    (init-field points [diameter 4.0] [samples #false])
    
    (define-values (point others) (values (car points) (cdr points)))
    (define-values (x0 y0 xn yn) (bezier-window-enclosing-box point others))
    (define-values (width height) (values 1 1))

    (define image (send this make-bitmap 1 1))
    (define dc (send image make-dc))
    (define pt-transform (void))
    
    (define/override (on-paint)
      (send (send this get-dc) draw-bitmap image 0 0))

    (define/override (on-size w h)
      (set!-values (width height) (values w h))
      (set! pt-transform (make-point-transform x0 y0 xn yn width height diameter))
      (set! image (send this make-bitmap width height))

      (define dc (send image make-dc))
      (send dc set-smoothing 'smoothed)
      (when (send (send this get-parent) is-maximized?)
        (thread (thunk (draw-bezier dc)))))

    (define/private (draw-bezier dc)
      (define make-intermediate-point (make-bezier-function point others))
      (define sample-count (or samples (max width height)))
      (define curve-nodes (for/list ([t (in-bezier-domain sample-count)]) (pt-transform (make-intermediate-point t))))
      (define point-nodes (map pt-transform (cons point others)))
      (send (send this get-parent) set-label (format "Yacht(~a, ~a)" width height))
      (send dc set-pen (make-pen #:color "Gainsboro"))
      (send dc draw-lines point-nodes)
      (send dc set-pen (make-pen #:color "Black"))
      (send dc draw-lines curve-nodes)
      (send this refresh))))

(define universe (new frame% [label "Yacht"] [min-width 512] [min-height 512]))
(define yacht (new yacht% [parent universe] [points points] [samples 32]))

(send* universe (center 'both) (show #true) (maximize #true))
