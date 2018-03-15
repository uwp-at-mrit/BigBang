#lang racket/gui

(require "bezier.rkt")

(define-values (samples window-size) (values 64 512))
(define-values (x0 y0 xn yn) (values 528467.29464027 -62041.37096839 571267.29464027 -42041.37096839))

(define vertices-take
  (lambda [vertices count]
    (define total (length vertices))
    (define last-idx (sub1 total))
    (define step (quotient total count))
    (let select ([vs (list (car vertices))] [i (+ 1 step)])
      (cond [(>= i last-idx) (reverse (cons (list-ref vertices last-idx) vs))]
            [else (select (cons (list-ref vertices i) vs) (+ i step))]))))

(define compute-curve-points
  (lambda [src samples pt-transform]
    (define-values (point others) (values (car src) (cdr src)))
    (define make-intermediate-point (make-bezier-function point others))
    (values (for/list ([t (in-bezier-domain samples)])
              (pt-transform (make-intermediate-point t)))
            (map pt-transform (cons point others)))))

(define parts
  (for/list ([vertices.txt (in-list (list "deck_bottom.rktl" "deck_top.rktl" "cabin_rt.rktl"))])
    (call-with-input-file* vertices.txt read)))

(define bezier%
  (class canvas% (super-new)
    (init-field parts [diameter 4.0] [samples #false])
    
    (define-values (width height) (values 1 1))
    (define sample-count (or samples 128))
    (define image (send this make-bitmap width height))
    (define dc (send image make-dc))
    (define pt-transform (void))
    (define saved-points (make-hash))

    (define ghostcat
      (make-object timer%
        (let* ([max-count 64]
               [counts (append (range 2 max-count) (range (sub1 max-count) 2 -1))]
               [size (length counts)]
               [idx 0])
          (thunk (let ([count (list-ref counts (remainder idx size))])
                   (send (send this get-parent) set-label (format "Yacht(~a, ~a, ~a)" width height count))
                   (draw-bezier dc count)
                   (set! idx (add1 idx)))))))
    
    (define/override (on-paint)
      (send (send this get-dc) draw-bitmap image 0 0))

    (define/override (on-size w h)
      (hash-clear! saved-points)
      (set!-values (width height) (values w h))
      (set! pt-transform (make-point-transform x0 y0 xn yn width height diameter))
      (set! sample-count (or samples (max width height)))

      (set! image (send this make-bitmap width height))
      (set! dc (send image make-dc))
      (send dc set-smoothing 'smoothed))

    (define/override (on-superwindow-show shown?)
      (cond [(not shown?) (send ghostcat stop)]
            [else (send ghostcat start (quotient 1000 16))]))

    (define/private (draw-bezier dc count)
      (send dc clear)
      (for ([points (in-list parts)]
            [seq (in-naturals)])
        (define curve.point-nodes
          (hash-ref! saved-points (cons seq count)
                     (thunk (let-values ([(curve-nodes point-nodes) (compute-curve-points points sample-count pt-transform)])
                              (cons curve-nodes point-nodes)))))
        (define-values (curve-nodes point-nodes) (values (car curve.point-nodes) (cdr curve.point-nodes)))

        (send dc set-pen (make-pen #:color "Gainsboro"))
        (for ([control-point (in-list (take (cdr point-nodes) (- (length points) 2)))])
          (draw-endpoint dc control-point diameter))
        (send dc set-pen (make-pen #:color "Green"))
        (draw-endpoint dc (car point-nodes) diameter)
        (send dc set-pen (make-pen #:color "Red"))
        (draw-endpoint dc (last point-nodes) diameter)
        (send dc set-pen (make-pen #:color "Black"))
        (send dc draw-lines curve-nodes)
        (send this refresh)))))

(define yacht%
  (class canvas% (super-new)
    (init-field parts [diameter 4.0] [samples #false])
    
    (define-values (width height) (values 1 1))
    (define image (send this make-bitmap width height))
    (define pt-transform (void))

    (define ghostcat (thread void))
    
    (define/override (on-paint)
      (send (send this get-dc) draw-bitmap image 0 0))

    (define/override (on-size w h)
      (set!-values (width height) (values w h))
      (set! pt-transform (make-point-transform x0 y0 xn yn width height diameter))
      (send (send this get-parent) set-label (format "Yacht(~a, ~a)" width height))

      (set! image (send this make-bitmap width height))
      (define dc (send image make-dc))
      (send dc set-smoothing 'smoothed)

      (when (thread-running? ghostcat) (kill-thread ghostcat))
      (set! ghostcat (thread (thunk (draw-bezier dc)))))

    (define/override (on-superwindow-show shown?)
      (when (not shown?)
        (when (thread-running? ghostcat)
          (kill-thread ghostcat))))

    (define/private (draw-bezier dc)
      (for ([points (in-list parts)])
        (define-values (curve-nodes point-nodes)
          (compute-curve-points points (or samples (max width height)) pt-transform))
        (send dc set-pen (make-pen #:color "Gainsboro"))
        (for ([control-point (in-list (take (cdr point-nodes) (- (length points) 2)))])
          (draw-endpoint dc control-point diameter))
        (send dc set-pen (make-pen #:color "Green"))
        (draw-endpoint dc (car point-nodes) diameter)
        (send dc set-pen (make-pen #:color "Red"))
        (draw-endpoint dc (last point-nodes) diameter)
        (send dc set-pen (make-pen #:color "Black"))
        (send dc draw-lines curve-nodes)
        (send this refresh)))))

(define universe (new frame% [label "Yacht"] [min-width 512] [min-height 512]))
(define yacht (new bezier% [parent universe] [parts parts] [samples 64]))

(send* universe (center 'both) (show #true) (maximize #true))
