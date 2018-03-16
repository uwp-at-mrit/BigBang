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

(define draw-curve
  (lambda [dc src curve-nodes point-nodes diameter]
    (send dc set-pen (make-pen #:color "Gainsboro"))
    (send dc draw-lines src)
    (for ([control-point (in-list (take (cdr point-nodes) (- (length point-nodes) 2)))])
      (draw-endpoint dc control-point diameter))
    (send dc set-pen (make-pen #:color "Green"))
    (draw-endpoint dc (car point-nodes) diameter)
    (send dc set-pen (make-pen #:color "Red"))
    (draw-endpoint dc (last point-nodes) diameter)
    (send dc set-pen (make-pen #:color "Black"))
    (send dc draw-lines curve-nodes)))

(define parts
  (for/list ([vertices.txt (in-list (list "deck_bottom.rktl" "deck_top.rktl" "cabin_rt.rktl"))])
    (call-with-input-file* vertices.txt read)))

(define yacht%
  (class canvas% (super-new)
    (init-field parts [diameter 4.0] [max-count 64] [samples #false])

    (define-values (width height) (values 1 1))
    (define sample-count (or samples 128))
    (define pt-transform (void))
    (define images (make-hash))

    (define point-count 2)
    (define curve-count (length parts))
    (define counts (append (range 2 (add1 max-count)) (range max-count 2 -1)))
    (define count-size (length counts))
    (define count-idx 0)

    (define timer-running? #false)
    (define ghostcat
      (make-object timer%
        (thunk (let ([window (send this get-parent)])
                 (set! count-idx (add1 count-idx))
                 (set! point-count (list-ref counts (remainder count-idx count-size)))
                 (send this refresh)))))
  
    (define/override (on-paint)
      (when (procedure? pt-transform)
        (define dc (send this get-dc))
        (for ([seq (in-range curve-count)])
          (define key (list seq point-count width height))
          (define image
            (hash-ref! images key
                       (thunk (thread (thunk (let ([src (list-ref parts seq)])
                                               (define points (vertices-take src point-count))
                                               (define image (send this make-bitmap width height))
                                               (define dc (send image make-dc))
                                               (send dc set-smoothing 'smoothed)
                                               (define-values (curve-nodes point-nodes)
                                                 (compute-curve-points points sample-count pt-transform))
                                               (draw-curve dc (map pt-transform src) curve-nodes point-nodes diameter)
                                               (hash-set! images key image)
                                               (send this refresh)))))))
        
          (cond [(thread? image) (send dc draw-text (format "Drawing curves with ~a control points" point-count) 0 0 #true)]
                [else (send* dc (draw-bitmap image 0 0) (draw-text (format "Control points: ~a" point-count) 0 0 #true))]))))

    (define/override (on-char keyboard)
      (case (send keyboard get-key-release-code)
        [(left) (manually-animate-curves sub1)]
        [(right) (manually-animate-curves add1)]
        [(#\space) (cond [(and timer-running?) (send ghostcat stop)]
                         [else (send ghostcat start (quotient 1000 16))])
                   (set! timer-running? (not timer-running?))]))
    
    (define/override (on-size w h)
      (clear-images)
      (set!-values (width height) (values w h))
      (set! pt-transform (make-point-transform x0 y0 xn yn width height diameter))
      (set! sample-count (or samples (max width height)))
      (send (send this get-parent) set-label (format "Yacht(~a, ~a)" width height)))

    (define/override (on-superwindow-show shown?)
      (unless (and shown?)
        (send ghostcat stop)
        (clear-images)))

    (define/private (clear-images)
      (for ([image (in-hash-values images)])
        (when (thread? image)
          (kill-thread image)))
      (hash-clear! images))

    (define/private (manually-animate-curves op)
      (set! count-idx (op count-idx))
      (when (< count-idx 0) (set! count-idx 0) (bell))
      (set! point-count (list-ref counts (remainder count-idx count-size)))
      (send this refresh))))

(define universe (new frame% [label "Yacht"] [min-width 512] [min-height 512]))
(define yacht (new yacht% [parent universe] [parts parts] [samples 64]))

(send* universe (center 'both) (show #true) (maximize #true))
