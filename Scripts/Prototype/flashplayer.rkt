#lang racket/gui

(provide (all-defined-out))

(define flashplayer%
  (class* frame% []
    (init-field fps draw-frame width height margin)
    (init-field [caption "Dynamic Curves"])

    (super-new [label caption] [stretchable-width #false] [stretchable-height #false]
               [min-width (+ width margin margin)]
               [min-height (+ height margin margin)])

    (define times 0)
    (define canvas (make-object canvas% this null (λ [self dc] (send dc draw-bitmap frame margin margin))))
    (define frame (send canvas make-bitmap width height))
    (define frame-dc (send frame make-dc))
    (define saved-color (send frame-dc get-text-foreground))
    (define info-color (make-object color% "Violet"))
    (define info-y (- height (send frame-dc get-char-height)))
    (define timer
      (make-object timer%
        (λ []
          (set! times (add1 times))
          (send frame-dc clear)
          (define-values (_ cpu real gc) (time-apply (λ [] (draw-frame frame-dc times)) null))
          (send frame-dc set-text-foreground info-color)
          (send frame-dc draw-text (format "cpu time: ~a real time: ~a gc time: ~a" cpu real gc) 0 info-y #true)
          (send frame-dc set-text-foreground saved-color)
          (send canvas refresh))))

    (define/override (on-superwindow-show shown?)
      (cond [(not shown?) (send timer stop)]
            [else (send frame-dc set-smoothing 'smoothed)
                  (send timer start (exact-round (/ 1000 fps)))]))))
