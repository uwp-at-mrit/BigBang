#lang racket/gui

(provide (all-defined-out))

(require pict/snip)

(define hand.cur (make-object cursor% 'hand))

(define toolbar-pasteboard%
  (class pasteboard% (super-new)
    (init-field alignment [gapsize 0])

    (send* this
      (set-selection-visible #false)
      (auto-wrap #true))

    (inherit get-admin get-dc get-snip-location move-to move find-snip)
    (inherit editor-location-to-dc-location local-to-global global-to-local)

    (define/augment (can-interactive-move? mouse) (inner #false can-interactive-move? mouse))
    (define/augment (can-interactive-resize? mouse) (inner #false can-interactive-resize? mouse))
    (define/augment (can-load-file? filename format) (inner #false can-load-file? filename format))
    (define/augment (can-save-file? filename format) (inner #false can-save-file? filename format))

    (define/public (get-snip-rectangle snip #:global? [dc-coordinate? #false])
      (define &x (box 0.0))
      (define &y (box 0.0))
      (define &width (box 0.0))
      (define &height (box 0.0))
      (get-snip-location snip &x &y) ; TODO: why must call (get-extent)?
      (send snip get-extent (get-dc) (unbox &x) (unbox &y) &width &height #false #false #false #false)
      (when dc-coordinate? (local-to-global &x &y))
      (values (real->double-flonum (unbox &x)) (real->double-flonum (unbox &y))
              (real->double-flonum (unbox &width)) (real->double-flonum (unbox &height))))

    (define/override (adjust-cursor mouse)
      (define maybe-cursor
        (when (get-admin)
          (define &editor-x (box #| initiated with global x |# (send mouse get-x)))
          (define &editor-y (box #| initiated with global y |# (send mouse get-y)))  
          (global-to-local &editor-x &editor-y)
          (define maybe-snip (find-snip (unbox &editor-x) (unbox &editor-y)))
          (unless (false? maybe-snip)
            (get-snip-location maybe-snip &editor-x &editor-y)
            (define-values (dc-x dc-y) (editor-location-to-dc-location (unbox &editor-x) (unbox &editor-y)))
            (send maybe-snip adjust-cursor (get-dc) dc-x dc-y (unbox &editor-x) (unbox &editor-y) mouse))))
      (if (void? maybe-cursor) (super adjust-cursor mouse) maybe-cursor))
    
    (define/override (on-default-char keyboard)
      (unless (memq (send keyboard get-key-code)
                    '(left right up down #\backspace #\rubout))
        (super on-default-char keyboard)))
    
    (define/augment (after-insert snip before x y)
      (case alignment
        [(vertical)
         (if (false? before)
             (let ([last-one (send snip previous)])
               (cond [(false? last-one) (move-to snip 0 0)]
                     [else (let-values ([(_x last-y _w last-height) (get-snip-rectangle last-one)])
                             (move-to snip 0 (+ last-y last-height gapsize)))]))
             (let-values ([(snip-x snip-y _w _h) (get-snip-rectangle before)])
               (move-to snip snip-x snip-y)
               (define-values (_x _y _w snip-height) (get-snip-rectangle snip))
               (let move-down ([this-snip before])
                 (unless (false? this-snip)
                   (move this-snip 0 (+ snip-height gapsize))
                   (move-down (send this-snip next))))))]
        [(horizontal)
         (if (false? before)
             (let ([last-one (send snip previous)])
               (cond [(false? last-one) (move-to snip 0 0)]
                     [else (let-values ([(last-x _y last-width _h) (get-snip-rectangle last-one)])
                             (move-to snip (+ last-x last-width gapsize) 0))]))
             (let-values ([(snip-x snip-y _w _h) (get-snip-rectangle before)])
               (move-to snip snip-x snip-y)
               (define-values (_x _y snip-width _h) (get-snip-rectangle snip))
               (let move-right ([this-snip before])
                 (unless (false? this-snip)
                   (move this-snip (+ snip-width gapsize) 0)
                   (move-right (send this-snip next))))))])
      (inner (void) after-insert snip before x y))
    
    (define/augment (after-delete snip)
      (define ?next (send snip next))
      (unless (false? ?next)
        (case alignment
          [(vertical)
           (define-values (_x _y _w snip-height) (get-snip-rectangle snip))
           (let move-up ([this-snip ?next])
             (unless (false? this-snip)
               (move this-snip 0 (- (+ snip-height gapsize)))
               (move-up (send this-snip next))))]
          [(horizontal)
           (define-values (_x _y snip-width _h) (get-snip-rectangle snip))
           (let move-left ([this-snip ?next])
             (unless (false? this-snip)
               (move this-snip (- (+ snip-width gapsize)) 0)
               (move-left (send this-snip next))))]))
      (inner (void) after-delete snip))))

(define toolbar-snip%
  (class pict-snip% (init pict) (super-make-object pict)
    (init-field on-click)
    
    (send this set-flags (cons 'handles-all-mouse-events (send this get-flags)))

    (define/override (adjust-cursor dc x y ex ey mouse)
      hand.cur)
    
    (define/override (on-event dc x y ex ey mouse)
      (when (send mouse button-up?)
        (on-click dc x y ex ey mouse)))))
