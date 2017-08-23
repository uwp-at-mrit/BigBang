#lang racket/gui

(require "digitama/shape.rkt")
(require "digitama/ui.rkt")

(define (save-as-svg stage width height path)
  (define path.svg (build-path (find-system-path 'desk-dir) path))
  (define svg (make-object svg-dc% width height path.svg 'truncate/replace))
  (send svg start-doc "CG Editor Prototype")
  (send stage print-to-dc svg)
  (send svg end-doc)
  (system (string-append "open " (path->string path.svg))))

(define (save-as-pdf stage path)
  (define path.pdf (build-path (find-system-path 'desk-dir) path))
  (define pdf (new pdf-dc% [interactive #true] [output path.pdf]))
  (when (send pdf ok?)
    (send pdf start-doc "CG Editor Prototype")
    (send stage print-to-dc pdf)
    (send pdf end-doc)
    (system (string-append "open " (path->string path.pdf)))))

(define (preview-stage self dc)
  (define-values (&W &H) (values (box 0) (box 0)))
  (define-values (w h) (send self get-client-size))
  (send stage get-view-size &W &H)
  
  (define-values (sw sh) (values (/ w (unbox &W)) (/ h (unbox &H))))
  (send dc set-alpha 1.0)
  (send dc set-scale sw sh)
  (send stage print-to-dc dc 0)
  
  (send dc set-scale 1.0 1.0)
  (send dc set-alpha 0.32)
  (define text-height (send dc get-char-height))
  (send dc draw-text (format "Original Size: ~a x ~a" (unbox &W) (unbox &H)) 0 (- h (* text-height 3)))
  (send dc draw-text (format "Monitor Size: ~a x ~a" w h) 0 (- h (* text-height 2)))
  (send dc draw-text (format "Scaling factor: ~a ~a" sw sh) 0 (- h (* text-height 1))))

(define cgeditor (new frame% [label "CGEditor Prototype"] [width 800] [height 600]))
(define cgpreview (new frame% [label "Monitor"] [width 400] [height 300]))

(define workspace (new horizontal-panel% [parent cgeditor] [alignment '(left top)]))
(define controlbar (new horizontal-panel% [parent cgeditor] [style '(border)] [stretchable-height #false] [alignment '(right center)]))

(define preview.btn (make-object button% "Preview" controlbar (λ [b e] (send* cgpreview (show #true) (center 'both)))))
(define save-svg.btn (make-object button% "Save as SVG" controlbar (λ [b e] (save-as-svg stage 800 600 "stage.svg"))))
(define save-pdf.btn (make-object button% "Save as PDF" controlbar (λ [b e] (save-as-pdf stage "stage.pdf"))))
(define print.btn (make-object button% "Print" controlbar (λ [b e] (send stage print))))
(define exit.btn (make-object button% "Quit" controlbar (λ [b e] (send cgeditor show #false))))

(define cgtools (make-object toolbar-pasteboard% 'vertical 5))
(define stage (make-object pasteboard%))

(define toolbar (instantiate editor-canvas% (workspace cgtools '(no-vscroll no-hscroll)) [min-width 78] [stretchable-width #false]))
(define monitor (make-object editor-canvas% workspace stage '(auto-vscroll auto-hscroll)))
(define preview (make-object canvas% cgpreview '(border) preview-stage))

(send* cgeditor (show #true) (center 'both))

(send cgtools insert (make-object toolbar-snip% (alarmlet 64) (λ _ (send stage insert (alarmlet 128)))))
(send cgtools insert (make-object toolbar-snip% (buttonlet "Button" 64) (λ _ (send stage insert (buttonlet "Button" 100)))))
(send cgtools insert (make-object toolbar-snip% (scale-by-width (textlet "Text") 64) (λ _ (send stage insert (textlet "Text Input")))))
;(send cgtools insert (make-object toolbar-snip% (bitmaplet (stopwatch-icon #:height 64)) (λ _ (displayln 'bitmap))))
