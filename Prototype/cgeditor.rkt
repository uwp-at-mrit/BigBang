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

(define frame (new frame% [label "CGEditor 原型"] [width 800] [height 600]))
;(define titlebar (new horizontal-panel% [parent frame] [style '(border)] [stretchable-height #false]))
(define workspace (new horizontal-panel% [parent frame] [alignment '(left top)]))
(define controlbar (new horizontal-panel% [parent frame] [style '(border)] [stretchable-height #false] [alignment '(right center)]))

;(define methods (make-object radio-box% #false '("拉伸" "居中") titlebar void '(horizontal)))
;(define resolutions (make-object radio-box% #false '("800x600" "1024x768" "1920x1024") workspace void))
(define save-svg.btn (make-object button% "保存成 SVG" controlbar (λ [b e] (save-as-svg stage 800 600 "stage.svg"))))
(define save-pdf.btn (make-object button% "保存成 PDF" controlbar (λ [b e] (save-as-pdf stage "stage.pdf"))))
(define print.btn (make-object button% "打印" controlbar (λ [b e] (send stage print))))
(define exit.btn (make-object button% "退出" controlbar (λ [b e] (send frame show #false))))

(define cgtools (make-object toolbar-pasteboard% 'vertical 5))
(define stage (make-object pasteboard%))

(define toolbar (instantiate editor-canvas% (workspace cgtools '(no-vscroll no-hscroll)) [min-width 78] [stretchable-width #false]))
(define monitor (make-object editor-canvas% workspace stage '(auto-vscroll auto-hscroll)))

(send* frame (show #true) (center 'both))

(send cgtools insert (make-object toolbar-snip% (alarmlet 64) (λ _ (send stage insert (alarmlet 128)))))
(send cgtools insert (make-object toolbar-snip% (buttonlet "按钮" 64) (λ _ (send stage insert (buttonlet "按钮" 100)))))
(send cgtools insert (make-object toolbar-snip% (scale-by-width (textlet "文本框") 64) (λ _ (send stage insert (textlet "文本框")))))
;(send cgtools insert (make-object toolbar-snip% (bitmaplet (stopwatch-icon #:height 64)) (λ _ (displayln 'bitmap))))
