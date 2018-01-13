#lang racket

(provide (all-defined-out))

(require racket/flonum)
(require racket/draw)
(require pict)

(define filesystem-tree
  (let ([1em (pict-height (text ""))] [phantom (blank 0)])
    (lambda [tree #:value-pict [value->pict filesystem-value->pict]
                  #:padding-space [gapsize 8] #:padding-x [offset 12] #:padding-box [delta 8]
                  #:dir-color [cdir 'LightSkyBlue] #:file-color [cfile 'Ghostwhite] #:line-color [cline 'Gainsboro]]
      (define yoffset (* 0.5 (+ delta 1em)))
      (define xy-find (λ [p f] (let-values ([(x y) (lt-find p f)]) (values (+ x offset) (+ y yoffset)))))
      (define (leaf->pict v color)
        (define content (value->pict v))
        ((curryr cc-superimpose content)
         (filled-rounded-rectangle #:border-color (~a cline) #:color (~a color)
                                   (+ delta delta (pict-width content))
                                   (+ delta (pict-height content)))))
      (define (branch->pict nodes)
        (for/fold ([root (leaf->pict (car nodes) cdir)])
                  ([subtree (in-list (cdr nodes))])
          (define body (filesystem-tree subtree #:value-pict value->pict
                                        #:padding-space gapsize #:padding-x offset #:padding-box delta
                                        #:dir-color cdir #:file-color cfile #:line-color cline))
          (define child (pin-line #:under? #true #:color (~a cline)
                                  (ht-append (* 4 gapsize) phantom body)
                                  phantom xy-find body xy-find))
          (pin-line #:under? #true #:color (~a cline)
                    (vl-append gapsize root child)
                    root xy-find child xy-find)))
      (cond [(null? tree) phantom]
            [(list? tree) (branch->pict tree)]
            [else (leaf->pict tree cfile)]))))

(define pie-chart
  (let ([2pi (* 2 pi)])
    (lambda [flwidth flheight datasource
                     #:legend-font [legend-font (make-font #:family 'modern #:weight 'bold)]
                     #:label-color [lbl-clr (make-color 0 0 0)]
                     #:%-color [clr% (make-color 106 114 143)]]
      (dc (λ [dc dx dy]
            (define saved-font (send dc get-font))
            (define saved-color (send dc get-text-foreground))
            (define saved-pen (send dc get-pen))
            (define saved-brush (send dc get-brush))

            (send dc set-smoothing 'aligned)
            (send dc set-font legend-font)
            (send dc set-pen lbl-clr 0 'transparent)
      
            (when (pair? datasource)
              (define 1ch (send dc get-char-width))
              (define 1em (send dc get-char-height))
              (define lineheight (* 1em 1.2))
              (define ring-width (* 1em pi))
              (define legend-diameter (* 1em 0.618))
              (define legend-off (* (- 1em legend-diameter) 1/2))
              (define ring-diameter (min flwidth flheight))
              (define hollow-diameter (- ring-diameter ring-width))
              (define hollow-off (/ (- ring-diameter hollow-diameter) 2))
              (define-values (ring-x ring-y) (values (+ dx (- flwidth ring-diameter)) (+ dy (/ (- flheight ring-diameter) 2))))
              (define-values (hollow-x hollow-y) (values (+ ring-x hollow-off) (+ ring-y hollow-off)))
              (define-values (cx cy) (let ([r (/ ring-diameter 2)]) (values (+ ring-x r) (+ ring-y r))))
              (define total (for/sum ([ds (in-list datasource)]) (vector-ref ds 1)))
              (define ring (make-object region% #false))
              (define hollow (make-object region% #false))
              (send ring set-rectangle dx dy flwidth flheight)
              (send hollow set-ellipse hollow-x hollow-y hollow-diameter hollow-diameter)
              (send ring subtract hollow)
              (send dc set-clipping-region ring)
              (let draw-data ([ds (car datasource)]
                              [rest (cdr datasource)]
                              [radian0 ring-diameter]
                              [legend-y dy])
                (define label (symbol->string (vector-ref ds 0)))
                (define d% (/ (vector-ref ds 1) total))
                (define color (let ([c (vector-ref ds 2)]) (if (symbol? c) (symbol->string c) c)))
                (define lbl% (string-append (~r (* d% 100) #:precision '(= 2)) "%"))
                (define radiann (+ radian0 (* 2pi d%)))
                (define-values (label-width _h _d _s) (send dc get-text-extent label legend-font #true))
                (send dc set-brush color 'solid)
                (send dc draw-arc ring-x ring-y ring-diameter ring-diameter radian0 radiann)
                (send dc draw-ellipse (+ dx legend-off) (+ legend-y legend-off) legend-diameter legend-diameter)
                (send dc set-text-foreground lbl-clr)
                (send dc draw-text label (+ dx 1em) legend-y #true)
                (send dc set-text-foreground clr%)
                (send dc draw-text lbl% (+ dx 1em label-width 1ch) legend-y)
          
                (when (pair? rest)
                  (draw-data (car rest) (cdr rest)
                             radiann (+ legend-y lineheight))))

              (let ([bytes (~size total)])
                (define-values (total-width total_height _d _s) (send dc get-text-extent bytes legend-font #true))
                (define-values (tx ty) (values (- cx (/ total-width 2)) (- cy (/ total_height 2))))
                (send dc set-clipping-region #false)
                (send dc draw-text bytes tx ty #true)))

            
            (send* dc
              (set-font saved-font)
              (set-text-foreground saved-color)
              (set-pen saved-pen)
              (set-brush saved-brush)))
          flwidth flheight))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define filesystem-value->pict
  (let ([substyle '(subscript large-script)]
        [ftext (lambda [t [s null] [c #false]]
                 (define color (send the-color-database find-color (~a c)))
                 (text (~a t) (if color (cons color s) s) 12))])
    (letrec ([value->pict (match-lambda
                            [(or (box filename) (vector filename))
                             (ftext filename '(italic))]
                            [(or (cons filename info) (vector filename info))
                             (hc-append 4 (value->pict filename) (ftext info substyle 'ForestGreen))]
                            [(vector filename info color)
                             (ht-append 4 (value->pict filename) (ftext info substyle color))]
                            [val (ftext val)])])
      value->pict)))

(define units '(KB MB GB TB))
(define ~size
  (lambda [size [unit 'Bytes] #:precision [prcs '(= 3)]]
    (if (eq? unit 'Bytes)
        (cond [(< -1024.0 size 1024.0) (~a (exact-round size) "Bytes")]
              [else (~size (fl/ (real->double-flonum size) 1024.0) 'KB #:precision prcs)])
        (let try-next-unit ([s (real->double-flonum size)] [us (memq unit units)])
          (cond [(or (fl< (flabs s) 1024.0) (null? (cdr us))) (string-append (~r s #:precision prcs) (symbol->string (car us)))]
                [else (try-next-unit (fl/ s 1024.0) (cdr us))])))))
