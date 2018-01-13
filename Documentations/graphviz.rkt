#lang racket

(provide (all-defined-out))
(provide (all-from-out pict))

(require racket/flonum)
(require racket/draw)
(require pict)

(require plot/no-gui)

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
  (let ([no-pen (make-pen #:style 'transparent)]
        [legend-pen (make-pen #:color (make-color 187 187 187 1.0))]
        [legend-brush (make-brush #:color (make-color 255 255 255 0.618))])
    (lambda [flwidth flheight datasource
                     #:legend-font [legend-font (make-font #:family 'modern #:weight 'bold)]
                     #:label-color [lbl-clr (make-color 0 0 0)]
                     #:%-color [clr% (make-color 106 114 143)]
                     #:radian0 [r0 0.0]]
      (dc (λ [dc dx dy]
            (define saved-font (send dc get-font))
            (define saved-color (send dc get-text-foreground))
            (define saved-pen (send dc get-pen))
            (define saved-brush (send dc get-brush))

            (send dc set-smoothing 'aligned)
            (send dc set-font legend-font)
            (send dc set-pen no-pen)
      
            (when (pair? datasource)
              (define 1ch (send dc get-char-width))
              (define 1em (send dc get-char-height))
              (define lineheight (* 1em 1.2))
              (define ring-width (* 1em pi))
              (define legend-diameter (* 1em 0.618))
              (define legend-off (* (- 1em legend-diameter) 1/2))
              (define ring-diameter (- (min flwidth flheight) 1.0))
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

              (define-values (legend-width-hint legends)
                (let draw-ring ([rest datasource]
                                [radian0 r0]
                                [max-width 0]
                                [legends null])
                  (cond [(null? rest) (values max-width (reverse legends))]
                        [else (let ([datum (car rest)])
                                (define datum% (/ (vector-ref datum 1) total))
                                (define color (vector-ref datum 2))
                                (define brush (make-brush #:color (if (symbol? color) (symbol->string color) color)))
                                (define radiann (+ radian0 (* datum% pi 2)))
                                (send dc set-brush brush)
                                (send dc draw-arc ring-x ring-y ring-diameter ring-diameter radian0 radiann)

                                (define label (symbol->string (vector-ref datum 0)))
                                (define percentage (string-append (~r (* datum% 100) #:precision '(= 2)) "%"))

                                (define-values (l-width _lh _ld _ls) (send dc get-text-extent label legend-font #true))
                                (define-values (p-width _ph _pd _ps) (send dc get-text-extent percentage legend-font #true))
                                (draw-ring (cdr rest) radiann (max (+ l-width p-width) max-width)
                                           (cons (vector label percentage brush l-width) legends)))])))

              (send dc set-clipping-region #false)
              (define legend-box-x (+ dx legend-off))
              (define legend-box-y (+ dy legend-off))
              (define legend-box-width (+ 1em 1ch legend-width-hint legend-off legend-off))
              (define legend-box-height (+ legend-off (* lineheight (length datasource))))
              (define-values (legend-x0 legend-y0) (values (+ legend-box-x legend-off) (+ legend-box-y legend-off)))
              (send dc set-pen legend-pen)
              (send dc set-brush legend-brush)
              (send dc draw-rectangle legend-box-y legend-box-y legend-box-width legend-box-height)
              (send dc set-pen no-pen)

              (for ([lgd (in-list legends)]
                    [idx (in-naturals)])
                (define label (vector-ref lgd 0))
                (define lbl% (vector-ref lgd 1))
                (define legend-y (+ legend-y0 (* lineheight idx)))
                (define label-width (vector-ref lgd 3))
                (send dc set-brush (vector-ref lgd 2))
                (send dc draw-ellipse (+ legend-x0 legend-off) (+ legend-y legend-off) legend-diameter legend-diameter)
                (send dc set-text-foreground lbl-clr)
                (send dc draw-text label (+ legend-x0 1em) legend-y #true)
                (send dc set-text-foreground clr%)
                (send dc draw-text lbl% (+ legend-x0 1em label-width 1ch) legend-y))
          
              (let ([bytes (~size total)])
                (define-values (total-width total_height _d _s) (send dc get-text-extent bytes legend-font #true))
                (define-values (tx ty) (values (- cx (/ total-width 2)) (- cy (/ total_height 2))))
                (send dc draw-text bytes tx ty #true)))

            
            (send* dc
              (set-font saved-font)
              (set-text-foreground saved-color)
              (set-pen saved-pen)
              (set-brush saved-brush)))
          flwidth flheight))))

(define git-time-series
  (lambda [datasource]
    (define dates (sort (hash-keys datasource) <))
    (define-values (xmin xmax) (values (car dates) (last dates)))
    (define addition
      (for/fold ([addition (list (vector 0 0))]) ([i (in-list dates)])
        (define datum (hash-ref datasource i))
        (cons (vector i (+ (- (car datum) (cdr datum)) (vector-ref (car addition) 1))) addition)))
    (vl-append (text (format "[~a, ~a]" xmin xmax))
               (plot-pict #:x-min xmin #:x-max xmax #:width 600
                          (lines #:color "Green" #:label "insertion"
                                 (cdr (reverse addition)))))))

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
