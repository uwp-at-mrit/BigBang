#lang racket

(provide (all-defined-out))
(provide (all-from-out pict))

(require racket/date)
(require racket/hash)

(require racket/flonum)
(require racket/fixnum)

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
  (let ([no-pen (make-pen #:style 'transparent)]
        [legend-pen (make-pen #:color (make-color 187 187 187 1.0))]
        [legend-brush (make-brush #:color (make-color 255 255 255 0.618))])
    (lambda [flwidth flheight datasource
                     #:radian0 [r0 #false] #:bytes-fx [fx 0.5] #:bytes-fy [fy 0.5]
                     #:legend-font [legend-font (make-font #:family 'modern #:weight 'bold)]
                     #:label-color [label-color (make-color 0 0 0)]
                     #:%-color [%-color (make-color 106 114 143)]
                     #:total-color [total-color (make-color 0 0 0 0.3)]]
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
              (define total (for/sum ([ds (in-list datasource)]) (vector-ref ds 2)))

              (when (positive? total)
                (define ring (make-object region% #false))
                (define hollow (make-object region% #false))
                (define nonfirst% (- 1.0 (/ (vector-ref (car datasource) 2) total)))
                (send ring set-rectangle dx dy flwidth flheight)
                (send hollow set-ellipse hollow-x hollow-y hollow-diameter hollow-diameter)
                (send ring subtract hollow)
                (send dc set-clipping-region ring)
                
                (define-values (legend-label-width legend-%width legends)
                  (let draw-ring ([rest datasource]
                                  [radian0 (if (flonum? r0) r0 (* nonfirst% pi))]
                                  [max-lwidth 0]
                                  [max-pwidth 0]
                                  [legends null])
                    (cond [(null? rest) (values max-lwidth max-pwidth (reverse legends))]
                          [else (let ([datum (car rest)])
                                  (define datum% (/ (vector-ref datum 2) total))
                                  (define brush (make-brush #:color (rgba (vector-ref datum 1))))
                                  (define radiann (+ radian0 (* datum% pi 2)))
                                  (send dc set-brush brush)
                                  (send dc draw-arc ring-x ring-y ring-diameter ring-diameter radian0 radiann)
                                  
                                  (define label (symbol->string (vector-ref datum 0)))
                                  (define percentage (string-append (~r (* datum% 100) #:precision '(= 2)) "%"))
                                  
                                  (define-values (lwidth _lh _ld _ls) (send dc get-text-extent label legend-font #true))
                                  (define-values (pwidth _ph _pd _ps) (send dc get-text-extent percentage legend-font #true))
                                  (draw-ring (cdr rest) radiann (max lwidth max-lwidth) (max pwidth max-pwidth)
                                             (cons (vector label percentage pwidth brush) legends)))])))
                
                (send dc set-clipping-region #false)
                (define legend-box-x (+ dx legend-off))
                (define legend-box-y (+ dy legend-off))
                (define legend-box-width (+ 1em legend-label-width 1ch legend-%width legend-off legend-off))
                (define legend-box-height (+ legend-off (* lineheight (length datasource))))
                (define-values (legend-x0 legend-y0) (values (+ legend-box-x legend-off) (+ legend-box-y legend-off)))
                (send dc set-pen legend-pen)
                (send dc set-brush legend-brush)
                (send dc draw-rectangle legend-box-y legend-box-y legend-box-width legend-box-height)
                (send dc set-pen no-pen)
                
                (let ([bytes (~size total)])
                  (define-values (total-width total-height _d _s) (send dc get-text-extent bytes legend-font #true))
                  (define tx (+ ring-x (* (- ring-diameter total-width)  fx)))
                  (define ty (+ ring-y (* (- ring-diameter total-height) fy)))
                  (send dc set-text-foreground total-color)
                  (send dc draw-text bytes tx ty #true))
                
                (for ([lgd (in-list legends)]
                      [idx (in-naturals)])
                  (define label (vector-ref lgd 0))
                  (define lbl% (vector-ref lgd 1))
                  (define legend-x (+ legend-x0 1em legend-label-width (- 1ch legend-off) (- legend-%width (vector-ref lgd 2))))
                  (define legend-y (+ legend-y0 (* lineheight idx)))
                  (send dc set-brush (vector-ref lgd 3))
                  (send dc draw-ellipse (+ legend-x0 legend-off) (+ legend-y legend-off) legend-diameter legend-diameter)
                  (send dc set-text-foreground label-color)
                  (send dc draw-text label (+ legend-x0 1em) legend-y #true)
                  (send dc set-text-foreground %-color)
                  (send dc draw-text lbl% legend-x legend-y #true))))
            
            (send* dc
              (set-font saved-font)
              (set-text-foreground saved-color)
              (set-pen saved-pen)
              (set-brush saved-brush)))
          flwidth flheight))))

(define git-loc-series
  (lambda [flwidth flheight datasource
                   #:date0 [date-start #false] #:daten [date-end #false]
                   #:line0 [line-start #false] #:linen [line-end #false]
                   #:line-axis-count [axis-count #false] #:line-peak-ahead-factor [peak-factor 1000]
                   #:mark-font [mark-font (make-font #:family 'system)]
                   #:mark-color [mark-color (make-color #x6A #x73 #x7D)]
                   #:axis-color [axis-color (make-color #x00 #x00 #x00 0.3)]]
    (dc (λ [dc dx dy]
          (define saved-font (send dc get-font))
          (define saved-color (send dc get-text-foreground))
          (define saved-pen (send dc get-pen))
          (define saved-brush (send dc get-brush))

          (send dc set-smoothing 'aligned)
          (send dc set-font mark-font)
        
          (when (ormap pair? (map (λ [lsrc] (vector-ref lsrc 2)) datasource))
            (define 1ch (send dc get-char-width))
            (define 1em (send dc get-char-height))
            (define 1ex (* 1em 1/2))
            
            (define-values (locsource peak x0 xn)
              (for/fold ([src null] [peak 0] [x0 +inf.0] [xn 0])
                        ([lang-src (in-list datasource)])
                (define lang (vector-ref lang-src 0))
                (define pen (make-pen #:color (rgba (vector-ref lang-src 1))))
                (define stats (vector-ref lang-src 2))
                (define-values (date0 daten)
                  (cond [(null? stats) (values x0 xn)]
                        [else (values (caar stats) (car (last stats)))]))
                (define-values (LoCs total)
                  (for/fold ([LoCs null] [total 0])
                            ([stat (in-list stats)])
                    (define total++ (+ total (- (cadr stat) (cddr stat))))
                    (values (cons (cons (car stat) total++) LoCs) total++)))
                (values (cons (vector lang pen (reverse LoCs) total) src)
                        (max peak total)
                        (min x0 date0)
                        (max xn daten))))
              
            (define-values (date0 daten line0 linen)
              (values (or date-start x0)
                      (or date-end xn)
                      (or line-start 0)
                      (or line-end (* (exact-ceiling (/ peak peak-factor)) peak-factor))))

            (define-values (mark-max-width _h _d _s) (send dc get-text-extent (~loc peak) mark-font #true))
            (define x-start (+ dx mark-max-width 1ch))
            (define x-length (- (+ dx flwidth) x-start mark-max-width 1ch))
            (define y-start (- (+ dy flheight) 1ex 1em))
            (define y-length (- y-start dy 1ex))
            (define date-length (- daten date0))
            (define line-length (- linen line0))
            (define date-fraction (/ x-length date-length))
            (define line-fraction (/ y-length line-length))
            
            (send dc set-pen axis-color 1 'solid)
            (send dc set-text-foreground mark-color)

            (let draw-x-axis ([this-endx 0.0]
                              [this-date date0])
              (cond [(<= this-date daten)
                     (let ([the-date (seconds->date this-date)])
                       (define-values (year month) (values (date-year the-date) (date-month the-date)))
                       (define month-starts (find-seconds 0 0 0 1 month year))
                       (define-values (x-axis x-mark)
                         (cond [(= this-date date0) (values this-date (~day (date-day the-date)))]
                               [(= month 1) (values month-starts (number->string year))]
                               [else (values month-starts (~month month))]))
                       (draw-x-axis (draw-x dc (- x-axis date0) x-mark this-endx
                                            mark-font x-start y-start
                                            date-fraction 1ex)
                                    (+ month-starts (* 3600 24 31))))]
                    [else (draw-x dc (- daten date0)
                                  (~day (date-day (seconds->date daten)))
                                  this-endx mark-font x-start y-start
                                  date-fraction 1ex)]))

            (define adjust-count
              (cond [(and (integer? axis-count) (> axis-count 1)) axis-count]
                    [else (exact-ceiling (/ y-length (* 1em 2.0)))]))
            (if (<= adjust-count 1)
                (send* dc
                  (draw-text "0" (- x-start 1ch 1ch) (- y-start 1ex) #true)
                  (draw-line x-start y-start (+ x-start x-length) y-start))
                (for ([y-axis (in-range line0 (+ linen 1) (/ line-length (- adjust-count 1)))])
                  (define y (- y-start (* (- y-axis line0) line-fraction)))
                  (define y-mark (if (zero? y-axis) "0" (~loc y-axis)))
                  (define-values (y-width _w _d _s) (send dc get-text-extent y-mark mark-font #true))
                  (send dc draw-text y-mark (- x-start 1ch y-width) (- y 1ex) #true)
                  (send dc draw-line x-start y (+ x-start x-length) y)))
              
            (for ([loc-src (in-list locsource)])
              (send dc set-pen (vector-ref loc-src 1))
              (send dc set-text-foreground (send (vector-ref loc-src 1) get-color))
              (define this-y-axis (vector-ref loc-src 3))
              (define y (- y-start (* (- this-y-axis line0) line-fraction)))
              (send dc draw-text (~loc this-y-axis) (+ x-start x-length 1ch) (- y 1ex) #true)
              (send dc draw-lines
                    (for/list ([date.LoC (in-list (vector-ref loc-src 2))])
                      (define-values (x-axis y-axis) (values (car date.LoC) (cdr date.LoC)))
                      (cons (+ x-start (* (- x-axis date0) date-fraction))
                            (- y-start (* (- y-axis line0) line-fraction)))))))
            
          (send* dc
            (set-font saved-font)
            (set-text-foreground saved-color)
            (set-pen saved-pen)
            (set-brush saved-brush)))
        flwidth flheight)))

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

(define (draw-x dc x-axis x-mark last-mark-endx mark-font x-start y-start date-fraction 1ex)
  (define x (+ x-start (* x-axis date-fraction)))
  (define-values (x-width x-height _d _s) (send dc get-text-extent x-mark mark-font #true))
  (let ([mark-x (- x (/ x-width 2))])
    (cond [(> mark-x last-mark-endx)
           (send dc draw-line x y-start x (+ y-start 1ex))
           (send dc draw-text x-mark mark-x (+ y-start 1ex) #true)
           (+ mark-x x-width)]
          [else mark-x])))

(define hex->rgb-bytes
  (lambda [rgb]
    (values (fxand (fxrshift rgb 16) #xFF)
            (fxand (fxrshift rgb 8) #xFF)
            (fxand rgb #xFF))))

(define rgba
  (lambda [src]
    (cond [(symbol? src) (make-object color% (symbol->string src))]
          [(exact-integer? src) (let-values ([(r g b) (hex->rgb-bytes src)]) (make-color r g b))]
          [(is-a? src color%) src]
          [else (make-object color% (~a src))])))

(define units '(KB MB GB TB))
(define ~size
  (lambda [size [unit 'Bytes] #:precision [prcs '(= 3)]]
    (if (eq? unit 'Bytes)
        (cond [(< -1024.0 size 1024.0) (~a (exact-round size) "Bytes")]
              [else (~size (fl/ (real->double-flonum size) 1024.0) 'KB #:precision prcs)])
        (let try-next-unit ([s (real->double-flonum size)] [us (memq unit units)])
          (cond [(or (fl< (flabs s) 1024.0) (null? (cdr us))) (string-append (~r s #:precision prcs) (symbol->string (car us)))]
                [else (try-next-unit (fl/ s 1024.0) (cdr us))])))))

(define ~loc
  (lambda [loc]
    (cond [(< loc 1000) (number->string loc)]
          [else (string-replace (~r (/ loc 1000) #:precision '(= 3)) "." ",")])))

(define ~month
  (lambda [m]
    (cond [(< m 10) (string-append "0" (number->string m))]
          [else (number->string m)])))

(define ~day
  (lambda [d]
    (format "~~~a" d)))
