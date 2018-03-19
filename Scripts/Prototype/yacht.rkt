#lang racket/gui

(require typed/opengl)

(require "bezier.rkt")

(define-values (samples window-size) (values 64 512))
(define-values (x0 y0 xn yn) (values 528467.29464027 -62041.37096839 571267.29464027 -42041.37096839))

(define cfg (make-object gl-config%))
(send* cfg
  (set-multisample-size 4)
  (set-stencil-size 1)
  (set-hires-mode #t))

(define parts
  (for/list ([vertices.txt (in-list (list "deck_bottom.rktl" "deck_top.rktl" "cabin_rt.rktl"))])
    (call-with-input-file* vertices.txt read)))

(define vertices-take
  (lambda [vertices count]
    (define total (length vertices))
    (define last-idx (sub1 total))
    (define step (quotient total count))
    (let select ([vs (list (car vertices))] [i (+ 1 step)])
      (cond [(>= i last-idx) (reverse (cons (list-ref vertices last-idx) vs))]
            [else (select (cons (list-ref vertices i) vs) (+ i step))]))))

(define (make-point-transform x0 y0 xn yn width height margin)
  (define-values (x-range y-range) (values (- width margin margin) (- height margin margin)))
  (define-values (x-start y-start) (values (/ (- width x-range) 2) (- height margin)))
  (define-values (x-ratio y-ratio) (values (/ x-range (- xn x0)) (/ y-range (- yn y0))))
  (lambda [p]
    (cons (+ x-start (* (- (real-part p) x0) x-ratio))
          (- y-start (* (- (imag-part p) y0) y-ratio)))))

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

(define yacht%
  (class canvas% (super-new [gl-config cfg] [style '(gl no-autoclear)])
    (init-field parts [diameter 8.0] [max-count 64] [samples #false])

    (define-values (width height) (values 1 1))
    (define sample-count (or samples 128))
    (define pt-transform (void))
    (define vertices (make-hash))

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
                 (send this with-gl-context (thunk (setup-yacht-viewport)))
                 (send this refresh)))))
  
    (define/override (on-paint)
      (when (procedure? pt-transform)
        (send this with-gl-context
              (lambda []
                (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT GL_STENCIL_BUFFER_BIT))
                (for ([seq (in-range curve-count)])
                  (define key (list seq point-count width height))
                  (let ([src (list-ref parts seq)])
                    (define points (vertices-take src point-count))
                    (define-values (curve-nodes point-nodes) (compute-curve-points points sample-count pt-transform))
                    #;(hash-set! points key (cons curve-nodes point-nodes))
                    #;(send this refresh)
                    (void))
                
                  #;(cond [(thread? image) (send dc draw-text (format "Drawing curves with ~a control points" point-count) 0 0 #true)]
                          [else (send* dc (draw-bitmap image 0 0) (draw-text (format "Control points: ~a" point-count) 0 0 #true))]))
                
                (glFlush)
                (send this swap-gl-buffers)))))

    (define/override (on-char keyboard)
      (case (send keyboard get-key-release-code)
        [(left) (manually-animate-curves sub1)]
        [(right) (manually-animate-curves add1)]
        [(#\space) (cond [(and timer-running?) (send ghostcat stop)]
                         [else (send ghostcat start (quotient 1000 16))])
                   (set! timer-running? (not timer-running?))]))
    
    (define/override (on-size w h)
      (send this with-gl-context
            (lambda ()
              (when (void? pt-transform)
                (glShadeModel GL_SMOOTH)
                (glEnable GL_MULTISAMPLE)
                (glEnable GL_DEPTH_TEST))
              
              (set!-values (width height) (send this get-gl-client-size))
              (glViewport 0 0 width height)
              (glClearColor 1.0 1.0 1.0 1.0)
              (setup-yacht-viewport)
              (set! pt-transform (make-point-transform x0 y0 xn yn width height diameter))
              (set! sample-count (or samples (max width height)))
              (send (send this get-parent) set-label (format "Yacht[OpenGL(~a, ~a)]" width height))
              (send this focus))))

    (define/override (on-superwindow-show shown?)
      (unless (and shown?)
        (send ghostcat stop)))

    (define/private (manually-animate-curves op)
      (set! count-idx (op count-idx))
      (when (< count-idx 0) (set! count-idx 0) (bell))
      (set! point-count (list-ref counts (remainder count-idx count-size)))
      (send this with-gl-context (thunk (setup-yacht-viewport)))
      (send this refresh))

    (define/private (setup-yacht-viewport)
      ;; projection matrix
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (if (< width height)
          (let ([h (/ height width)])
            (glFrustum -1.0 1.0 (- h) h 5.0 60.0))
          (let ((h (/ width height)))
            (glFrustum (- h) h -1.0 1.0 5.0 60.0)))
      
      ;; modelview matrix
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (glTranslatef 0.0 0.0 -40.0))))


(module+ main
  (define universe (new frame% [label "Yacht"]))
  (define yacht (new yacht% [parent universe] [parts parts] [samples 64]))

  (send* universe (center 'both) (show #true) (maximize #true)))
