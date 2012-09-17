; jellyfish

(clear)

; store pinch as it's slow to calculate
(define pinch 1)
(define last-touch-pos (vector 0 0 0))
(define world-pos (vector 0 0 0))
(define world-zoom 1)
(define first-zoom #t)
(define first-move #t)

(define root (with-state
              ;(translate (vector 1 2 3))
              (build-locator)))

(define (build-plane)
  (let ((p (build-polygons 4 1)))
    (with-primitive 
     p
     (pdata-set! "p" 0 (vector 0 0 0))
     (pdata-set! "p" 1 (vector 0 1 0))
     (pdata-set! "p" 2 (vector 1 0 0))
     (pdata-set! "p" 3 (vector 1 1 0))
     (pdata-set! "t" 0 (vector 0 0 0))
     (pdata-set! "t" 1 (vector 0 1 0))
     (pdata-set! "t" 2 (vector 1 0 0))
     (pdata-set! "t" 3 (vector 1 1 0))
     (pdata-map! (lambda (n) (vector 0 0 -1)) "n")
     (pdata-map! (lambda (n) (vector 1 1 1)) "c"))
    p))

(define (check-objs l fn)
  (let* ((line (get-line-from-mouse))
         (s (with-primitive 
             root 
             (get-line-intersect (car line) (cadr line)))))
    (when (not (zero? s))
          (fn s))))

(define (update-input objs fn)
  (if (eqv? (length (get-touch-ids)) 1)
      (begin
        (let* ((ppos (vmul 
                      (get-point-from-touch 
                       (car (get-touch-ids)))
                      (/ 0.2 world-zoom)))
               (pos (vector (vx ppos) (vy ppos) 0)))
          (if (not first-move)
              (set! world-pos (vadd world-pos
                                    (vsub pos last-touch-pos)))
              (check-objs objs fn))
          (set! last-touch-pos pos))
        (set! first-move #f))
      (set! first-move #t))
  
  (if (> (length (get-touch-ids)) 1)
      (begin
        (let ((p (* (vdist 
                     (get-point-from-touch (car (get-touch-ids)))
                     (get-point-from-touch (cadr (get-touch-ids))))
                    0.1)))
          (when (not first-zoom)
                (set! world-zoom (+ world-zoom (- p pinch))))
          (when (< world-zoom 0.1) (set! world-zoom 0.1))
          (set! pinch p)
          (set! first-zoom #f)
          ))
      (set! first-zoom #t))
  
  (with-primitive
   root
   (identity)
   (scale (vector world-zoom world-zoom world-zoom))
   (translate world-pos))  
  )

(clear-colour (vector 0.4 0.2 0))
(colour (vector 1 0 0))
;(hint-unlit)

(define objs 
  (map
   (lambda (i)
     (with-state
      (parent root)
      (translate (vsub (vector (modulo i 3)
                               (quotient i 3) 2)
                       (vector 2 2 0)))
      (with-state 
       (texture (load-texture "icons.png"))
       (colour (vector 1 0 0 0.3))
       (scale (vector 0.9 0.9 1))
       (build-plane))))
   (build-list (lambda (i) i) 100)))

(define pp (with-state
            (texture (load-texture "squib.png"))
            (translate (vector -2 2 0))
            (colour (vector 1 1 1 1))
            (build-cube)))

(define (render)
    (update-input 
     objs
     (lambda (o)
       (with-primitive o (colour (rndvec)))))

    (with-primitive pp
                    (rotate (vector 1 2 3)))
    )

(with-state
 (parent root)
 (translate (vector 0 1 0))
 (build-cube))

                   
(every-frame (render))


