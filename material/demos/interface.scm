
(define NOP 0)
(define JMP 1)
(define JMZ 2)
(define JLT 3)
(define JGT 4)
(define LDL 5)
(define LDA 6)
(define LDI 7)
(define STA 8)
(define STI 9)
(define ADD 10)
(define SUB 11)
(define MUL 12)
(define DIV 13)
(define ABS 14)
(define SIN 15)
(define ATN 16)
(define DOT 17)
(define CRS 18)
(define SQR 19)
(define LEN 20)
(define DUP 21)
(define CMP 22)
(define SHF 23)
(define BLD 24)
(define RET 25)

(define REG_PCO 100)
(define REG_SPD 101)
(define REG_POS 102)
(define REG_VEL 103)
(define REG_COL 104)
(define REG_NIT 105)
(define REG_SCC 106)
(define REG_SDR 107)
(define REG_AND 108)
(define REG_MDL 120)
(define REG_MDL_END 199)
(define REG_STP 200)
(define REG_STK 201)
(define REG_NDT 256)

(define (program-jellyfish l)
  (for-each
   (lambda (v p)
     (pdata-set! "p" p v))
   l (build-list (lambda (i) i) (length l))))


(clear-colour (vector 1 1 1))


;(define p (build-cube))

(clear)

;(with-state
; (rotate (vector 45 45 0))
; (build-cube))

;(every-frame
; (with-primitive jelly (rotate (vector 2.2 2 1))))


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

(define icon-count-x 5)
(define icon-count-y 10)
(define icon-width (/ 1 icon-count-x))
(define icon-height (/ 1 icon-count-y))

(define (icon-tex-coord i)
  (vector (/ (modulo i icon-count-x) icon-count-x)
          (/ (quotient i icon-count-x) icon-count-y)
          0))

(define objs 
  (map
   (lambda (i)
     (let ((p (with-state
               (parent root)
               (translate (vsub (vector (modulo i 3)
                                        (quotient i 3) 2)
                                (vector 2 2 0)))
               (with-state 
                (hint-unlit)
                (texture (load-texture "icons.png"))
                ;(colour (vector 1 0 0 0.3))
                (scale (vector 0.9 0.9 1))
                (build-plane)))))
       (with-primitive 
        p
        (let ((t (icon-tex-coord i)))
          (pdata-set! "t" 3 t)
          (pdata-set! "t" 1 (vadd t (vector icon-width 0 0)))
          (pdata-set! "t" 2 (vadd t (vector 0 icon-height 0)))
          (pdata-set! "t" 0 (vadd t (vector icon-width icon-height 0)))))
        p))
   (build-list (lambda (i) i) 100)))

(define pp (with-state
            (texture (load-texture "squib.png"))
            (translate (vector -2 2 0))
            (rotate (vector 45 34 2))
            (colour (vector 1 1 1 1))
            (build-cube)))

(define (render)
    (update-input 
     objs
     (lambda (o)
       (with-primitive o (colour (rndvec)))))

    ;(with-primitive pp
    ;                (rotate (vector 1 2 3)))
    )

(define jelly (build-jellyfish))

(with-primitive jelly
 (program-jellyfish
  (list
 ; data
   (vector 0 0 0)         ; time (increases by 1 each loop)
   (vector 2 2 -3)        ; shuffle data for converting (x y z) -> (z z x)
 ; code follows to build a vertex by rotation around an angle based on the index
   (vector LDA 0 0)       ; load current time from address 0
   (vector LDL 135.3 0)   ; load angle 135.3 (in degrees)
   (vector MUL 0 0)       ; multiply time by angle
   (vector SIN 0 0)       ; makes (sin(angle) cos(angle) 0)
 ; make a spiral by scaling up with time
   (vector LDA 0 0)       ; load time again
   (vector LDL 0.05 0)    ; load 0.05
   (vector MUL 0 0)       ; multiply to get time*0.05
   (vector MUL 0 0)       ; mul rotation vector by time*0.05
 ; move backward in z so we get some depth
   (vector LDA 0 0)       ; load the time again
   (vector LDL 0.03)      ; load 0.03
   (vector MUL 0 0)       ; multiply the time by 0.01
   (vector LDA 1 0)       ; load the shuffle vec from address 1 
   (vector SHF 0 0)       ; shuffle the x to z position
   (vector ADD 0 0)       ; add (0 0 x) to set z on current position
   (vector STI 0 REG_MDL) ; write position to model memory registers
 ; increment the index by 1
   (vector LDA 0 0)       ; load address
   (vector LDL 1 0)       ; load inc
   (vector ADD 0 0)       ; add them together
   (vector STA 0 0)       ; store at address loc
   (vector JMP 2 0)))     ; goto 2
 
 (pdata-map! 
  (lambda (c)
    (rndvec)) ;(vector 0.7 0.7 0.7 0.4))
  "c")
 (hint-unlit)
 (hint-wire)
 (line-width 3))

(with-primitive 
 jelly
 (translate (vector -0.3 1.4 0))
 (rotate (vector -40 -20 2))
 )
                   
(every-frame (render))


