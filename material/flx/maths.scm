
(define (square x) 
  (* x x))

(define (vx v) (vector-ref v 0))
(define (vy v) (vector-ref v 1))
(define (vz v) (vector-ref v 2))

(define (vadd a b)
  (vector (+ (vx a) (vx b)) 
          (+ (vy a) (vy b))
          (+ (vz a) (vz b)))) 

(define (vmag v)
  (sqrt (+ (square (vx v))
           (square (vy v))
           (square (vz v)))))
         
(define (vsub a b)
  (vector (- (vx a) (vx b)) 
          (- (vy a) (vy b))
          (- (vz a) (vz b)))) 

(define (vmul v a)
  (vector (* (vx v) a) (* (vy v) a) (* (vz v) a))) 

(define (vdiv v a)
  (vector (/ (vx v) a) (/ (vy v) a) (/ (vz v) a))) 

(define (vdist a b)
  (vmag (vsub a b)))

(define (vlerp v1 v2 t)
	(vadd v1 (vmul (vsub v2 v1) t)))


(define (mscale v)
  (vector (vx v) 0 0 0 
          0 (vy v) 0 0 
          0 0 (vz v) 0 
          0 0 0 1))

; 0  1  2  3 
; 4  5  6  7
; 8  9 10 11
;12 13 14 15

(define (mtranspose m)
  (vector (vector-ref m 0) (vector-ref m 4) (vector-ref m 8) (vector-ref m 12)
          (vector-ref m 1) (vector-ref m 5) (vector-ref m 9) (vector-ref m 13)
          (vector-ref m 2) (vector-ref m 6) (vector-ref m 10) (vector-ref m 14)
          (vector-ref m 3) (vector-ref m 7) (vector-ref m 11) (vector-ref m 15)))


(define (vtransform v m)
  (let ((m m));(mtranspose m)))
    (let ((w (+ (* (vx v) (vector-ref m 3)) 
                (* (vy v) (vector-ref m 7)) 
                (* (vz v) (vector-ref m 11)) 
                (vector-ref m 15))))
   (vdiv
    (vector
     (+ (* (vx v) (vector-ref m 0)) 
        (* (vy v) (vector-ref m 4))
        (* (vz v) (vector-ref m 8))
        (vector-ref m 12))
     (+ (* (vx v) (vector-ref m 1))
        (* (vy v) (vector-ref m 5))
        (* (vz v) (vector-ref m 9))
        (vector-ref m 13))
     (+ (* (vx v) (vector-ref m 2)) 
        (* (vy v) (vector-ref m 6))
        (* (vz v) (vector-ref m 10))
        (vector-ref m 14)))
    w))))
