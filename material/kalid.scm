(clear)

(define twirl-shape 
  (with-state
   (hint-unlit)
   (build-polygons 40 triangle-strip)))

(define finger-shapes
  (with-state
   (hint-none)
   (hint-unlit)
   (hint-wire)
   (list (build-polygons 30 triangle-strip)
         (build-polygons 30 triangle-strip))))

(define (spiral)
  (line-width 5)
  (pdata-map! (lambda (c) (vector 1 0.29 0.42)) "c")
  (pdata-index-map!
   (lambda (i p)
     (let ((i (* i 0.8)))
       (vmul (vector (sin i) (cos i) 0) (* 0.02 i i))))
   "p")
  (pdata-copy "p" "pref") ; only really needed for animation
  (pdata-map!
   (lambda (p pref)
     (vadd pref (vmul (crndvec) 0.2)))
   "p" "pref"))

(for-each
 (lambda (finger-shape)
   (with-primitive finger-shape (spiral)))
 finger-shapes)

(with-primitive 
 twirl-shape
 (pdata-map!
  (lambda (c)
    (vector 1 0.29 0.42))
  "c"))

; using with-primitive is really slow, so directly use grab
; returns the distance between the objects
(define (get-pinch)
  (grab (car finger-shapes))
  (let ((a (vtransform (vector 0 0 0) (get-transform))))
    (ungrab)(grab (cadr finger-shapes))
    (let ((b (vtransform (vector 0 0 0) (get-transform))))
      (ungrab)
      (vdist a b))))

; store pinch as it's slow to calculate
(define pinch 1)

(every-frame
 (begin
   ; do the twirling
   (with-primitive
    twirl-shape
    (pdata-index-map!
     (lambda (i p)
       (let ((i (* i 0.5)))
         (vmul (vector (sin i) (cos i) (* 2 (cos (* i 10.43)))) 
               (* 5 (sin (* pinch (+ i (* 0.1 (time))) 0.1))))))
     "p"))
   ; check for touch events
   (for-each
    (lambda (touch-id)
      (let ((finger-shape (list-ref finger-shapes touch-id)))
        (with-primitive 
         finger-shape
         (identity)
         (translate 
          (get-point-from-touch touch-id))
         (rotate (vector 0 0 (* (time) 10)))))
      (set! pinch (get-pinch)))
    (get-touch-ids))))
