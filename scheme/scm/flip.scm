(clear)
(colour (vector 0 0.5 1 1))
(define cubes
  (map 
   (lambda (p)
     (with-primitive p
       (apply-transform)
       (translate (vector -2.5 2 5))
       (rotate (vector 45 0 0))
       (translate 
        (vector
         (quotient (- p 1) 6)
         0
         (modulo (- p 1) 6)))
       p))
   (build-list
    (lambda (n) 
      (with-state
       (scale (vector 0.5 0.05 0.5))
       (build-cube)))
    36)))

(every-frame
 (for-each 
  (lambda (p)
    (with-primitive 
     p
     (rotate (vmul (vector (sin p) 0 (cos p)) 5))))
  cubes))
                    

