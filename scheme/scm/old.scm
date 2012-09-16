(clear)
(colour (vector 0 0.5 1 1))
(define cubes
  (build-list
   (lambda (n) (build-cube))
   36))

(every-frame
 (for-each
  (lambda (p)
    (with-primitive p
     (let ((s 
            (sin 
              (* 0.5
                 (vdist 
                  (vector 0 0 0) 
                  (vtransform 
                   (vector 0 0 0) 
                   (get-transform)))))))
       (identity)
       (translate 
        (vadd (vector -5 3.5 0)
              (vector 
               (* 
                (sin (* (time) 0.01)) 
                  5)
               (* 
                (cos (* (time) 0.022)) 
                  5) 0)))
       (rotate (vector 130 0 0))
       (translate 
         (vmul (vector 
                (quotient (- p 1) 6)
                (modulo (- p 1) 6) 0) 2))
       (scale (vector s s s)))))
  cubes))