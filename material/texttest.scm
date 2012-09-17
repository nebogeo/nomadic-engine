(with-state
 (texture (load-texture "font.png"))
 (hint-wire)
 (translate (vector 0 0 -10))
 (build-cube))

(define (choose l)
  (list-ref l (random (length l))))

(define p (with-state
           (texture (load-texture "font.png"))
           (hint-unlit)
           (colour (vector 1 0.2 0.3))
           (translate (vector -2.8 0 0))
           (scale (vector 1.4 1.4 1.4))
           (build-text "hello")))

(every-frame 
 (begin
   (with-primitive 
    p
    (set-text (choose (list 
                       "oskar" 
                       "loves"
                       "amber"))))))