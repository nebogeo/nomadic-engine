(clear)

(define a (build-cube))
(define b (with-state
           (translate (vector 0 4 0))
           (build-cube)))

(with-primitive b (parent a))

(every-frame (with-primitive a (rotate (vector 0 0 1))))