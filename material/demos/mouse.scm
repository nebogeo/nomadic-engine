(clear)

(define p (with-state
           (hint-unlit)
           (build-cube)))
(define b (with-state 
           (hint-none)
           (hint-unlit)
           (hint-wire)
 ;          (translate (vector 1 0 0)) 
           (scale (vector 8 8 1)) 
          ; (rotate (vector 0 0 45))
           (build-cube)))

(with-primitive 
 b
 (pdata-map! (lambda (c) (vector 1 0 0)) "c")
 (apply-transform)
 (rotate (vector 0 45 20))
 (translate (vector 5 -10 0))
)

(every-frame 
 (let* ((line (get-line-from-mouse))
        (r (with-primitive 
            b
            (let ((mi
                   (minverse (get-transform))
                   ))
              (display (vtransform (vector 0 0 0) mi))(newline)
              (geo/line-intersect 
               (vtransform (car line) mi)
               (vtransform (cadr line) mi))))))
   (let ((pos (if (not (null? r))
                  (cdr (assoc "p" (car r)))
                  (vector 0 0 0))))
     (with-primitive
      p
      (identity)
      (translate (with-primitive 
                  b 
                  (vtransform pos (get-transform))))))))
