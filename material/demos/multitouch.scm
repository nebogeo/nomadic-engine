(clear)

(define shapes (list (build-cube) (build-cube)))

(every-frame
 (for-each
  (lambda (tid)
    (let ((p (list-ref shapes tid)))
      (with-primitive 
       p
       (identity)
       (translate 
        (get-point-from-touch tid)))))
  (get-touch-ids)))