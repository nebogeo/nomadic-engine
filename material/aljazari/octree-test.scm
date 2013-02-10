;; al jazari two (c) 2013 dave griffiths gpl v3
(load "aljazari/blockview.scm")
(load "aljazari/octree.scm")

(define octree
  (octree-compress
   (octree-compress
    (octree-compress
     (octree-compress
      (octree-compress
       (octree-fill-sphere 
        (make-empty-octree) 
        (vector 16 16 16) 20 0)))))))

(define root (build-cube))
(parent root)

(define block-view (with-state
 (scale (vector 0.15 0.15 0.15))
 (translate (vector -16 -16 -16))
 (make-block-view octree)))

(every-frame 
 (with-primitive 
  root
  (rotate (vector 1 1.2 0))))