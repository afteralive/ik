#lang racket

(provide
 (struct-out absolute-point)
 (struct-out rotate-relative-point)
 (struct-out point)

 point+
 point-
 point-neg
 find-chain/root
 absolute-position)

(struct absolute-point (x y) #:transparent)
(struct rotate-relative-point (root length angle) #:transparent)
(struct point (x y) #:transparent)

(define (find-root-angle rrp)
  (define (iter root sum)
    (if (rotate-relative-point? root)
        (iter (rotate-relative-point-root root) (+ sum (rotate-relative-point-angle root)))
        sum))
  (iter (rotate-relative-point-root rrp) 0))

(define (point+ p1 p2)
  (point (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2))))
(define (point- p1 p2)
  (point+ p1 (point-neg p2)))
(define (point-neg p)
  (point (- (point-x p)) (- (point-y p))))

(define (rot-x len a)
  (* len (cos a)))
(define (rot-y len a)
  (* len (sin a)))

(define (find-chain/root p)
  (let iter ([chain (list p)] [cur (rotate-relative-point-root p)])
    (if (absolute-point? cur)
        (values chain cur)
        (iter (cons cur chain) (rotate-relative-point-root cur)))))

(define (absolute-position p)
  (if (absolute-point? p)
      (point (absolute-point-x p) (absolute-point-y p))
      (let-values ([(chain root) (find-chain/root p)])
        (let iter ([cur chain] [pos (absolute-position root)] [rangle 0])
          (if (null? cur)
              pos
              (iter
               (cdr cur)
               (point+ pos
                       (point
                        (rot-x (rotate-relative-point-length (car cur)) (+ (rotate-relative-point-angle (car cur)) rangle))
                        (rot-y (rotate-relative-point-length (car cur)) (+ (rotate-relative-point-angle (car cur)) rangle))))
               (+ rangle (rotate-relative-point-angle (car cur)))))))))
