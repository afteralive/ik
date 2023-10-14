#lang racket

(require
  racket/gui
  "foundation.rkt")

(define frame-display%
  (class frame%
    (super-new
     [label "Afteralive"]
     [width 600]
     [height 400])

    (init-field drawer-thread)

    (define/augment (on-close)
      (kill-thread drawer-thread))))

(define dc '())

(define frm (new frame-display%
                 [drawer-thread
                  (thread
                   (lambda ()
                     (let continue ()
                      (begin
                        (unless (null? dc)
                          (send dc set-background (make-color 130 130 130))
                          (send dc clear)
                          (draw-contents dc))
                        (sleep 0.033)
                        (continue)))))]))

(define ik-point (point 0 100))

(define display-canvas%
  (class canvas%
    (super-new)
    (define dc (send this get-dc))
    (define/override (on-event e)
      (when (send e button-down? 'left)
        (set! ik-point
              (dc-point->point (point (send e get-x) (send e get-y)) dc)))
      'ok)))

(define canvas
  (new display-canvas%
       [parent frm]))

(define (point->x/y/w/h p)
  (define size/2 5)
  (values
   (- (point-x p) size/2)
   (- (point-y p) size/2)
   (* size/2 2)
   (* size/2 2)))

(define (dc-origin dc)
  (let*-values ([(dc-w dc-h) (send dc get-size)]
                [(origin) (point (/ dc-w 2)(- dc-h 100))])
    origin))

;;; 将数学坐标系里的点转换到dc的坐标系上
(define (point->dc-point p dc)
  (let ([origin (dc-origin dc)])
    (point+
     origin
     (point (point-x p) (- (point-y p))))))

;;; 与point->dc-point做的转换相反
(define (dc-point->point p dc)
  (let ([origin (dc-origin dc)])
    (point
     (- (point-x p) (point-x origin))
     (- (point-y origin) (point-y p)))))

(define (draw-math-coord-rounded-rectangle dc p)
  (let-values ([(x y w h) (point->x/y/w/h (point->dc-point p dc))])
    (send dc draw-rounded-rectangle x y w h)))

;;; 绘制骨骼节点
(define (draw-bone-node dc point)
  (send dc set-brush (if (absolute-point? point) "red" "black") 'solid)
  (let-values ([(x y w h) (point->x/y/w/h (point->dc-point (absolute-position point) dc))])
    (send dc draw-rounded-rectangle x y w h)))

(define (draw-arrow dc st ed)
  (send dc set-brush (make-color 0 0 255) 'solid)
  (define r-st (point->dc-point st dc))
  (define r-ed (point->dc-point ed dc))
  (send dc draw-line (point-x r-st) (point-y r-st) (point-x r-ed) (point-y r-ed)))

(define (draw-bone-chain dc chain)
  (draw-bone-node dc (car chain))
  (let iter ([cur (cdr chain)] [last-position (absolute-position (car chain))])
    (unless (null? cur)
      (define cur-position (absolute-position (car cur)))
      (draw-arrow dc last-position cur-position)
      (draw-bone-node dc (car cur))
      (iter (cdr cur) cur-position))))

;;; DEMO节点
(define p1 (absolute-point 0 0))
(define p2 (rotate-relative-point p1 50 (/ pi 2)))
(define p3 (rotate-relative-point p2 50 (* pi 0.75)))

(define (draw-contents dc)
  (draw-bone-chain dc (list p1 p2 p3))

  ;;; DRAW IK POINT
  (send dc set-brush (make-color 0 0 255) 'solid)
  (draw-math-coord-rounded-rectangle dc ik-point)
  'ok)

(display (absolute-position p3))

(set! dc (send canvas get-dc))

(send frm show #t)