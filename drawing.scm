#lang racket

(provide (all-defined-out))
; A ``point'' is representd by a dotted pair (x . y), where
;;        x is the x-cooridante of the point
;;        y is the y-coordinate of the point
(define cons-point cons)
(define point.x car)
(define point.y cdr)

; A ``d&p'' is  a dotted pair (direction . point), where
;;        direction is a trigonometric angle in radian
;;        point is a ``point''
(define cons-d&p cons)
(define d&p.direction car)
(define d&p.point cdr)



; A ``polyline'' is represented by a list of ``point''s
(define cons-polyline list)
; if polyline is a ``polyline'' and point is a ``point'' , (polyline.preprend polyline point)
; is the ``poyline'' whose first ``point'' is point and whose tail is poyline
(define polyline.preprend
  (lambda (polyline point)
    (cons point polyline)))




; A ``bounding box'' is represented by a list (minx miny maxx maxy) representing a rectangle, where
;;        minx is the minimum x coodrinate of the rectangle
;;        miny is the minimum y coodrinate of the rectangle
;;        maxx is the minimum x coodrinate of the rectangle
;;        maxy is the minimum y coodrinate of the rectangle
(define cons-bounding-box list)
(define bb.minx car)
(define bb.miny cadr)
(define bb.maxx caddr)
(define bb.maxy cadddr)



; A ``drawing'' is represented by a list (direction saved-positions polylines bounding-box), where
;;        direction is the current direction, express as a trigonometric angle in radian
;;        saved-positions is a list of ``d&p''s
;;        polylines is a list of ``polyline''s
;;        bounding-box is the ``bounding box'' of the drawing
;;;  The first point of the first polyline is the current ``point''
;;;  The ``bounding box'' is the smallest rectangles which encloses all the points of the drawing
(define cons-drawing
  (lambda (direction saved-positions polylines bounding-box)
    (list direction saved-positions polylines bounding-box)))

(define cons-empty-drawing
  (lambda (direction)
    (cons-drawing direction
                  '()
                  (list (cons-polyline (cons-point 0 0)))
                  (cons-bounding-box 0 0 0 0))))

(define drawing.direction car)

(define drawing.saved-positions cadr)

(define drawing.polylines caddr)

(define drawing.position
  (lambda (drawing)
    (caar (drawing.polylines drawing))))

(define drawing.bounding-box
  (lambda (drawing)
    (car (cdddr drawing))))

(define drawing.minx
  (lambda (drawing)
    (car (drawing.polylines drawing))))

(define drawing.miny
  (lambda (drawing)
    (cadr (drawing.polylines drawing))))

(define drawing.maxx
  (lambda (drawing)
    (caddr (drawing.polylines drawing))))

(define drawing.maxy
  (lambda (drawing)
    (cadddr (drawing.polylines drawing))))

; if drawing is a ``drawing'', direction is a trigonometric angle expressed in radian and
; position is a ``point'', (drawing.push-d&p drawing direction point) returns the list nsp
; such that (car nsp) is (cons-d&p direction point) and  (cdr nsp) is
; (drawing.saved-positions drawing)
(define drawing.push-d&p
  (lambda (drawing direction point)
    (cons (cons-d&p direction point)
          (drawing.saved-positions drawing))))

; if drawing is a ``drawing'', (drawing.pop-saved-positions drawing) returns a dotted pair
; (d&p . tail-saved-positions)
(define drawing.pop-saved-positions drawing.saved-positions)

; if drawing is a ``drawing'' and point is a ``point'', (drawing.peek-new-polyline drawing point)
; returns a list of polylines where the first polyline contains only the ``point'' point and
; the other polylines are those of drawing.
(define drawing.peek-new-polyline
  (lambda (drawing point)
    (cons (cons-polyline point) (drawing.polylines drawing))))



; ------------------------------------------- TODO: REMOVE ------------------------------------- ;
;(define init (cons-drawing 0 '() (list (list (cons 0 0))) (list 0 0 0 0)))
;(define point->string
;  (lambda (point)
;    (string-append "(" (number->string (car point)) " ; " (number->string (cdr point)) ")")))
;(define polyline->string-aux
;  (lambda (polyline)
;    (if (null? polyline) ""
;        (string-append (point->string (car polyline)) " " (polyline->string-aux (cdr polyline))))))
;(define polyline->string
;  (lambda (polyline)
;    (string-append "[" (polyline->string-aux polyline) "]")))
;(define polylines->string-aux
;  (lambda (polylines)
;    (if (null? polylines) ""
;        (string-append (polyline->string (car polylines)) " " (polylines->string-aux (cdr polylines))))))
;(define polylines->string
;  (lambda (polylines)
;    (string-append "(" (polylines->string-aux polylines) ")")))
;(define point-list->string
;  (lambda (point-list)
;    (string-append "(" (polyline->string-aux point-list) ")")))
;(define bounding-box->string
;  (lambda (bounding-box)
;    (string-append "(" (number->string (car bounding-box)) "; "
;                   (number->string (cadr bounding-box)) "; "
;                   (number->string (caddr bounding-box)) "; "
;                   (number->string (cadddr bounding-box))
;                   ")")))
;;(foldr string-append "" (map number->string (bounding-box drawing))
;
;(define drawing->string
;  (lambda (drawing)
;    (string-append "Current position: " (point->string (position drawing)) "\n"
;                   "Direction: " (number->string (direction drawing)) "\n"
;                   "Saved positions: " (point-list->string (saved-positions drawing)) "\n"
;                   "Polylines: " (polylines->string (polylines drawing)) "\n"
;                   "Bounding box: " (bounding-box->string (bounding-box drawing)))))
;(define display-drawing
;  (lambda (drawing)
;    (display (drawing->string drawing))))
