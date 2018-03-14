#lang racket


(require "drawing.scm")
(provide cons-init-turtle-store)


; A ``turtle f-store'' is a function encapsulating in its closure:
;;        a ``drawing'' (see "drawing.scm")
;;        a ``rotation angle'' which is the angle (expressed in
;;            radians) of a unit change of direction
; It takes as input t-symb,  a ``turtle symbol'', and returns:
;;       the encapsulated ``drawing'', if t-symb is the empty string ""
;;       a ``turtle f-store'' encapsulating the new ``drawing'' corresponding to
;;            chaning the current ``drawing'' according to the known
;;            ``turtle-symbol'' t-symb
;; or raises (error "Unknown turtle symbol") if t-symb is unknown

; The ``turtle symbol''s are;
;;  T,    which means trace forward one unit (on the current polyline)
;;  T[x], which means trace forward x units (on the current polyline)
;;  F,    which means move forward one unit and start a new polyline there
;;  F[x], which means move forward x units and start a new polyline there
;;  <,    which means save the current position and direction
;;  >,    which means restore the last position and direction saved if there were any, otherwise, does nothing
;;  +,    which means make a unit change of  direction
;;  +[x], which means make a change of direction of x units
;;  -,    which means make a change of direction of -1 unit
;;  -[x], which means make a change of direction of -x units

; A ``turtle string'' is a list of ``turtle symbol''s


; if angle and direction are trigonometric angles expressed in degrees,
; (cons-init-turtle-store angle direction) returns a ``turtle f-store''
; encapsulating the empty ``drawing'', with the given initial direction (in radians)
;               a ``rotation angle'' of angle radians




(define cons-init-turtle-store
  (lambda (angle direction)
    (generateTurtleFStore (cons-empty-drawing (/ (* direction pi) 180)) (/ (*  angle pi) 180))))

(define unit 100)

(define newPoint
  (lambda (position direction x)
    (cons (+ (car position) (* unit x (cos direction)))
          (+ (cdr position) (* unit x (sin direction))))))


(define newBoundingbox
  (lambda (boundingbox point)
    (list (if (< (car boundingbox) (car point))
              (car boundingbox)
              (car point))
          (if (< (cadr boundingbox) (cdr point))
              (cadr boundingbox)
              (cdr point))
          (if (> (caddr boundingbox) (car point))
              (caddr boundingbox)
              (car point))
          (if (> (cadddr boundingbox) (cdr point))
              (cadddr boundingbox)
              (cdr point)))))



(define generateTurtleFStore
  (lambda (drawing rotation)
    (lambda (tSymb)
      (cond ((equal? "" tSymb)
             (list (drawing.direction drawing)
                   (drawing.saved-positions drawing)
                   (drawing.polylines drawing)
                   (drawing.bounding-box drawing)))
            ((equal? "T" tSymb)
             (generateTurtleFStore (list (drawing.direction drawing)
                                         (drawing.saved-positions drawing)
                                         (cons (cons (newPoint (drawing.position drawing)
                                                               (drawing.direction drawing)
                                                               1) 
                                                     (car (drawing.polylines drawing)))
                                               (cdr (drawing.polylines drawing)))
                                         (newBoundingbox (drawing.bounding-box drawing)
                                                         (newPoint (drawing.position drawing)
                                                                   (drawing.direction drawing)
                                                                   1)))
                                   rotation))
            ((equal? "F" tSymb)
             (generateTurtleFStore (list (drawing.direction drawing)
                                         (drawing.saved-positions drawing)
                                         (drawing.peek-new-polyline drawing (newPoint (drawing.position drawing)
                                                                                             (drawing.direction drawing)
                                                                                             1))
                                         (newBoundingbox (drawing.bounding-box drawing)
                                                         (newPoint (drawing.position drawing)
                                                                           (drawing.direction drawing)
                                                                           1)))
                                   rotation))
            ((equal? "<" tSymb)
             (generateTurtleFStore (list (drawing.direction drawing)
                                         (drawing.push-d&p drawing (drawing.direction drawing) (drawing.position drawing))
                                         (drawing.polylines drawing)
                                         (drawing.bounding-box drawing))
                                   rotation))
            ((equal? ">" tSymb)
             (generateTurtleFStore (list (car (car(drawing.saved-positions drawing)))
                                         (cdr (drawing.saved-positions drawing))
                                         (drawing.peek-new-polyline drawing (cdr (car(drawing.saved-positions drawing))))
                                         (drawing.bounding-box drawing))
                                   rotation))
            ((equal? "+" tSymb)
             (generateTurtleFStore (list (+ (drawing.direction drawing) rotation)
                                         (drawing.saved-positions drawing)
                                         (drawing.polylines drawing)
                                         (drawing.bounding-box drawing))
                                   rotation))
            ((equal? "-" tSymb)
             (generateTurtleFStore (list (- (drawing.direction drawing) rotation)
                                         (drawing.saved-positions drawing)
                                         (drawing.polylines drawing)
                                         (drawing.bounding-box drawing))
                                   rotation))
            (else
             (let ((lString (string-length tSymb)))
               (cond ((and (> lString 3)
                           (equal? "T[" (substring tSymb 0 2))
                           (equal? "]" (substring tSymb (- lString 1))))
                      (let ((x (string->number (substring tSymb 2 (- lString 1)))))
                        (generateTurtleFStore
                         (list (drawing.direction drawing)
                                         (drawing.saved-positions drawing)
                                         (cons (cons (newPoint (drawing.position drawing)
                                                               (drawing.direction drawing)
                                                               x) 
                                                                 (car (drawing.polylines drawing)))
                                               (cdr (drawing.polylines drawing)))
                                         (newBoundingbox (drawing.bounding-box drawing)
                                                         (newPoint (drawing.position drawing)
                                                                           (drawing.direction drawing)
                                                                           x)))
                         rotation)))
                     ((and (> lString 3)
                           (equal? "F[" (substring tSymb 0 2))
                           (equal? "]" (substring tSymb (- lString 1))))
                      (let ((x (string->number (substring tSymb 2 (- lString 1)))))
                        (generateTurtleFStore
                         (list (drawing.direction drawing)
                               (drawing.saved-positions drawing)
                               (drawing.peek-new-polyline drawing (newPoint (drawing.position drawing)
                                                                            (drawing.direction drawing)
                                                                            x))
                               (newBoundingbox (drawing.bounding-box drawing)
                                                         (newPoint (drawing.position drawing)
                                                                           (drawing.direction drawing)
                                                                           x)))
                         rotation)))
                     ((and (> lString 3)
                           (equal? "+[" (substring tSymb 0 2))
                           (equal? "]" (substring tSymb (- lString 1))))
                      (let ((x (string->number (substring tSymb 2 (- lString 1)))))
                        (generateTurtleFStore
                         (list (+ (drawing.direction drawing) (* x rotation))
                               (drawing.saved-positions drawing)
                               (drawing.polylines drawing)
                               (drawing.bounding-box drawing))
                         rotation)))
                     ((and (> lString 3)
                           (equal? "-[" (substring tSymb 0 2))
                           (equal? "]" (substring tSymb (- lString 1))))
                      (let ((x (string->number (substring tSymb 2 (- lString 1)))))
                        (generateTurtleFStore
                         (list (- (drawing.direction drawing) (* x rotation))
                               (drawing.saved-positions drawing)
                               (drawing.polylines drawing)
                               (drawing.bounding-box drawing))
                         rotation)))
                     (else (error "Unknown turtle symbol")))))))))



