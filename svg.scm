#lang racket

(require racket/format) ; see ~r function
(require "drawing.scm")

(provide drawing->svg)

; if width and height are strictly positive integers and drawing is a ``drawing'' (see "drawing.scm")
; then (drawing->svg width height drawing) returns the SVG representation  as a string
; of the given drawing in a (width x height) viewport



;génère la structure du document SVG (il affiche le résulat à l'écran)
; pour tester il faut copier le résultat dans un fichier .svg et l'ouvrir via internet
(define drawing->svg
  (lambda (width height drawing)
    (let ((boundingBox (drawing.bounding-box drawing))
          (polyy (caddr drawing))) 
     (string-append
      "<?xml version=\"1.0\" standalone=\"no\"?>\n"
      "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n\n"
      "<svg width=\""
      (number->string width)
      "\" height=\""
      (number->string height)
      "\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" "
      "viewBox = \""
      (number->string (bb.minx boundingBox))
      " "
      (number->string (bb.miny boundingBox))
      " "
      (number->string (- (bb.maxx boundingBox) (bb.minx boundingBox)))
      " "
      (number->string (- (bb.maxy boundingBox) (bb.miny boundingBox)))
      "\" >\n\n"
      (polylinesToSVG polyy)      
      "\n</svg>"
      ))))

;converti la liste de polylines en un string. Les polyline y sont représenté sous le format svg
(define polylinesToSVG
  (lambda (polylines)
    (if (null? polylines)
        ""
        (apply string-append
         (map
          (lambda (polyline)
            (if (or (null? polyline) (null? (car polyline)))
                ""
                (string-append "<polyline points=\""
                               (apply string-append (map pointToString polyline))
                               "\"\n\tstyle=\" fill:#FFFFFF; stroke:#000000\" />\n"))) polylines)))))

; converti un objet 'point' en string
(define pointToString
  (lambda (point)
    (if (null? point)
        ""
        (string-append (number->string (round (car point))) "," (number->string (round (cdr point))) " ")))) 




; ici c'est la fonction qui est la somme des deux précédente, mais je pense que c'est moins clair
;(define polylinesToSVG
;  (lambda (polylines)
;    (if (null? polylines)
;        ""
;        (apply string-append
;         (map
;          (lambda (polyline)
;            (if (or (null? polyline) (null? (car polyline)))
;                ""
;                (string-append "<polyline points=\""
;                               (apply string-append (map (lambda (point)
;                                                           (if (null? point)
;                                                               ""
;                                                               (string-append (number->string (car point))
;                                                                              ","
;                                                                              (number->string (cdr point))
;                                                                              " "))) polyline))
;                "\"\n\tstyle=\" fill:#FFFFFF; stroke:#000000\" />\n"))) polylines)))))
;


