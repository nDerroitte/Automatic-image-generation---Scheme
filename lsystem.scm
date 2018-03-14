;===============================================================================
; Projet 1 - Programmation fonctionelle - 31/03/17
;
; DERROITTE Natan - PIRE Renaud
;===============================================================================
#lang racket

(provide lsystem.generate-string)
(provide tlsyst.angle)
(provide tlsyst.lsystem)

(provide sierpinski-triangle)
(provide sierpinski-carpet)
(provide dragon-curve)
(provide tree-growth)
(provide plant-growth)
(provide gosper-curve)

;===============================================================================
; Un "Turtle L-system" contient la representation d'un "L-system" générant
; un "Turtle string" et son angle de rotation.
; Un "Turtle L-system" est donc resprésenté par une paire dont le premier élément
; est l'angle de rotation et le second est le L-System.
;===============================================================================

;; ----------------------------------------------------------------------------
;; tlsyst.angle
;;
;; Argument :
;;    -TSystem un Turtle L-Sytem
;;
;; Renvoie l'angle correspondant à ce Turtle L-Sytem
;; ----------------------------------------------------------------------------
(define tlsyst.angle (lambda (TSystem)
                      (if (null? TSystem) 0
                       (car TSystem))))
;; ----------------------------------------------------------------------------
;; tlsyst.lsystem
;;
;; Argument :
;;    -TSystem un Turtle L-Sytem
;;
;; Renvoie le L-Sytem correspondant à ce Turtle L-Sytem
;; ----------------------------------------------------------------------------
(define tlsyst.lsystem (lambda (TSystem)
                         (if (null? TSystem) '()
                        (car (cdr TSystem)))))

;; ----------------------------------------------------------------------------
;; tlsyst.generate-string
;;
;; Argument :
;;    -LSystem un L-System
;;    -n : l'ordre de la chaine d'instruction à générer
;;
;; Renvoie une liste de caractère correspondant à la chaine d'instruction pour
;; l'ordre et le LSystem donné en entrée. Celle-ci peut ne pas être unique si
;; le L-System contient des règles de production/terminaison de probabilité
;; non-unitaire.
;; -----------------------------------------------------------------------------
(define lsystem.generate-string(lambda (LSystem n)
                                 (if (null? LSystem) '()
                                 (listToString (generateList LSystem n)))))

;===============================================================================
; Nous définissons ci-dessous les TLSystem pour les différentes figures du
; projet.
; Ces TLSystem sont composé d'une liste de deux éléments
;     - L'angle de rotation
;     - Le L-System correspondant composé lui-même de plusieurs éléments
;            * L'axiome de base
;            * La/Les règle(s) de production
;            * La/Les règle(s) de terminaison
;            * Les éventuelles probabilités qui sont liées aux règles de prod.
;            * La facteur initial de x et son facteur de divison
;===============================================================================
; Le "Turtle L-system" correspondant au triangle de Sierpinsky
(define sierpinski-triangle '(120 ((A - B - B)            
                                   ((A (A - B + A + B - A))
                                    (B (B B)))           
                                   ((A (T))               
                                    (B (T)))             
                                   ()                     
                                   ())))                   
; Le "Turtle L-system" correspondant au tapis de Sierpinsky
(define sierpinski-carpet '(90 ((T - T - T - T)           
                                ((T (T < - T - T > T < - T - T - T > T))) 
                                ()                        
                                ()                        
                                ())))                      
; Le "Turtle L-system" correspondant à la courbe de dragon
(define dragon-curve '(45 ((D)                      
                           ((D (- D + + E))          
                            (E (D - - E +)))        
                           ((D (- - T + + T))       
                            (E (T - - T + +)))      
                           ()                       
                           ())))                     

; Le "Turtle L-system" correspondant à l'arbre
(define tree-growth '(25.7 ((T)                       
                            ((T (T < + T > T < - T > T))
                             (T (T < + T > T))        
                             (T (T < - T > T)))       
                            ()                        
                            (0.33 0.33 0.34)          
                            ())))                      

; Le "Turtle L-system'' correspondant à la plante
(define plant-growth '(8 ((B)                       
                          ((B (T < + 5 B > < - 7 B >
                                 - 1 T < + 4 B > < - 7 B >
                                 - 1 T < + 3 B > < - 5 B >
                                 - 1 T B)))              
                          ((B(T)()))                        
                          ()                       
                          (1 2))))                 

; Le "Turtle L-system'' correspondant à la courbe de gosper
(define gosper-curve '(60 ((A)                
                           ((A (A - B - - B + A + + A A + B -)) 
                            (B (+ A - B B - - B - A + + A + B)))
                           ((A (T))           
                            (B (T)))          
                           ()                 
                           ())))           

;===============================================================================
; Ci-dessous sont reprises les fonctions nous servant à générer une chaine
; d'instructions et de la transformer en liste de string à partir d'un L-Sytem
; et de l'ordre
;===============================================================================
;; ----------------------------------------------------------------------------
;; listToString
;;
;; Arguments :
;;    -liste : liste à formatter sous forme de string
;;
;; Renvoie une liste string dont les éléments sont égaux à ceux de la liste
;; de symboles et nombres correspondant à la chaine prise en argument.
;; ----------------------------------------------------------------------------
(define listToString (lambda (liste)
                  (cond ((empty? liste) '())
                        ((and (not (empty? (cdr liste)))
                              (number? (car (cdr liste))))
                         (cons (string-append (symbol->string (car liste))"["
                                              (number->string (car (cdr liste)))
                                              "]")
                               (listToString (cdr (cdr liste)))))
                        (else (cons (symbol->string (car liste))
                                    (listToString (cdr liste)))))))
;; ----------------------------------------------------------------------------
;; generateList
;;
;; Arguments :
;;    -LSystem : un L-System
;;    -n : l'ordre de la chaine d'instruction à générer
;;
;; Renvoie une liste de symboles et nombres correspondant à la chaine
;; d'instruction pour l'ordre et le L-System donné en entrée. 
;; ----------------------------------------------------------------------------
(define generateList(lambda (LSystem n)
                  (develop n (car LSystem) (cadr LSystem) (caddr LSystem)
                           (car (cdddr LSystem)) (car (cddddr LSystem)))))

;; ----------------------------------------------------------------------------
;; flattenList
;;
;; Argument :
;;    -liste : Une liste à aplatir
;;
;; Renvoie une liste aplatie dont les éléments sont égaux à ceux de la liste
;; prise en argument
;; -----------------------------------------------------------------------------
(define flattenList (lambda (liste)
  (if (pair? liste)
      (append (flattenList (car liste))(flattenList (cdr liste)))
      (if (symbol? liste)(list liste)
          (if (number? liste)(list liste)'() )))))

;; ---------------------------------------------------------------------------
;; develop
;;
;; Arguments :
;;    -n :      l'ordre de la chaine d'instruction à génerer
;;
;;    -currentListe     :  La liste courante à laquelle on va appliquer les
;;                         règles de production (ou de terminaison si n =0).
;;    -listeProductions :  Liste contenant les règles de productions.
;;                         Chacune de ses règles est une liste dont le premier
;;                         élément est celui à remplacer et le second est l'
;;                         élément remplaçant
;;    -listeTerminaisons :Lliste contenant les règles de terminaisons.
;;                         Chacune de ses règles est une liste dont le premier
;;                         élément est celui à remplacer et le second est l'
;;                         élément remplaçant
;;
;;    -proba : Liste contenant les probabilités des règles de productions.
;;             Il est important que la liste de proba contienne autant
;;             d'éléments que le nombre de règles de productions et que
;;             la somme des probabilités soit égale à 1.
;;             Liste vide si probabilité unitaire pour chaque règle de prod.
;;    -valX :  Liste contenant la valeur initiale de X et son facteur de
;;             division si ceux-ci existent dans le L-System
;;
;; Renvoie une liste de symboles et nombres correspondant à la modification du
;; de la liste 'string' par les règles de productions. De plus, si n est nul,
;; on lui applique également les règles de terminaisons
;; ---------------------------------------------------------------------------
(define develop
  (lambda( n currentListe listeProductions listeTerminaisons proba valX)
        (if (zero? n)
            (if (null? valX)
             (flattenList (replaceLine currentListe listeTerminaisons '()))
             (flattenList (addX(flattenList(replaceLine currentListe
                                                       listeTerminaisons '()))
                            'T (car valX))))
            (if (null? valX)
             (develop
              (- n 1)
              (flattenList(replaceLine currentListe listeProductions proba))
              listeProductions
              listeTerminaisons
              proba
              valX)
                (develop
                 (- n 1)
                 (flattenList (addX
                                (flattenList
                                          (replaceLine
                                           currentListe
                                           listeProductions
                                           proba))
                                'T
                                (car valX)))
                    listeProductions
                    listeTerminaisons
                    proba
                    (list (/ (car valX) (cadr valX)) (car (cdr valX))))))))
;; ---------------------------------------------------------------------------
;; replaceLine
;;
;; Arguments :
;;    -currentListe :      La liste actuelle à laquelle on va appliquer les
;;                         règles de productions (et de terminaisons si n =0)
;;    -listeModifications  Liste contenant les règles de modification
;;                         (terminaisons ou productions).
;;                         Chacune de ses règles est une liste dont le premier
;;                         élément est celui à remplacer et le second est
;;                         l'élément remplaçant
;;
;;    -proba : liste contenant les probabilités des règles de productions.
;;             Liste vide si probabilité unitaire pour chaque règle de prod ou
;;             si on applique des règles de terminaisons.
;;             Il est important que la liste de proba contienne autant
;;             d'éléments que le nombre de règles de productions et que la somme
;;             des probabilités soit égale à 1.
;;
;; Renvoie une liste à laquelle chacun des éléments a été modifié par la règle
;; de modification (terminaisons ou productions) si une règle s'appliquait pour
;; cet élément.
;; ---------------------------------------------------------------------------
(define replaceLine
  (lambda(currentListe listeModifications proba)
       (if (null? currentListe)
          '()
          (cons(replaceChar (car currentListe) listeModifications proba)
               (replaceLine(cdr currentListe) listeModifications proba)))))
;; ---------------------------------------------------------------------------
;; replaceChar
;;
;; Arguments :
;;    -caractere :         Le caractère à vérifier et auquel appliquer une 
;;                         règle de modification au cas écheant
;;    -listeModifications  Liste contenant les règles de modifications
;;                         (terminaisons ou productions).
;;                         Chacune de ses règles est une liste dont le premier
;;                         élément est celui à remplacer et le second est
;;                         l'élément remplaçant
;;
;;    -proba : liste contenant les probabilités des règles de productions.
;;             Liste vide si probabilité unitaire pour chaque règle de prod ou
;;             si on applique des règles de terminaisons.
;;             Il est important que la liste de proba contienne autant
;;             d'éléments que le nombre de règles de productions et que la somme
;;             des probabilités soit égale à 1.
;;
;; Renvoie le symbol ou nombre après lui avoir appliqué les règles de
;; modification (terminaison ou production). Si le caractère n'était concerné
;; par aucune règle, il reste inchangé.
;; ---------------------------------------------------------------------------
(define replaceChar(lambda (caractere listeModifications proba)
                      (if (null? listeModifications)
                          caractere
                          (if (eq? caractere (car (car listeModifications)))
                              (if (null? proba)
                                  (cdr (car listeModifications))
                                  (randomRule (/ (random 100) 100.0)
                                               (car proba)
                                               (cdr proba)
                                               listeModifications))
                              (replaceChar caractere
                                            (cdr listeModifications)
                                            proba)))))
;; ---------------------------------------------------------------------------
;; randomRule
;;
;; Arguments :
;;    -rand  : Nombre aléatoire entre 0 et 1
;;    -somme : Somme entre les probabilités des règles déjà essayées et de la
;;             probabilité de la règle en cours d'essai
;;    -proba : liste contenant les probabilités des règles de productions.
;;             Il est important que la liste de proba contienne autant
;;             d'éléments que le nombre de règles de productions et que
;;             la somme des probabilités soit égale à 1.
;:
;;    -listeModifications  Liste contenant les règles de modifications
;;                         (terminaisons ou productions).
;;                         Chacune de ses règles est une liste dont le premier
;;                         élément est celui à remplacer et le second est
;;                         l'élément remplaçant
;;
;; Renvoie une la règle de production choisie aléatoirement
;; ---------------------------------------------------------------------------
(define randomRule (lambda (rand somme proba listeModifications)
                      (if (null? proba)
                          (cdr (car listeModifications))
                          (if (> somme rand)
                              (cdr (car listeModifications))
                              (randomRule rand (+ somme (car proba))
                                           (cdr proba)
                                           (cdr listeModifications))))))

;; ---------------------------------------------------------------------------
;; addx
;;
;; Arguments :
;;    -currentList  : La liste à modifier
;;    -caractere    : le symbol avant la valeur de x
;;    -x            : la valeur de x
;;
;; Renvoie la liste modifié où on a ajouter les bonnes valeurs de x aux
;; endroits correspondant.
;; --------------------------------------------------------------------------- 
(define addX (lambda (currentList caractere x)
               (cond ((null? currentList) '())
                     ((or (and (eq? caractere (car currentList))
                               (not (null? (cdr currentList)))
                               (not (number? (car (cdr currentList)))))
                          (and (eq? caractere (car currentList))
                               (null? (cdr currentList))))
                      (list caractere x (addX (cdr currentList) caractere x)))
                     ((and (eq? caractere (car currentList))
                           (null? (cdr currentList)))
                      (list caractere x (addX (cdr currentList) caractere x)))
                     (else
                      (cons
                       (car currentList)
                       (addX (cdr currentList) caractere x))))))
;===============================================================================
                    