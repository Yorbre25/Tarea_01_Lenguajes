#lang racket 
(require "AlgoritmoGoloso.rkt")


(define (checkGameStatus mat var)
  (cond
    ((empty? mat) mat)
    (else (accessElem mat 0 0 var))
    )
)

(define (accessElem mat row col var)
    (cond
    ((empty? mat))
    (else (accessElemAux (caar mat) (cdar mat) (cdr mat) row col var))
    )
)

(define (accessElemAux elem Y mat row col var)
    (cond
    ((and (empty? Y) (empty? mat)) false)
    ((empty? Y) (accessElemAux (caar mat) (cdar mat) (cdr mat) (+ row 1) col var))
    ((equal? elem var) (checkBoundaries elem Y mat row col var))
    (else (accessElemAux (car Y) (cdr Y) mat row (+ col 1) var))
    ))


(define (checkBoundaries elem Y mat row col var)
    (cond
    ((checkHorizontal elem Y var) (println "tuqi"))
    ((checkVertical elem Y mat col var) (println "tuqi2"))
    ((checkDiagonal elem Y mat row col var) (println "tuqi3"))
    (else false)))

(define (checkHorizontal elem Y var)
    (cond
    ((empty? Y) false)
    (else false)))
    ;(else (checkHorizontalAux elem Y var 1))))

(define (checkHorizontalAux elem Y var i)
    (cond
    ((equal? i 3) true)
    ((equal? elem var) (checkHorizontalAux elem (cdr Y) var (+ i 1)))
    (else false)))

(define (checkVertical elem Y mat col var)
    (cond
    ((empty? mat) false)
    (else false)))
    ;(else (checkVerticalAux elem Y mat col var 1 col))))

(define (checkVerticalAux elem Y mat col var i currCol)
    (cond
    ((empty? mat) false)
    ((equal? i 3) true)
    ((and (equal? elem var) (equal? col currCol) (checkVerticalAux elem (car mat) (cdr mat) col var (+ i 1) 0)))
    ((equal? col currCol) false)
    (else (checkVerticalAux elem (cdr Y) mat col var i (+ currCol 1))))
)

(define (checkDiagonal elem Y mat row col var)
    (cond
    ((empty? mat) false)
    (else (checkDiagonalLeftAux elem Y mat row col row col var 1) (checkDiagonalRightAux elem Y mat row col row col var 1))))

(define (checkDiagonalLeftAux elem Y mat row col currRow currCol var i)
    (cond
    ((empty? mat) false)
    ((empty? Y) (checkDiagonalLeftAux (caar mat) (cdar mat) (cdr mat) row col (+ currRow 1) currCol var i))
    ((equal? i 3) true)
    ((and (equal? elem var) (equal? col currCol) (equal? row currRow) (checkDiagonalLeftAux elem (car mat) (cdr mat) (+ row 1) (- col 1) currRow currCol var (+ i 1))))
    ((equal? col currCol) false)
    (else (checkDiagonalLeftAux (car Y) (cdr Y) mat row col currRow (+ currCol 1) var i))
))

(define (checkDiagonalRightAux elem Y mat row col currRow currCol var i)
    (cond
    ((empty? mat) false)
    ((empty? Y) (checkDiagonalLeftAux (caar mat) (cdar mat) (cdr mat) row col (+ currRow 1) currCol var i))
    ((equal? i 3) true)
    ((and (equal? elem var) (equal? col currCol) (equal? row currRow) (checkDiagonalLeftAux elem (car mat) (cdr mat) (+ row 1) (+ col 1) currRow currCol var (+ i 1))))
    ((equal? col currCol) false)
    (else (checkDiagonalLeftAux (car Y) (cdr Y) mat row col currRow (+ currCol 1) var i))
))
; prueba !!!!!!!!!!!!!!!!!!!!!!!!!

(println "-----crear matriz-----")
(define mat (buildMatrix 6 6))
(printMat mat)
;(set! mat (setValToPos mat 5 1 2))
(println "-----agregar a matriz-----")
(printMat mat)
(println "-----agregar a matriz-----")
(set! mat (setValToPos mat 1 1 2))
(printMat mat)
(println "-----ver parte de matriz-----")
(define estado (caar mat))
(displayln estado)
;(println "-----ver parte de matriz-----")
;(define estado2 (cddr mat))
;(displayln estado2)
;(define estado3 (caar estado2))
;(displayln estado3)
(println "-----game status-----")
(define estado4 (checkGameStatus mat 2))
(displayln estado4)

; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


(provide (all-defined-out))