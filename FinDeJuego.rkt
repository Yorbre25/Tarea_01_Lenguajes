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
    ((empty? Y) (accessElemAux (caar mat) (cdar mat) (cdr mat) (+ row 1) 0 var))
    ((equal? elem var) (checkBoundaries elem Y mat row col var))
    (else (accessElemAux (car Y) (cdr Y) mat row (+ col 1) var))
    ))


(define (checkBoundaries elem Y mat row col var)
    (cond
    ((checkHorizontal elem Y var) true)
    ((checkVertical elem Y mat col var) true)
    ((checkDiagonal elem Y mat row col var) true)
    (else false)))

(define (checkHorizontal elem Y var)
    (cond
    ((or (empty? Y) (empty? (cdr Y))) false)
    ;(else false)))
    (else (checkHorizontalAux (cadr Y) Y var 1))))

(define (checkHorizontalAux elem Y var i)
    (cond
    ((equal? i 3) true)
    ((empty? Y) false)
    ((equal? elem var) (checkHorizontalAux (cadr Y) Y var (+ i 1)))
    (else false)))

(define (checkVertical elem Y mat col var)
    (cond
    ((empty? mat) false)
    ;(else false)))
    (else (checkVerticalAux (caadr mat) (cdar mat) mat col var 1 0))))

(define (checkVerticalAux elem Y mat col var i currCol)
    (cond
    ((empty? mat) false)
    ((empty? Y) (checkVerticalAux (caadr mat) (cadr mat) (cdr mat) col var i (+ currCol 1)))
    ((equal? i 3) true)
    ((and (equal? elem var) (equal? col currCol)) (checkVerticalAux (caadr mat) (car (cdr mat)) (cdr mat) col var (+ i 1) -1))
    ((equal? col currCol) false)
    (else (checkVerticalAux (car Y) (cdr Y) mat col var i (+ currCol 1))))
)

(define (checkDiagonal elem Y mat row col var)
    (cond
    ((empty? mat) false)
    (else (or (checkDiagonalLeftAux (caadr mat) (cdar mat) (cdr mat) (+ row 1) (- col 1) (+ row 1) 0 var 1 -1) (checkDiagonalLeftAux (caadr mat) (cdar mat) (cdr mat) (+ row 1) (- col 1) (+ row 1) 0 var 1 1)))))

(define (checkDiagonalLeftAux elem Y mat row col currRow currCol var i direc)
    (cond
    ((empty? mat) false)
    ((empty? Y) (checkDiagonalLeftAux (caar mat) (cdar mat) (cdr mat) row col (+ currRow 1) currCol var i))
    ((equal? i 3) true)
    ((and (equal? elem var) (equal? col currCol) (equal? row currRow) (checkDiagonalLeftAux (caadr mat) (cdar mat) (cdr mat) (+ row 1) (+ col direc) (+ row 1) 0 var (+ i 1) direc)))
    ((equal? col currCol) false)
    (else (checkDiagonalLeftAux (car Y) (cdr Y) mat row col currRow (+ currCol 1) var i direc))
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
;(set! mat (setValToPos mat 1 3 2))
(set! mat (setValToPos mat 1 4 2))
(set! mat (setValToPos mat 1 5 2))
(set! mat (setValToPos mat 2 1 1))
(set! mat (setValToPos mat 2 2 2))
;(set! mat (setValToPos mat 3 1 2))
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