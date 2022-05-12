#lang racket 
(require "AlgoritmoCodicioso.rkt")


(define (checkGameStatus mat var)
  (cond
    ((empty? mat) mat)
    (else (accessElem mat 0 0 var))
    )
)

(define (accessElem mat row col var)
    (cond
    ((empty? mat) false)
    (else (accessElemAux (caar mat) (cdar mat) (cdr mat) row col var))
    )
)

(define (accessElemAux elem Y mat row col var)
    (cond
    ((and (empty? Y) (empty? mat)) false)
    ((equal? elem var) (checkBoundaries elem Y mat row col var))
    ((empty? Y) (accessElemAux (caar mat) (cdar mat) (cdr mat) (+ row 1) 0 var))
    (else (accessElemAux (car Y) (cdr Y) mat row (+ col 1) var))
    ))


(define (checkBoundaries elem Y mat row col var)
    (cond
    ((checkHorizontal elem Y var) (println "horizontal") true)
    ((checkVertical elem Y mat col var) (println "vertical") true)
    ((checkDiagonal elem Y mat row col var) (println "diagonal") true)
    (else (accessElem mat row col var))))

(define (checkHorizontal elem Y var)
    (cond
    ((or (empty? Y) (empty? (cdr Y))) false)
    ;(else false)))
    (else (checkHorizontalAux (car Y) Y var 1))))

(define (checkHorizontalAux elem Y var i)
    (cond
    ((equal? i 3) true)
    ((empty? Y) false)
    ((and (equal? elem var) (empty? (cdr Y))) (checkHorizontalAux (car Y) Y var (+ i 1))) ;caso especial
    ((equal? elem var) (checkHorizontalAux (cadr Y) (cdr Y) var (+ i 1)))
    (else false)))

(define (checkVertical elem Y mat col var)
    (cond
    ((or (empty? mat) (empty? (cdr mat))) false)
    ;(else false)))
    (else (checkVerticalAux (caadr mat) (car mat) (cdr mat) col var 1 -1))))

(define (checkVerticalAux elem Y mat col var i currCol)
    (cond
    ((equal? i 3) true)
    ((and (empty? mat) (empty? Y)) false)
    ((and (empty? Y) (empty? (cdr mat))) false)
    ((and (equal? elem var) (equal? col currCol) (empty? (cdr mat))) (checkVerticalAux (car Y) (car mat) mat col var (+ i 1) -1)) ; caso especial ultima fila
    ((and (equal? elem var) (equal? col currCol)) (checkVerticalAux (cadr mat) (car mat) (cdr mat) col var (+ i 1) -1)) ; caso especial 
    ((empty? Y) (checkVerticalAux (caadr mat) (car mat) (cdr mat) col var i 0))
    ((equal? col currCol) false)
    (else (checkVerticalAux (car Y) (cdr Y) mat col var i (+ currCol 1)))))


(define (checkDiagonal elem Y mat row col var)
    (cond
    ;((or (empty? mat) (cdr mat)) false)
    ((empty? mat) false)
    (else (or (checkDiagonalLeftAux (caar mat) (cdar mat) (cdr mat) (+ row 1) (- col 1) (+ row 1) 0 var 1 -1) (checkDiagonalLeftAux (caar mat) (cdar mat) (cdr mat) (+ row 1) (+ col 1) (+ row 1) 0 var 1 1)))))

(define (checkDiagonalLeftAux elem Y mat row col currRow currCol var i direc)
    (cond
    ((and (empty? mat) (empty? Y)) false)
    ((empty? Y) (checkDiagonalLeftAux (caar mat) (cdar mat) (cdr mat) row col (+ currRow 1) currCol var i direc))
    ;((empty? mat) (checkDiagonalLeftAux (car Y) (cdr Y) mat row col currRow (+ currCol 1) var i direc))
    ((equal? i 3) true)
    ((and (equal? elem var) (equal? col currCol) (equal? row currRow) (empty? mat)) (checkDiagonalLeftAux (car Y) (cdr Y) mat (+ row 1) (+ col direc) (+ row 1) 0 var (+ i 1) direc)) ; caso especial ultima fila
    ((and (equal? elem var) (equal? col currCol) (equal? row currRow) (empty? (cdr mat))) (checkDiagonalLeftAux (caar mat) (car mat) (cdr mat) (+ row 1) (+ col direc) (+ row 1) 0 var (+ i 1) direc)) ; caso especial ultima fila
    ((and (equal? elem var) (equal? col currCol) (equal? row currRow)) (checkDiagonalLeftAux (caar mat) (cdar mat) (cdr mat) (+ row 1) (+ col direc) (+ row 1) 0 var (+ i 1) direc))
    ((equal? col currCol) false)
    (else (checkDiagonalLeftAux (car Y) (cdr Y) mat row col currRow (+ currCol 1) var i direc)))
)


; prueba !!!!!!!!!!!!!!!!!!!!!!!!!

(println "-----crear matriz-----")
(define mat (buildMatrix 4 3))
(printMat mat)
(set! mat (setValToPos mat 2 1 2))
(set! mat (setValToPos mat 1 1 2))
(set! mat (setValToPos mat 0 2 2))
(set! mat (setValToPos mat 3 0 2))
;(set! mat (setValToPos mat 1 1 2))
;(set! mat (setValToPos mat 2 0 2))
;(set! mat (setValToPos mat 2 2 1))
;(set! mat (setValToPos mat 1 2 1))
;(set! mat (setValToPos mat 0 2 1))
;(set! mat (setValToPos mat 2 0 2))
(println "-----agregar a matriz-----")
(printMat mat)
(println "-----agregar a matriz-----")
;(set! mat (setValToPos mat 1 3 2))
;(set! mat (setValToPos mat 5 5 2))
;set! mat (setValToPos mat 4 5 2))
;(set! mat (setValToPos mat 3 5 2))
;(set! mat (setValToPos mat 1 5 2))
;(set! mat (setValToPos mat 2 4 2))
;(set! mat (setValToPos mat 2 3 2))
;(set! mat (setValToPos mat 2 2 2))
;(set! mat (setValToPos mat 3 3 2))
;(set! mat (setValToPos mat 3 1 2))
(printMat mat)
(println "-----ver parte de matriz-----")
(define estado (caar mat))
(displayln estado)
(println "-----ver parte de matriz-----")
(define estado2 (cddr mat))
(displayln estado2)
(define estado3 (caar estado2))
(displayln estado3)
(println "-----game status-----")
;(define estado4 (checkGameStatus mat 1))
(define estado5 (checkGameStatus mat 2))
;(displayln estado4)
(displayln estado5)
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


(provide (all-defined-out))