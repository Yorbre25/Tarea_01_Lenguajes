#lang racket

#|
    Imprime en la terminal la una matriz
|#
(define (printMat mat)
    (cond
    [(= (length mat) 1) (displayln (car mat))]
    [else (printMatAux (car mat) (cdr mat))]
    )
)

(define (printMatAux raw1 mat)
    (cond
    [(null? mat) (displayln raw1)]
    [else (displayln raw1) (printMatAux (car mat) (cdr mat))]
    )
)

#|
    Crea una función de 0s.
    m y n deben ser mayor que 2 y menor que 11.
    Entrada:
        m: numero de filas
        n: numero de columnas
|#
(define (buildMatrix m n)
    (cond
    [(and (and (>= m 3) (>= n 3)) (and (<= m 3) (<= n 3))) (buildMatrixAux '() '() m n 0 0)]
    [else printf("Error en las dimensiones de la matriz")]
    )
)

(define (buildMatrixAux mat col m n rNum cNum)
    (cond
    [(= m rNum) mat]
    [(= cNum n) (buildMatrixAux (cons col mat) '() m n (+ rNum 1) 0)]
    [else (buildMatrixAux mat (cons 0 col) m n rNum (+ cNum 1))]
    )
)

#|
    Cambia el valor de la matriz. mat[i, j] = valor
    Entrada:
        mat: matriz
        i: posición en filas
        j: posición de columnas
        valor: valor que se va a ingresar a la matriz
|#
(define (setPosTo mat i j valor)
    (setPosToAuxR mat i j valor 0 0))

(define (setPosToAuxR mat posRaw posCol valor i j)
    (cond
    [(null? mat) '()]
    [(= posRaw i) (cons (setPosToAuxC (car mat) posCol valor 0) (setPosToAuxR (cdr mat) posRaw posCol valor (+ i 1) j))]
    [else (cons (car mat) (setPosToAuxR (cdr mat) posRaw posCol valor (+ i 1) j))]
    )
)

(define (setPosToAuxC raw posCol valor j)
    (cond
    [(null? raw) '()]
    [(= posCol j) (cons valor (setPosToAuxC (cdr raw) posCol valor (+ j 1)))]
    [else (cons (car raw) (setPosToAuxC (cdr raw) posCol valor (+ j 1)))]
    )
)

#|
    Retorna una lista de de tuplas con las posiciones
    de la matriz donde el valor es igual a 0
    Entrada:
        mat: matriz
|#
(define (conjuntoCandidatos mat)
    (conjuntoCandidatosAux (cdr mat) (car mat) '() 0 0)
)

(define (conjuntoCandidatosAux mat raw candidatos i j)
    (cond
    [(and (null? mat) (null? raw)) candidatos]
    [(null? raw) (conjuntoCandidatosAux (cdr mat) (car mat) candidatos (+ i 1) 0)]
    [(= (car raw) 0) (conjuntoCandidatosAux mat (cdr raw) (cons (list i j) candidatos) i (+ j 1))]
    [else (conjuntoCandidatosAux mat (cdr raw) candidatos i (+ j 1))]
    )
)

(define mat (buildMatrix 3 3))
; (printMat mat)
; (printMat (setPosTo mat 0 2 -1))
(conjuntoCandidatos mat)


(provide buildMatrix) ;;; Exporta la funcion para importarla desde otro archivo