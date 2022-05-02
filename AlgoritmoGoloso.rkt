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
    [(and (and (>= m 3) (>= n 3)) (and (<= m 10) (<= n 10))) (buildMatrixAux '() '() m n 0 0)]
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
        j: posición en columnas
        valor: valor que se va a ingresar a la matriz
|#
(define (setValToPos mat i j valor)
    (setValToPosR mat i j valor 0 0))

(define (setValToPosR mat posRaw posCol valor i j)
    (cond
    [(null? mat) '()]
    [(= posRaw i) (cons (setValToPosC (car mat) posCol valor 0) (setValToPosR (cdr mat) posRaw posCol valor (+ i 1) j))]
    [else (cons (car mat) (setValToPosR (cdr mat) posRaw posCol valor (+ i 1) j))]
    )
)

(define (setValToPosC raw posCol valor j)
    (cond
    [(null? raw) '()]
    [(= posCol j) (cons valor (setValToPosC (cdr raw) posCol valor (+ j 1)))]
    [else (cons (car raw) (setValToPosC (cdr raw) posCol valor (+ j 1)))]
    )
)

#|
    Retorna una lista de de tuplas con las posiciones
    de la matriz donde el valor es igual a 0
    Entrada:
        mat: matriz
|#
(define (viabilidad mat)
    (viabilidadAux (cdr mat) (car mat) '() 0 0)
)

(define (viabilidadAux mat raw candidatos i j)
    (cond
    [(and (null? mat) (null? raw)) candidatos]
    [(null? raw) (viabilidadAux (cdr mat) (car mat) candidatos (+ i 1) 0)]
    [(= (car raw) 0) (viabilidadAux mat (cdr raw) (cons (list i j) candidatos) i (+ j 1))]
    [else (viabilidadAux mat (cdr raw) candidatos i (+ j 1))]
    )
)

#|
    Función selección: Retorna el mejor candidato para marcar en el tablero
    Entrada:
        mat: Matriz
        candidatos: Lista de pares candidatos
|#
(define (seleccion mat candidatos)
    (seleccionAux (objetivo mat candidatos) '(0 0))
)

(define (seleccionAux candidatosPuntuados mejor)
    (cond
    [(null? candidatosPuntuados) mejor]
    [(< (cadr mejor) (cadar candidatosPuntuados)) (seleccionAux (cdr candidatosPuntuados) (car candidatosPuntuados))]
    [else (seleccionAux (cdr candidatosPuntuados) mejor)]
    )
)

#|
    Función objetivo: Puntua los candidatos
    Entrada:
        mat: Matriz
        candidatos: Lista de candidatos
|#
(define (objetivo mat candidatos)
    (objetivoAux mat candidatos '())
)

(define (objetivoAux mat candidatos candidatosPuntuados)
    ; (displayln candidatos)
    (cond
    [(null? candidatos) candidatosPuntuados]
    [else (objetivoAux mat (cdr candidatos) (cons (puntuar mat (car candidatos)) candidatosPuntuados))]
    )
)

#|
    Función puntuar: suma los puntos 
|#
(define (puntuar mat candSinPts)
    (puntHoriz mat candSinPts (car candSinPts) (cadr candSinPts) (car candSinPts) (+ (car candSinPts) 2)  (length mat) (length (car mat)))
    candSinPts    
)

#|
    Funcion puntHoriz: Visita hasta 2 casillas horizontales más allá de
    la posición del candidato. Dependiendo de que se ubique en esas posiciones,
    aumenta o disminuye los puntos de la candidata.
    Entrada:
        mat: Matriz
        candSinPts: Par candidato con 0 puntos
        rPos: Posición de filas del candidato
        cPos: Posición en columnas del candidato
        i: Posición en filas de la casilla por visitar
        j: Posición en columnas de la casilla por visitar
        m: Número de filas de la matriz
        n: Número de columnas de la matriz
|#
(define (puntHoriz mat candSinPts rPos cPos i j m n)
    (display rPos)
    (displayln cPos)
    (cond)
)

; (define (puntAux mat candSinPts rPos cPos i j))


; (provide buildMatrix) ;;; Exporta la funcion para importarla desde otro archivo
(provide (all-defined-out)) ;Exporta todo