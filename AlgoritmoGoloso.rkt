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
    Crea una matriz de 0s.
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
    Devuelve el valor de la matriz que está en la posición
    de entrada.
    Entrada:
        mat: matriz
        i: posición en filas
        j: posición en columnas
|#
(define (getValInPos mat i j)
    (getValInPosAuxR mat i j 0 0)
)

(define (getValInPosAuxR mat rPos cPos i j)
    (cond
    [(null? mat) '()]
    [(= rPos i) (getValInPosAuxC (car mat) cPos j)]
    [else (getValInPosAuxR (cdr mat) rPos cPos (+ i 1) j)]
    )
)

(define (getValInPosAuxC raw cPos j)
    (cond
    [(null? raw) '()]
    [(= cPos j) (car raw)]
    [else (getValInPosAuxC (cdr raw) cPos (+ j 1))]
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
|#
(define (seleccion mat)
    (cond
    [(null? (viabilidad mat)) null]
    [else (seleccionAux (objetivo mat (viabilidad mat)) '(0 0))]
    )
)

(define (seleccionAux candidatosPuntuados mejor)
    (cond
    [(null? candidatosPuntuados) mejor]
    [(<= (cadr mejor) (cadar candidatosPuntuados)) (seleccionAux (cdr candidatosPuntuados) (car candidatosPuntuados))]
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
    (displayln (objetivoAux mat candidatos '()))
    (objetivoAux mat candidatos '())
)

(define (objetivoAux mat candidatos candidatosPuntuados)
    ; (displayln (list (car candidatos) '(0)))
    (cond
    [(null? candidatos) candidatosPuntuados]
    [else (objetivoAux mat (cdr candidatos) (cons (puntuar mat (cons (car candidatos) (list 0))) candidatosPuntuados))]
    )
)

#|
    Función puntuar: suma los puntos 
    Por cada tile vacía que rodee la tile candidata +1pt (pts max posibles 8pts)
    Por cada tile con un circulo +3pts (pts max posibles 18 pts)
    Por cada oportunidad de victoria (2 circulos seguidos en vertical, horizontal o diagonal) +25ps (pts max posibles 150pts)
    Por cada tile con una equis -3pts (pts minimos posibles -18 pts)
    Por cada oportunidad de derrota (2 equis seguidas en vertical, horizontal o diagonal) +20ps (pts max posibles 120pts)
    Entrada:
        mat: Matriz 
        candSinPts: Candidato junto con sus puntos. Ej: ((i j) 3pts)
|#
(define (puntuar mat candSinPts)
    ; (horzPts mat candSinPts (cadar candSinPts) (caar candSinPts) (- (cadar candSinPts) 1) (length (car mat)))
    ; (vertPts mat candSinPts (caar candSinPts) (- (caar candSinPts) 1) (cadar candSinPts)  (length mat) (length (car mat)))
    ; (diagPts mat candSinPts (caar candSinPts) (cadar candSinPts) (- (caar candSinPts) 1) (- (cadar candSinPts) 1) (length mat) (length (car mat)))
    ; (inDiagPts mat candSinPts (caar candSinPts) (cadar candSinPts) (- (caar candSinPts) 1) (+ (cadar candSinPts) 1) (length mat) (length (car mat)))
    
    (vertPts mat (horzPts mat (diagPts mat (inDiagPts mat candSinPts (caar candSinPts) (cadar candSinPts) (- (caar candSinPts) 1) (+ (cadar candSinPts) 1) (length mat) (length (car mat)) 0)
    (caar candSinPts) (cadar candSinPts) (- (caar candSinPts) 1) (- (cadar candSinPts) 1) (length mat) (length (car mat)) 0)
    (cadar candSinPts) (caar candSinPts) (- (cadar candSinPts) 1) (length (car mat)) 0)
    (caar candSinPts) (- (caar candSinPts) 1) (cadar candSinPts)  (length mat) (length (car mat)) 0)
)

#|
    Función inDiagPts: puntua las casillas vecinas antidiagonales
    a la casilla vacía.
    Entrada:
        mat: Matriz
        cand: Candidato
        rPos: Posición en filas de la casilla vacía
        cPos: Posición en columnas de la casilla vacía
        i: Posición en filas de la casilla visitada
        j: Posición en columnas de la casilla visitada
        m: Número de filas
        n: Número de columnas
|#
(define (inDiagPts mat cand rPos cPos i j m n lastTile)
    (cond
    [(or (< i 0) (and (= j cPos) (= i rPos))) (inDiagPts mat cand rPos cPos (+ i 1) (- j 1) m n lastTile)]
    [(or (>= i m) (>= j n) (and (= i (+ rPos 2)) (= j (- cPos 2)))) cand] 
    [(null? (getValInPos mat i j)) cand]
    [(= (getValInPos mat i j) 0) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) 1))) rPos cPos (+ i 1) (- j 1) m n lastTile)]
    [else 
        (cond
        [(and (= lastTile 1) (= (getValInPos mat i j) 1)) (cons (car cand) (list (+ (cadr cand) 22)))]
        [(and (= lastTile 2) (= (getValInPos mat i j) 2)) (cons (car cand) (list (+ (cadr cand) 23)))]
        [(and (> i rPos) (= (getValInPos mat i j) 1)) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) (inDPtsForOther1 mat rPos cPos (+ i 1) (- j 1) m n)))) rPos cPos (+ i 1) (- j 1) m n 1)]
        [(and (< i rPos) (= (getValInPos mat i j) 1)) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) (inDPtsForOther1 mat rPos cPos (- i 1) (+ j 1) m n)))) rPos cPos (+ i 1) (- j 1) m n 1)]
        [(and (< i rPos) (= (getValInPos mat i j) 2)) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) (inDPtsForOther2 mat rPos cPos (- i 1) (+ j 1) m n)))) rPos cPos (+ i 1) (- j 1) m n 2)]
        [(and (> i rPos) (= (getValInPos mat i j) 2)) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) (inDPtsForOther2 mat rPos cPos (+ i 1) (- j 1) m n)))) rPos cPos (+ i 1) (- j 1) m n 2)]
        )
    ]    
    )
)

#|
    Función inDPtsForOther1: Verifica si hay 2 casillas con un 1 alineadas de forma
    antidiagonal. Agrega 25pts si se cumple.
    Entrada:
        mat: Matriz
        rPos: Posición en filas de la casilla antidiagonal anterior
        cPos: Posición en columnas de la casilla antidiagonal anterior
        nextTileRPos: Posición en filas de la casilla visitada
        nextTileCPos: Posición en columna de la casilla visitada
        m: Número de filas
        n: Número de columnas
|#
(define (inDPtsForOther1 mat rPos cPos nextTileRPos nextTileCPos m n)
    (cond
    [(or (and (> nextTileRPos rPos) (< nextTileRPos m) (< nextTileCPos cPos) (>= nextTileCPos 0)) (and (< nextTileRPos rPos) (>= nextTileRPos 0) (> nextTileCPos cPos) (< nextTileCPos n)))
        ; (displayln "entró")
        ; (displayln (if (= (getValInPos mat nextTileRPos nextTileCPos) 1) 25 3))
        (if (= (getValInPos mat nextTileRPos nextTileCPos) 1) 25 3)
    ]
    [else 3]
    )
)

#|
    Función inDPtsForOther2: Verifica si hay 2 casillas con un 2 alineadas de forma
    antidiagonal. Agrega 20pts si se cumple.
    Entrada:
        mat: Matriz
        rPos: Posición en filas de la casilla antidiagonal anterior
        cPos: Posición en columnas de la casilla antidiagonal anterior
        nextTileRPos: Posición en filas de la casilla visitada
        nextTileCPos: Posición en columna de la casilla visitada
        m: Número de filas
        n: Número de columnas
|#
(define (inDPtsForOther2 mat rPos cPos nextTileRPos nextTileCPos m n)
    (cond
    [(or (and (> nextTileRPos rPos) (< nextTileRPos m) (< nextTileCPos cPos) (>= nextTileCPos 0)) (and (< nextTileRPos rPos) (>= nextTileRPos 0) (> nextTileCPos cPos) (< nextTileCPos n)))
        (if (= (getValInPos mat nextTileRPos nextTileCPos) 2) 20 -3)
    ]
    [else -3]
    )
)

#|
    Función diagPts: puntua las casillas vecinas diagonales
    a la casilla vacía.
    Entrada:
        mat: Matriz
        cand: Candidato
        rPos: Posición en filas de la casilla vacía
        cPos: Posición en columnas de la casilla vacía
        i: Posición en filas de la casilla visitada
        j: Posición en columnas de la casilla visitada
        m: Número de filas
        n: Número de columnas
|#
(define (diagPts mat cand rPos cPos i j m n lastTile)
    (cond
    [(or (< i 0) (< j 0) (= j cPos) (= i rPos)) (diagPts mat cand rPos cPos (+ i 1) (+ j 1) m n lastTile)]
    [(or (>= i m) (>= j n) (= i (+ rPos 2)) (= j (+ cPos 2))) cand] 
    [(= (getValInPos mat i j) 0) (diagPts mat (cons (car cand) (list (+ (cadr cand) 1))) rPos cPos (+ i 1) (+ j 1) m n lastTile)]
    [else 
        (cond
        [(and (= lastTile 1) (= (getValInPos mat i j) 1)) (cons (car cand) (list (+ (cadr cand) 22)))]
        [(and (= lastTile 2) (= (getValInPos mat i j) 2)) (cons (car cand) (list (+ (cadr cand) 23)))]
        [(and (> i rPos) (= (getValInPos mat i j) 1)) (diagPts mat (cons (car cand) (list (+ (cadr cand) (dPtsForOther1 mat rPos cPos (+ i 1) (+ j 1) m n)))) rPos cPos (+ i 1) (+ j 1) m n 1)]
        [(and (< i rPos) (= (getValInPos mat i j) 1)) (diagPts mat (cons (car cand) (list (+ (cadr cand) (dPtsForOther1 mat rPos cPos (- i 1) (- j 1) m n)))) rPos cPos (+ i 1) (+ j 1) m n 1)]
        [(and (< i rPos) (= (getValInPos mat i j) 2)) (diagPts mat (cons (car cand) (list (+ (cadr cand) (dPtsForOther2 mat rPos cPos (- i 1) (- j 1) m n)))) rPos cPos (+ i 1) (+ j 1) m n 2)]
        [(and (> i rPos) (= (getValInPos mat i j) 2)) (diagPts mat (cons (car cand) (list (+ (cadr cand) (dPtsForOther2 mat rPos cPos (+ i 1) (+ j 1) m n)))) rPos cPos (+ i 1) (+ j 1) m n 2)]
        )
    ]    
    )
)

#|
    Función dPtsForOther1: Verifica si hay 2 casillas con un 1 alineadas de forma
    diagonal. Agrega 25pts si se cumple.
    Entrada:
        mat: Matriz
        rPos: Posición en filas de la casilla antidiagonal anterior
        cPos: Posición en columnas de la casilla antidiagonal anterior
        nextTileRPos: Posición en filas de la casilla visitada
        nextTileCPos: Posición en columna de la casilla visitada
        m: Número de filas
        n: Número de columnas
|#
(define (dPtsForOther1 mat rPos cPos nextTileRPos nextTileCPos m n)
    (cond
    [(or (and (> nextTileRPos rPos) (< nextTileRPos m) (> nextTileCPos cPos) (< nextTileCPos n)) (and (< nextTileRPos rPos) (>= nextTileRPos 0) (< nextTileCPos cPos) (>= nextTileCPos 0)))
        ; (displayln "entró")
        ; (displayln (if (= (getValInPos mat nextTileRPos nextTileCPos) 1) 25 3))
        (if (= (getValInPos mat nextTileRPos nextTileCPos) 1) 25 3)
    ]
    [else 3]
)
)

#|
    Función dPtsForOther2: Verifica si hay 2 casillas con un 2 alineadas de forma
    diagonal. Agrega 20pts si se cumple.
    Entrada:
        mat: Matriz
        rPos: Posición en filas de la casilla antidiagonal anterior
        cPos: Posición en columnas de la casilla antidiagonal anterior
        nextTileRPos: Posición en filas de la casilla visitada
        nextTileCPos: Posición en columna de la casilla visitada
        m: Número de filas
        n: Número de columnas
|#
(define (dPtsForOther2 mat rPos cPos nextTileRPos nextTileCPos m n)
    (cond
    [(or (and (> nextTileRPos rPos) (< nextTileRPos m) (> nextTileCPos cPos) (< nextTileCPos n)) (and (< nextTileRPos rPos) (>= nextTileRPos 0) (< nextTileCPos cPos) (>= nextTileCPos 0) ))
        (if (= (getValInPos mat nextTileRPos nextTileCPos) 2) 20 -3)
    ]
    [else -3]
    )
)

#|
    Función vertPts: puntua las casillas vecinas verticales
    a la casilla vacía.
    Entrada:
        mat: Matriz
        cand: Candidato
        rPos: Posición en filas de la casilla vacía
        i: Posición en filas de la casilla visitada
        j: Posición en columnas de la casilla visitada
        m: Número de filas
        n: Número de columnas
|#
(define (vertPts mat cand rPos i j m n lastTile)
    (cond
    [(or (< i 0) (= i rPos)) (vertPts mat cand rPos (+ i 1) j m n lastTile)]
    [(or (>= i m) (= i (+ rPos 2))) cand] 
    [(= (getValInPos mat i j) 0) (vertPts mat (cons (car cand) (list (+ (cadr cand) 1))) rPos (+ i 1) j m n lastTile)]
    [else 
        (cond
        [(and (= lastTile 1) (= (getValInPos mat i j) 1)) (cons (car cand) (list (+ (cadr cand) 22)))]
        [(and (= lastTile 2) (= (getValInPos mat i j) 2)) (cons (car cand) (list (+ (cadr cand) 23)))]
        [(and (> i rPos) (= (getValInPos mat i j) 1)) (vertPts mat (cons (car cand) (list (+ (cadr cand) (vPtsForOther1 mat rPos (+ i 1) j m)))) rPos (+ i 1) j m n 1)]
        [(and (< i rPos) (= (getValInPos mat i j) 1)) (vertPts mat (cons (car cand) (list (+ (cadr cand) (vPtsForOther1 mat rPos (- i 1) j m)))) rPos (+ i 1) j m n 1)]
        [(and (< i rPos) (= (getValInPos mat i j) 2)) (vertPts mat (cons (car cand) (list (+ (cadr cand) (vPtsForOther2 mat rPos (- i 1) j m)))) rPos (+ i 1) j m n 2)]
        [(and (> i rPos) (= (getValInPos mat i j) 2)) (vertPts mat (cons (car cand) (list (+ (cadr cand) (vPtsForOther2 mat rPos (+ i 1) j m)))) rPos (+ i 1) j m n 2)]
        )
    ]    
    )
)

#|
    Función vPtsForOther1: Verifica si hay 2 casillas con un 1 alineadas de forma
    vertical. Agrega 25pts si se cumple.
    Entrada:
        mat: Matriz
        rPos: Posición en filas de la casilla antidiagonal anterior
        nextTileRPos: Posición en filas de la casilla visitada
        nextTileCPos: Posición en columna de la casilla visitada
        m: Número de filas
|#
(define (vPtsForOther1 mat rPos nextTileRPos nextTileCPos m)
    (cond
    [(or (and (> nextTileRPos rPos) (< nextTileRPos m)) (and (< nextTileRPos rPos) (>= nextTileRPos 0)))
        ; (displayln "entró")
        ; (displayln (if (= (getValInPos mat nextTileRPos nextTileCPos) 1) 25 3))
        (if (= (getValInPos mat nextTileRPos nextTileCPos) 1) 25 3)
    ]
    [else 3]
    )
)

#|
    Función vPtsForOther2: Verifica si hay 2 casillas con un 2 alineadas de forma
    vertical. Agrega 20pts si se cumple.
    Entrada:
        mat: Matriz
        rPos: Posición en filas de la casilla antidiagonal anterior
        nextTileRPos: Posición en filas de la casilla visitada
        nextTileCPos: Posición en columna de la casilla visitada
        n: Número de columnas
|#
(define (vPtsForOther2 mat rPos nextTileRPos nextTileCPos n)
    (cond
    [(or (and (> nextTileRPos rPos) (< nextTileRPos n)) (and (< nextTileRPos rPos) (>= nextTileRPos 0)))
        (if (= (getValInPos mat nextTileRPos nextTileCPos) 2) 20 -3)
    ]
    [else -3]
    )
)


#|
    Función vertPts: puntua las casillas vecinas horizontales
    a la casilla vacía.
    Entrada:
        mat: Matriz
        cand: Candidato
        cPos: Posición en columnas de la casilla vacía
        i: Posición en filas de la casilla visitada
        j: Posición en columnas de la casilla visitada
        n: Número de columnas
|#
(define (horzPts mat cand cPos i j n lastTile)
    (cond
    [(or (< j 0) (= j cPos)) (horzPts mat cand cPos i (+ j 1) n lastTile)]
    [(or (>= j n) (= j (+ cPos 2))) cand] 
    [(= (getValInPos mat i j) 0) (horzPts mat (cons (car cand) (list (+ (cadr cand) 1))) cPos i (+ j 1) n lastTile)]
    [else 
        (cond
        [(and (= lastTile 1) (= (getValInPos mat i j) 1)) (cons (car cand) (list (+ (cadr cand) 22)))]
        [(and (= lastTile 2) (= (getValInPos mat i j) 2)) (cons (car cand) (list (+ (cadr cand) 23)))]
        [(and (< j cPos) (= (getValInPos mat i j) 1)) (horzPts mat (cons (car cand) (list (+ (cadr cand) (hPtsForOther1 mat cPos i (- j 1) n)))) cPos i (+ j 1) n 1)]
        [(and (> j cPos) (= (getValInPos mat i j) 1)) (horzPts mat (cons (car cand) (list (+ (cadr cand) (hPtsForOther1 mat cPos i (+ j 1) n)))) cPos i (+ j 1) n 1)]
        [(and (< j cPos) (= (getValInPos mat i j) 2)) (horzPts mat (cons (car cand) (list (+ (cadr cand) (hPtsForOther2 mat cPos i (- j 1) n)))) cPos i (+ j 1) n 2)]
        [(and (> j cPos) (= (getValInPos mat i j) 2)) (horzPts mat (cons (car cand) (list (+ (cadr cand) (hPtsForOther2 mat cPos i (+ j 1) n)))) cPos i (+ j 1) n 2)]
        )
    ]    
    )
)

#| 
    Función vPtsForOther1: Verifica si hay 2 casillas con un 1 alineadas de forma
    horizontal. Agrega 25pts si se cumple.
    Entrada:
        mat: Matriz
        cPos: Posición en columnas de la casilla antidiagonal anterior
        nextTileRPos: Posición en filas de la casilla visitada
        nextTileCPos: Posición en columna de la casilla visitada
        n: Número de columnas
|#
(define (hPtsForOther1 mat cPos nextTileRPos nextTileCPos n)
    (cond
    [(or (and (> nextTileCPos cPos) (< nextTileCPos n)) (and (< nextTileCPos cPos) (>= nextTileCPos 0)))
        ; (displayln "entró")
        ; (displayln (if (= (getValInPos mat nextTileRPos nextTileCPos) 1) 25 3))
        (if (= (getValInPos mat nextTileRPos nextTileCPos) 1) 25 3)
    ]
    [else 3]
    )
)

#|
    Función vPtsForOther2: Verifica si hay 2 casillas con un 2 alineadas de forma
    horizontal. Agrega 20pts si se cumple.
    Entrada:
        mat: Matriz
        cPos: Posición en columnas de la casilla antidiagonal anterior
        nextTileRPos: Posición en filas de la casilla visitada
        nextTileCPos: Posición en columna de la casilla visitada
        n: Número de columnas
|#
(define (hPtsForOther2 mat cPos nextTileRPos nextTileCPos n)
    (cond
    [(or (and (> nextTileCPos cPos) (< nextTileCPos n)) (and (< nextTileCPos cPos) (>= nextTileCPos 0)))
        (if (= (getValInPos mat nextTileRPos nextTileCPos) 2) 20 -3)
    ]
    [else -3]
    )
)

;;; (provide buildMatrix) ;;; Exporta la funcion para importarla desde otro archivo
;;; (provide setValToPos)
;;; (provide getValInPos)
;;; (provide printMat)
;;; (provide seleccion)
(provide (all-defined-out)) ;Exporta todo