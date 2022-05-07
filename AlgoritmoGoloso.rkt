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
    (horzPts mat candSinPts (cadar candSinPts) (caar candSinPts) (- (cadar candSinPts) 1)  (length mat) (length (car mat)))
    ; (vertPts mat candSinPts (caar candSinPts) (- (caar candSinPts) 1) (cadar candSinPts)  (length mat) (length (car mat)))
    ; (diagPts mat candSinPts (caar candSinPts) (cadar candSinPts) (- (caar candSinPts) 1) (- (cadar candSinPts) 1) (length mat) (length (car mat)))
    ; (inDiagPts mat candSinPts (caar candSinPts) (cadar candSinPts) (- (caar candSinPts) 1) (+ (cadar candSinPts) 1) (length mat) (length (car mat)))
)

(define (inDiagPts mat cand rPos cPos i j m n)
    (display rPos)
    (displayln cPos)
    (display i)
    (displayln j)
    (cond
    [(or (< i 0) (and (= j cPos) (= i rPos))) (inDiagPts mat cand rPos cPos (+ i 1) (- j 1) m n)]
    [(or (>= i m) (>= j n) (and (= i (+ rPos 2)) (= j (- cPos 2)))) cand] 
    [(null? (getValInPos mat i j)) cand]
    [(= (getValInPos mat i j) 0) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) 1))) rPos cPos (+ i 1) (- j 1) m n)]
    [else 
        (cond
        [(and (> i rPos) (= (getValInPos mat i j) 1)) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) (inDPtsForOtherO mat rPos cPos (+ i 1) (- j 1) m n)))) rPos cPos (+ i 1) (- j 1) m n)]
        [(and (< i rPos) (= (getValInPos mat i j) 1)) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) (inDPtsForOtherO mat rPos cPos (- i 1) (+ j 1) m n)))) rPos cPos (+ i 1) (- j 1) m n)]
        [(and (< i rPos) (= (getValInPos mat i j) 2)) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) (inDPtsForOtherX mat rPos cPos (- i 1) (+ j 1) m n)))) rPos cPos (+ i 1) (- j 1) m n)]
        [(and (> i rPos) (= (getValInPos mat i j) 2)) (inDiagPts mat (cons (car cand) (list (+ (cadr cand) (inDPtsForOtherX mat rPos cPos (+ i 1) (- j 1) m n)))) rPos cPos (+ i 1) (- j 1) m n)]
        )
    ]    
    )
)


(define (inDPtsForOtherO mat rPos cPos notEmpTileRPos notEmpTileCPos m n)
    ; (displayln "entró for")
    ; (display notEmpTileRPos)
    ; (displayln notEmpTileCPos)
    ; (displayln (getValInPos mat notEmpTileRPos notEmpTileCPos))
    (cond
    [(or (and (> notEmpTileRPos rPos) (< notEmpTileRPos m) (< notEmpTileCPos cPos) (>= notEmpTileCPos 0)) (and (< notEmpTileRPos rPos) (>= notEmpTileRPos 0) (> notEmpTileCPos cPos) (< notEmpTileCPos n)))
        ; (displayln "entró")
        ; (displayln (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 1) 25 3))
        (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 1) 25 3)
    ]
    [else 3]
    )
)

(define (inDPtsForOtherX mat rPos cPos notEmpTileRPos notEmpTileCPos m n)
    (cond
    [(or (and (> notEmpTileRPos rPos) (< notEmpTileRPos m) (< notEmpTileCPos cPos) (>= notEmpTileCPos 0)) (and (< notEmpTileRPos rPos) (>= notEmpTileRPos 0) (> notEmpTileCPos cPos) (< notEmpTileCPos n)))
        (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 2) 20 -3)
    ]
    [else -3]
    )
)

#|
    
|#
(define (diagPts mat cand rPos cPos i j m n)
    (cond
    [(or (< i 0) (< j 0) (= j cPos) (= i rPos)) (diagPts mat cand rPos cPos (+ i 1) (+ j 1) m n)]
    [(or (>= i m) (>= j n) (= i (+ rPos 2)) (= j (+ cPos 2))) cand] 
    [(= (getValInPos mat i j) 0) (diagPts mat (cons (car cand) (list (+ (cadr cand) 1))) rPos cPos (+ i 1) (+ j 1) m n)]
    [else 
        (cond
        [(and (> i rPos) (= (getValInPos mat i j) 1)) (diagPts mat (cons (car cand) (list (+ (cadr cand) (dPtsForOtherO mat rPos cPos (+ i 1) (+ j 1) m n)))) rPos cPos (+ i 1) (+ j 1) m n)]
        [(and (< i rPos) (= (getValInPos mat i j) 1)) (diagPts mat (cons (car cand) (list (+ (cadr cand) (dPtsForOtherO mat rPos cPos (- i 1) (- j 1) m n)))) rPos cPos (+ i 1) (+ j 1) m n)]
        [(and (< i rPos) (= (getValInPos mat i j) 2)) (diagPts mat (cons (car cand) (list (+ (cadr cand) (dPtsForOtherX mat rPos cPos (- i 1) (- j 1) m n)))) rPos cPos (+ i 1) (+ j 1) m n)]
        [(and (> i rPos) (= (getValInPos mat i j) 2)) (diagPts mat (cons (car cand) (list (+ (cadr cand) (dPtsForOtherX mat rPos cPos (+ i 1) (+ j 1) m n)))) rPos cPos (+ i 1) (+ j 1) m n)]
        )
    ]    
    )
)

(define (dPtsForOtherO mat rPos cPos notEmpTileRPos notEmpTileCPos m n)
    ; (displayln rPos)
    ; (display notEmpTileRPos)
    ; (displayln notEmpTileCPos)
    ; (displayln (getValInPos mat notEmpTileRPos notEmpTileCPos))
    (cond
    [(or (and (> notEmpTileRPos rPos) (< notEmpTileRPos m) (> notEmpTileCPos cPos) (< notEmpTileCPos n)) (and (< notEmpTileRPos rPos) (>= notEmpTileRPos 0) (< notEmpTileCPos cPos) (>= notEmpTileCPos 0)))
        ; (displayln "entró")
        ; (displayln (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 1) 25 3))
        (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 1) 25 3)
    ]
    [else 3]
)
)

(define (dPtsForOtherX mat rPos cPos notEmpTileRPos notEmpTileCPos m n)
    (cond
    [(or (and (> notEmpTileRPos rPos) (< notEmpTileRPos m) (> notEmpTileCPos cPos) (< notEmpTileCPos n)) (and (< notEmpTileRPos rPos) (>= notEmpTileRPos 0) (< notEmpTileCPos cPos) (>= notEmpTileCPos 0) ))
        (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 2) 20 -3)
    ]
    [else -3]
    )
)

#|
    
|#
(define (vertPts mat cand rPos i j m n)
    (cond
    [(or (< i 0) (= i rPos)) (vertPts mat cand rPos (+ i 1) j m n)]
    [(or (>= i m) (= i (+ rPos 2))) cand] 
    [(= (getValInPos mat i j) 0) (vertPts mat (cons (car cand) (list (+ (cadr cand) 1))) rPos (+ i 1) j m n)]
    [else 
        (cond
        [(and (> i rPos) (= (getValInPos mat i j) 1)) (vertPts mat (cons (car cand) (list (+ (cadr cand) (vPtsForOtherO mat rPos (+ i 1) j m)))) rPos (+ i 1) j m n)]
        [(and (< i rPos) (= (getValInPos mat i j) 1)) (vertPts mat (cons (car cand) (list (+ (cadr cand) (vPtsForOtherO mat rPos (- i 1) j m)))) rPos (+ i 1) j m n)]
        [(and (< i rPos) (= (getValInPos mat i j) 2)) (vertPts mat (cons (car cand) (list (+ (cadr cand) (vPtsForOtherX mat rPos (- i 1) j m)))) rPos (+ i 1) j m n)]
        [(and (> i rPos) (= (getValInPos mat i j) 2)) (vertPts mat (cons (car cand) (list (+ (cadr cand) (vPtsForOtherX mat rPos (+ i 1) j m)))) rPos (+ i 1) j m n)]
        )
    ]    
    )
)

(define (vPtsForOtherO mat rPos notEmpTileRPos notEmpTileCPos m) ;* Los nombres de los parametros son mejorables
    ; (displayln "entró for")
    ; (display notEmpTileRPos)
    ; (displayln notEmpTileCPos)
    ; (displayln (getValInPos mat notEmpTileRPos notEmpTileCPos))
    (cond
    [(or (and (> notEmpTileRPos rPos) (< notEmpTileRPos m)) (and (< notEmpTileRPos rPos) (>= notEmpTileRPos 0)))
        ; (displayln "entró")
        ; (displayln (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 1) 25 3))
        (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 1) 25 3)
    ]
    [else 3]
    )
)

(define (vPtsForOtherX mat rPos notEmpTileRPos notEmpTileCPos n)
    (cond
    [(or (and (> notEmpTileRPos rPos) (< notEmpTileRPos n)) (and (< notEmpTileRPos rPos) (>= notEmpTileRPos 0)))
        (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 2) 20 -3)
    ]
    [else -3]
    )
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
(define (horzPts mat cand cPos i j m n)
    ; (display i)
    ; (displayln j)
    ; (displayln (getValInPos mat i j))
    (cond
    [(or (< j 0) (= j cPos)) (horzPts mat cand cPos i (+ j 1) m n)]
    [(or (>= j n) (= j (+ cPos 2))) cand] 
    [(= (getValInPos mat i j) 0) (horzPts mat (cons (car cand) (list (+ (cadr cand) 1))) cPos i (+ j 1) m n)]
    [else 
        (cond
        [(and (< j cPos) (= (getValInPos mat i j) 1)) (horzPts mat (cons (car cand) (list (+ (cadr cand) (hPtsForOtherO mat cPos i (- j 1) n)))) cPos i (+ j 1) m n)]
        [(and (> j cPos) (= (getValInPos mat i j) 1)) (horzPts mat (cons (car cand) (list (+ (cadr cand) (hPtsForOtherO mat cPos i (+ j 1) n)))) cPos i (+ j 1) m n)]
        [(and (< j cPos) (= (getValInPos mat i j) 2)) (horzPts mat (cons (car cand) (list (+ (cadr cand) (hPtsForOtherX mat cPos i (- j 1) n)))) cPos i (+ j 1) m n)]
        [(and (> j cPos) (= (getValInPos mat i j) 2)) (horzPts mat (cons (car cand) (list (+ (cadr cand) (hPtsForOtherX mat cPos i (+ j 1) n)))) cPos i (+ j 1) m n)]
        )
    ]    
    )
)
; (cons (car cand) (list (+ (cadr cand) (hPtsForOtherO mat rPos cPos i j n))))

#|

|#
(define (hPtsForOtherO mat cPos notEmpTileRPos notEmpTileCPos n) ;* Los nombres de los parametros son mejorables
    ; (displayln "entró for")
    ; (display notEmpTileRPos)
    ; (displayln notEmpTileCPos)
    ; (displayln (getValInPos mat notEmpTileRPos notEmpTileCPos))
    (cond
    [(or (and (> notEmpTileCPos cPos) (< notEmpTileCPos n)) (and (< notEmpTileCPos cPos) (>= notEmpTileCPos 0)))
        ; (displayln "entró")
        ; (displayln (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 1) 25 3))
        (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 1) 25 3)
    ]
    [else 3]
    )
)

(define (hPtsForOtherX mat cPos notEmpTileRPos notEmpTileCPos n)
    (cond
    [(or (and (> notEmpTileCPos cPos) (< notEmpTileCPos n)) (and (< notEmpTileCPos cPos) (>= notEmpTileCPos 0)))
        (if (= (getValInPos mat notEmpTileRPos notEmpTileCPos) 2) 20 -3)
    ]
    [else -3]
    )
)

; (provide buildMatrix) ;;; Exporta la funcion para importarla desde otro archivo
(provide (all-defined-out)) ;Exporta todo