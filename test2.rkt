#lang racket
(require "AlgoritmoGoloso.rkt")

#|
    Una matriz 3x3, para ganas hay que colocar
    una bola en medio de otras 2.
|#

(println "-----crear matriz-----")
(define mat (buildMatrix 4 4))
(printMat mat)
(println "-----turno de jugador-----")
(set! mat (setValToPos mat 3 0 1))
(set! mat (setValToPos mat 1 2 1))
(printMat mat)
(println "-----turno de máquina-----")
(define mejorCandidato (seleccion mat))
(displayln mejorCandidato)