#lang racket
(require "AlgoritmoGoloso.rkt")

#|
    Una matriz 4x4
|#

(println "-----crear matriz-----")
(define mat (buildMatrix 4 4))
(printMat mat)
(println "-----turno de jugador-----")
(set! mat (setValToPos mat 1 1 2))
(printMat mat)
(println "-----turno de máquina-----")
(define mejorCandidato (seleccion mat))
(displayln mejorCandidato)
(set! mat (setValToPos mat 3 1 1))
(printMat mat)
(println "-----turno de jugador-----")
(set! mat (setValToPos mat 1 2 2))
(printMat mat)
(println "-----turno de máquina-----")
(set! mejorCandidato (seleccion mat))
(displayln mejorCandidato)
(set! mat (setValToPos mat 3 1 1))
(printMat mat)
