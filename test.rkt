#lang racket
(require "AlgoritmoCodicioso.rkt")

#|
    Una matriz 3x3, para ganas hay que colocar
    una bola en medio de otras 2.
|#

(println "-----crear matriz-----")
(define mat (buildMatrix 3 3))
(printMat mat)
; (println "-----caso 1-----")
; (set! mat (setValToPos mat 2 0 1))
; (set! mat (setValToPos mat 1 2 1))
; (println "-----caso 2-----")
; (set! mat (setValToPos mat 0 0 1))
; (set! mat (setValToPos mat 1 1 1))
(println "-----caso 3-----")
(set! mat (setValToPos mat 0 0 2))
(set! mat (setValToPos mat 1 2 2))
(printMat mat)
(println "-----candidatos puntuados-----")
(define candidatos (viabilidad mat))
(define candConPuntos (objetivo mat candidatos))
(displayln candConPuntos)