#lang racket
(require racket/gui/base)
(require table-panel)  ;;; raco pkg install table-panel

(define cell_dimensions 150)
(define spacing 2)

(define (TTT rows cols)
    (define frame (new frame% [label "TicTacToe"] 
                              [min-width (* rows cell_dimensions)] 
                              [min-height (* cols cell_dimensions)]))

    (define grid (new table-panel% [parent frame]
                                   [dimensions (list rows cols)]
                                   [alignment '(center center)]
                                   [spacing spacing]))


    (define canvas1 (new canvas% [parent grid]))

    (define canvas2 (new canvas% [parent grid]))

    (define canvas3 (new canvas% [parent grid]))

    (define canvas4 (new canvas% [parent grid]))

    (define canvas5 (new canvas% [parent grid]))

    (define canvas6 (new canvas% [parent grid]))

    (define canvas7 (new canvas% [parent grid]))

    (define canvas8 (new canvas% [parent grid]))

    (define canvas9 (new canvas% [parent grid]))


    (send canvas1 set-canvas-background (make-object color% 255 255 255 1))
    (send canvas2 set-canvas-background (make-object color% 255 255 255 1))
    (send canvas3 set-canvas-background (make-object color% 255 255 255 1))
    (send canvas4 set-canvas-background (make-object color% 255 255 255 1))
    (send canvas5 set-canvas-background (make-object color% 255 255 255 1))
    (send canvas6 set-canvas-background (make-object color% 255 255 255 1))
    (send canvas7 set-canvas-background (make-object color% 255 255 255 1))
    (send canvas8 set-canvas-background (make-object color% 255 255 255 1))
    (send canvas9 set-canvas-background (make-object color% 255 255 255 1))

    (send frame show #t)
)

(TTT 3 3)