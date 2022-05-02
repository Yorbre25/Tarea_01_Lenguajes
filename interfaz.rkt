#lang racket
(require racket/gui/base)
(require table-panel)  ;;; raco pkg install table-panel

(define cell_dimensions 70)
(define spacing 2)

(define (TTT rows cols)
    (define frame (new frame% [label "TicTacToe"] 
                              [min-width (* cols cell_dimensions)] 
                              [min-height (* rows cell_dimensions)]
                              [stretchable-width #f]	 
                              [stretchable-height #f]))

    (define grid (new table-panel% [parent frame]
                                   [dimensions (list rows cols)]
                                   [alignment '(center center)]
                                   [spacing spacing]))

    (generate_board grid rows cols)


    (send frame show #t)
)

(define (generate_board grid rows cols)
    (for ([i (in-range rows)])
        (for ([j (in-range cols)])
            (define cell (new canvas% [parent grid]))
            (send cell set-canvas-background (make-object color% 255 255 255 1))
        )
    )
)

(TTT 3 3)