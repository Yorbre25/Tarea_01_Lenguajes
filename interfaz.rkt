#lang racket

(require racket/gui/base)
(require table-panel)  ;;; raco pkg install table-panel

(require "AlgoritmoCodicioso.rkt")
(require "FinDeJuego.rkt")

(define player_turn #t)
(define cell_matrix (buildMatrix 3 3))
(define value_matrix (buildMatrix 3 3))

(define (game_result msg)
    (define game_result (new dialog% [label "Resultado de la partida"]
                                        [min-width 400]
                                        [min-height 150]
                                        [alignment '(center center) ]
                                        [stretchable-width #f]	 
                                        [stretchable-height #f]))

    (define message (new message% [parent game_result]
                                    [label msg]
                                    [font (make-object font% 24 'default)]))

    (send game_result show #t)
    (exit #t)
)

(define (get_dimensions dc)
    (list-ref (call-with-values (thunk (send dc get-size)) list) 0)
)

(define (calc_margin dc)
    (floor (/ (get_dimensions dc) 5))
)

(define (draw_X dc)
    (send dc set-pen (new pen% [color (make-object color% 250 112 112 1)] [width 4]))

    (send dc draw-line (calc_margin dc) (calc_margin dc)
                       (- (get_dimensions dc) (calc_margin dc)) (- (get_dimensions dc) (calc_margin dc)))

    (send dc draw-line (- (get_dimensions dc) (calc_margin dc)) (calc_margin dc)
                       (calc_margin dc) (- (get_dimensions dc) (calc_margin dc)))
)

(define (draw_O dc)
    (send dc set-pen (new pen% [color (make-object color% 94 247 125 1)] [width 4]))

    (send dc draw-ellipse (calc_margin dc) (calc_margin dc)
                          (- (get_dimensions dc) (* 2 (calc_margin dc))) (- (get_dimensions dc) (* 2 (calc_margin dc))))
)

(define (select_cell cell)
    (cond 
        [(and player_turn (eq? 0 (getValInPos value_matrix (send cell get-row) (send cell get-col))))
            (draw_X (send cell get-dc))
            (set! value_matrix (setValToPos value_matrix (send cell get-row) (send cell get-col) 2))
            (set! player_turn #f)
            (cond 
                [(checkGameStatus value_matrix 2)
                    (game_result "Ganó el jugador")]
                [else 
                    (sleep 0.5)
                    (enemy_turn)
                ]
            )
        ])
)

(define (enemy_turn)
    (define mejorCandidato (seleccion value_matrix))

    (draw_O (send (getValInPos cell_matrix (caar mejorCandidato) (cadar mejorCandidato)) get-dc))
    (set! value_matrix (setValToPos value_matrix (caar mejorCandidato) (cadar mejorCandidato) 1))
    (cond 
        [(checkGameStatus value_matrix 1)
            (game_result "Ganó la máquina")]
        [else 
            (set! player_turn #t)]
    )
)

(define cell% 
    (class canvas%
        (init row)
        (init col)

        (define row_position row)
        (define col_position col)

        (super-new)

        (define/public (get-row) row_position)
        (define/public (get-col) col_position)
        
        (define/override (on-event event)
            (when (eq? (send event get-event-type) 'left-up)
               (select_cell this)))  
    )
)

(define (generate_board grid rows cols)
    (set! cell_matrix (buildMatrix rows cols))
    (set! value_matrix (buildMatrix rows cols))
    (for ([i (in-range rows)])
        (for ([j (in-range cols)])
            (define cell (new cell% [parent grid]
                                    [row i]
                                    [col j]))

            (send cell set-canvas-background (make-object color% 255 255 255 1))
            (send (send cell get-dc) set-text-foreground (make-object color% 0 0 0 1))

            (set! cell_matrix (setValToPos cell_matrix i j cell))
        )
    )
)

(define (calc_cell_dimensions rows cols)
    (cond
        [(>= rows cols) (floor (/ (- 375 (* 25 rows)) 2))]
        [(< rows cols) (floor (/ (- 375 (* 25 cols)) 2))]
    )

)

(define (TTT cols rows)
    (define frame (new frame% [label "TicTacToe"] 
                              [min-width (* cols (calc_cell_dimensions rows cols))] 
                              [min-height (* rows (calc_cell_dimensions rows cols))]
                              [stretchable-width #f]	 
                              [stretchable-height #f]))

    (define grid (new table-panel% [parent frame]
                                   [dimensions (list rows cols)]
                                   [alignment '(center center)]
                                   [spacing 2]))

    (generate_board grid rows cols)

    (send frame show #t)
)

(TTT 3 3)