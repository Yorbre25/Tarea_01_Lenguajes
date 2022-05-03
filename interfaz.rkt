#lang racket

(require racket/gui/base)
(require table-panel)  ;;; raco pkg install table-panel

;;; (require "AlgoritmoGoloso.rkt")

(define (get_dimensions dc)
    (list-ref (call-with-values (thunk (send dc get-size)) list) 0))

(define (calc_margin dc)
    (floor (/ (get_dimensions dc) 5)))

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

(define (select_cell dc event)
    (displayln dc)
    (cond
        [(not event)
            (draw_O dc)
            (draw_X dc)])
)

(define cell% 
    (class canvas%
        (define/override (on-event event)
            (cond
                [(equal? (send event get-event-type) 'left-up) 
                    (lambda (cell dc) (select_cell dc #f))]))
        (super-new)
    )    
)

(define (generate_board grid rows cols)
    (for ([i (in-range rows)])
        (for ([j (in-range cols)])
            (define cell (new cell% [parent grid]
                                    [paint-callback (lambda (cell dc) (select_cell dc #f))]))

            (send cell set-canvas-background (make-object color% 255 255 255 1))
            (send (send cell get-dc) set-text-foreground (make-object color% 0 0 0 1))
        )
    )
)

(define (calc_cell_dimensions rows cols)
    (cond
        [(>= rows cols) (floor (/ (- 375 (* 25 rows)) 2))]
        [(< rows cols) (floor (/ (- 375 (* 25 cols)) 2))]
    )

)

(define (TTT rows cols)
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