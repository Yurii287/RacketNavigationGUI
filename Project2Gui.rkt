#lang racket/gui
(require racket/gui/base)

; Input field for destination and current location
; Input field for what comfort level they desire
; Input field for what transport they will be taking: car, bus, train
;  Bus - returns bus stops that lead to the destination
;  Train - returns train stops that lead to the destination

; Return a list of directions between those places that guide the user
; Use/Create Data structures that capture a the path of the journey

(struct station (name prev_station next_station))

(define start_location "")
(define end_location "")

(define get_start_location (lambda (x)
                             (set! start_location x)
                             )
  )

(define get_end_location (lambda (x)
                           (set! end_location x)
                           )
  )

(define find_start_station (lambda (query station_list)
                            (cond
                              ([equal? query (station-name (list-ref stations (round (/ (length stations) 2))))] query)
                              ([string<? query (station-name (list-ref stations (round (/ (length stations) 2))))] query)
                              ([string>? query (station-name (list-ref stations (round (/ (length stations) 2))))] query)
                              )
                             )
  )

(define find_end_station (lambda (query station_list)
                            (cond
                              ([equal? query (station-name (list-ref stations (round (/ (length stations) 2))))] query)
                              ([string<? query (station-name (list-ref stations (round (/ (length stations) 2))))] query)
                              ([string>? query (station-name (list-ref stations (round (/ (length stations) 2))))] query)
                              )
                             )
  )

(define stations (list
                  (station "A" "" "B")
                  (station "B" "A" "C")
                  (station "C" "B" "D")
                  (station "D" "C" "E")
                  (station "E" "D" "")
                  )
  )