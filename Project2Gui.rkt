#lang racket/gui
(require racket/gui/base)

; Input field for destination and current location
; Input field for what comfort level they desire
; Input field for what transport they will be taking: car, bus, train
;  Car - returns roads to take
;  Bus - returns bus stops that lead to the destination
;  Train - returns train stops that lead to the destination
; Return a list of directions between those places that guide the user
;  - Use information from OpenStreetMaps if possible
; Use/Create Data structures that capture a the path of the journey

; potentially use binary search to find the index of the start and end locations
;  - use vector
;(station-position (vector-ref stations 0))

(struct station (name position))

(define start_location "C")
(define end_location "")

(define get_start_location (lambda (x)
                             (set! start_location x)))

(define get_end_location (lambda (x)
                           (set! end_location x)))

(define get_start_station (lambda (start station_list)
                            (cond
                              ([equal? start (station-name (vector-ref stations (round (/ (vector-length stations) 2))))] #t))))

(define stations (vector
                  (station "A" 1)
                  (station "B" 2)
                  (station "C" 3)
                  (station "D" 4)
                  (station "E" 5)))