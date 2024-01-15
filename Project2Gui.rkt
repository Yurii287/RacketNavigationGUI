#lang racket/gui

; Input field for destination and current location
; Input field for what comfort level they desire
; Input field for what transport they will be taking: car, bus, train
;  Car - returns roads to take
;  Bus - returns bus stops that lead to the destination
;  Train - returns train stops that lead to the destination
;  Walk - returns the paths to walk to a destination
; Return a list of directions between those places that guide the user
;  - Use information from OpenStreetMaps if possible
; A* Algorithm for finding the shortest distance between places
;  - Explain why this and not other algorithms
; Use/Create Data structures that capture a the path of the journey
;  - Create our own
;    - struct with args (current_location, desired_location, distance)?
;  - Use vectors

; create function to set variable as start_location and end_location



(require racket/gui/base)

(define mainFrame (new frame%
                       [label "Racket GUI Navigation"]))

(define inputPanel (new vertical-panel%
                        [parent mainFrame]
                        [alignment '(center center)]
                        [style (list 'border)]))

(define start_location (new text-field%
                            [label "Start Location"]
                            [parent inputPanel]
                            [stretchable-width #f]))

(define end_location (new text-field%
                          [label "End Location"]
                          [parent inputPanel]
                          [stretchable-width #f]))

(send mainFrame show #t)
(send mainFrame resize 390 844)
