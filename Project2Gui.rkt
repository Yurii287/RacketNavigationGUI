#lang Racket

; Input field for destination and current location
; Input field for what comfort level they desire
; Input field for what transport they will be taking: car, bus, train
;  Car - returns roads to take
;  Bus - returns bus stops that lead to the destination
;  Train - returns train stops that lead to the destination
;  Walk - returns the paths to walk to a destination
; Return a list of directions between those places that guide the user
;  - Use information from OpenStreetMaps if possible