#lang racket/gui
(define train_line%
  (class object%
    (super-new)
    (init-field (line_name "")(train_stations (list )) (train_connections (list)))
    (define/public train_station(λ () train_stations) )
    (define/public line_name_get(λ () line_name))
    (define/public train_connections_get (lambda () train_connections))
    (define/public train_stations_get(λ () (for/list ([ i train_stations]) (car i))))
    (define/public step_free_get(λ (x) (for/list ([i x]) (cond ((equal? #t (first(cdr (assoc i train_stations)))) "Yes") (else "No")   ))))
    (define/public step_free(λ (x) (cond ( (pair? (assoc x train_stations))(first (cdr (assoc x train_stations)))) (else #f)) ))
    )
    
    )
(define northern_line(new train_line%
                          [line_name "Northern Line"]
                          [train_stations '(
  ("South Wimbledon" #f) ("Colliers Wood" #f) ("Tooting Broadway" #f) ( "Tooting Bec" #f) ("Balham" #f) ("Clapham South" #f) ("Clapham Common" #f) ("Clapham North" #f)
  ("Stockwell" #f) ("Oval" #f) ("Battersea Power Station" #f) ("Waterloo" #f) ("Embankment" #f) ("Charing Cross" #f) ("Leicester Square" #f) ("Goodge Street" #f) ("Warren Street" #f) ("Mornington Crescent" #f) ("Chalk Farm" #f)
  ("Belsize Park" #f) ("Hampstead" #f) ("Brent Cross" #f) ("Colindale" #f) ("Burnt Oak" #f) ("Battersea" #t) ("Borough" #t) ("Camden Town" #t) ("Edgware" #t) ("Elephant & Castle" #t) ("Euston" #t)
  ("Finchley Central" #t) ("Golders Green" #t) ( "Hendon Central" #t) ("High Barnet" #t) ("Kennington" #t) ("King's Cross St Pancras" #t) ("London Bridge" #t) ("Mill Hill East" #t) ("Moorgate" #t) ("Morden" #t)
  ("Nine Elms" #t) ("StockWell" #t) ("Tottenham Court Road" #t) ("West Finchley" #t) ("Woodside Park" #t))]
                          [train_connections (list
                                           (list "Mordern" "South Wimbledon")
                                           (list "South Wimbledon" "Colliers Wood") (list "South Wimbledon" "Mordern")
                                           (list "Colliers Wood" "Tooting Broadway") (list "Colliers Wood" "South Wimbledon")
                                           (list "Tooting Broadway" "Tooting Bec") (list "Tooting Broadway" "Colliers Wood")
                                           (list "Tooting Bec" "Balham") (list "Tooting Bec" "Tooting Broadway")
                                           (list "Balham" "Clapham South") (list "Balham" "Tooting Bec")
                                           (list "Clapham South" "Clapham Common") (list "Clapham South" "Balham")
                                           (list "Clapham Common" "Clapham North") (list "Clapham Common" "Clapham South")
                                           (list "Clapham North" "Stockwell") (list "Clapham North" "Clapham Common")
                                           (list "Stockwell" "Oval") (list "Stockwell" "Clapham North")
                                           (list "Oval" "Kennington") (list "Oval" "Stockwell")
                                           (list "Kennington" "Waterloo") (list "Kennington" "Elephant & Castle") (list "Kennington" "Oval")

                                           (list "Waterloo" "Embankment") (list "Waterloo" "Kennington")
                                           (list "Embankment" "Charing Cross") (list "Embankment" "Waterloo")
                                           (list "Charing Cross" "Leicester Square") (list "Charing Cross" "Embankment")
                                           (list "Leicester Square" "Tottenham Court Road") (list "Leicester Square" "Charing Cross")
                                           (list "Tottenham Court Road" "Goodge Street") (list "Tottenham Court Road" "Leicester Square")
                                           (list "Goodge Street" "Warren Street") (list "Goodge Street" "Tottenham Court Road")
                                           (list "Warrent Street" "Euston") (list "Warren Street" "Goodge Street")

                                           (list "Elephant & Castle" "Borough") (list "Elephant & Castle" "Kennington")
                                           (list "Borough" "London Bridge") (list "Borough" "Elephant & Castle")
                                           (list "London Bridge" "Bank") (list "London Bridge" "Borough")
                                           (list "Bank" "Moorgate") (list "Bank" "London Bridge")
                                           (list "Moorgate" "Old Street") (list "Moorgate" "Bank")
                                           (list "Old Street" "Angel") (list "Old Street" "Moorgate")
                                           (list "Angel" "King's Cross St Pancras") (list "Angel" "Old Street")
                                           (list "King's Cross St Pancras" "Euston") (list "King's Cross St Pancras")
                                           )]
                          )
  )

(define victoria_line(new train_line% [line_name "Victoria Line"] [train_stations '( ("Brixton" #t) ("Stockwell" #t)  ("Vauxhall" #t)  ("Pimlico" #f)  ("Victoria" #t)    ("Green Park" #t)
                          ("Oxford Circus" #t) ("Warren Street" #f)  ("Euston" #t)  ("King's Cross St Pancras" #t)
                          ("Highbury & Islington" #f)  ("Finsbury Park" #t)  ("Seven Sisters" #f)  ("Tottenham Hale" #t)
                          ("Blackhorse Road" #f)  ("Walthamstow Central" #f) )]))
(define lines(list northern_line victoria_line))

(require racket/gui/base)

(define a-graph (list
                 (list "A" "C" 50)
                 (list "B" "D" 40)
                 (list "C" "E" 20) (list "C" "A" 50)
                 (list "D" "E" 35) (list "D" "B" 40)
                 (list "E" "F" 100) (list "E" "C" 20) (list "E" "D" 35)
                 (list "F" "E" 100) (list "F" "G" 20)
                 (list "G" "F" 20)
                 ))

(define nodes (lambda (graph)
                (remove-duplicates (flatten graph))))

(define edge (lambda (n1 n2 graph)
               (flatten (filter
                  (lambda (y)
                    (equal? (first (rest y)) n2))
                  (filter
                  (lambda (x)
                    (equal? (first x) n1))
                  graph)))))

               
(define children (lambda (n graph)
                   (map second 
                   (filter
                    (lambda (edge)
                      (equal? (first edge) n))
                    graph))))

(define leaf (lambda (n graph)
               (null? (children n graph))))

(define grand-children (lambda (n graph)
                         (remove n (flatten (map (lambda (node) (children node graph)) (children n graph))))))

(define weight (lambda (n1 n2 graph)
                 (first (reverse (edge n1 n2 graph)))))

(define branch (lambda (n graph branch-nodes)
                 (cond
                   ([equal? n "Kennington"] branch-nodes)
                   ([not (equal? n "Kennington")] (branch (first (children n graph)) graph (cons n branch-nodes)))
                   )
                 )
  )
 
(define get-path1 (lambda (n1 n2 graph directions)
                   (cond
                     ([and (not (not (member n1 directions))) (not (not (member n2 directions)))] (remove-duplicates (reverse directions)))
                     ([not (not (member n2 (children n1 graph)))] (get-path1 n1 n2 graph (cons n2 (cons n1 directions))))
                     ([and (equal? n1 "Kennington") (not (not (member n2 (branch "A" graph '()))))] (get-path1 (second (children "E" graph)) n2 graph (cons n1 directions)))
                     ([and (equal? n1 "Kennington") (not (not (member n2 (branch "B" graph '()))))] (get-path1 (third (children "E" graph)) n2 graph (cons n1 directions))) 
                     ([and (equal? n1 "Kennington") (not (not (member n2 (branch "G" graph '()))))] (get-path1 (first (children "E" graph)) n2 graph (cons n1 directions))) 
                     ([not (not (member (first (children n1 graph)) directions))] (get-path1 (first (children n1 graph)) n2 graph (cons n1 directions)))
                     (else (get-path1 (first (children n1 graph)) n2 graph (cons n1 directions)))
                     )
                    )
  )

(define get-path (lambda (n1 n2)
                   (cond
                     ([equal? n1 n2] "You are at your destination")
                     ([equal? #t (not (not (member n2 (children n1 a-graph))))] (edge n1 n2 a-graph))
                     (else
                      (get-path1 n1 n2 a-graph '()))
                     )
                   )
  )
                       
                       
;Train stations
;Northern line
(define train_lines(send northern_line train_stations_get))
;Victoria line
(define train_lines3(list "Brixton" "Stockwell"  "Vauxhall"  "Pimlico"  "Victoria"    "Green Park"
                          "Oxford Circus" "Warren Street"  "Euston"  "King's Cross St. Pancras"
                          "Highbury & Islington"  "Finsbury Park"  "Seven Sisters"  "Tottenham Hale"
                          "Blackhorse Road"  "Walthamstow Central" ))
;List of TFL trainlines
(define train_lines2(list (send northern_line line_name_get) "Bakerloo Line" "Central Line" "Circle Line" "District Line"
                          "Hammersmith & City Line" "Jubilee Line" "Metropolitan Line Tube" "Northern Line"
                          "Piccadilly Line" "Victoria Line" "Waterloo & City Line"))
;S1
(define font_app(make-font #:size 12 #:family 'decorative #:weight 'normal))

(define journey(list "Hendon Central" "Brent Cross"))

 ;(send my-box get-string (send my-box get-selection))
; (for/list ([i '("Morden" "Brixton")]) (cond ((empty? (filter (λ (x) (equal? x i)) train_lines3)) "No") (else "Yes")))
(define choiceschoices(list "Field" "field" "field"))

(define frame (new frame% [label "Easy Access TFL"]
                   [alignment '(left top)]
                   [stretchable-width #f]	 
                   [stretchable-height #f]
                   [min-width 400]	 
                   [min-height 600]))

(define frame2 (new frame% [label "Easy Access TFL"]
                   [alignment '(left top)]
                   [min-width 400]	 
                   [min-height 600]))

(define frame3 (new frame% [label "Error"]
                   [alignment '(center top)]
                   [min-width 200]	 
                   [min-height 100]))


(new canvas% [parent frame]
     [min-height 30]
     [min-width 100]
     	[stretchable-width #t]	 
   	 	[stretchable-height #f]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-background (make-object color% 0 25 168))
            (send dc clear)  
                (send dc set-scale 1 1)
                (send dc set-pen "white" 1 'transparent)
                (send dc set-brush "0, 25, 168" 'solid)
                (send dc set-font (make-font #:size 16 #:family 'decorative #:weight 'bold))                
                (send dc set-text-foreground (make-object color% 255 255 255))
                (send dc draw-text "Easy Access TFL" 0 0))]
             [vert-margin 0]	 
   	 	[horiz-margin 0])
;S2
(new canvas% [parent frame2]
     [min-height 30]
     [min-width 100]
     	[stretchable-width #t]	 
   	 	[stretchable-height #f]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-background (make-object color% 0 25 168))
            (send dc clear)
                
                (send dc set-scale 1 1)
                (send dc set-pen "white" 1 'transparent)
                (send dc set-brush "0, 25, 168" 'solid)
                (send dc set-font (make-font #:size 16 #:family 'decorative #:weight 'bold))
                (send dc set-text-foreground (make-object color% 255 255 255))
                (send dc draw-text "Easy Access TFL" 0 0))])

(new canvas% [parent frame2]
     [min-height 30]
     [min-width 100]
     	[stretchable-width #t]	 
   	 	[stretchable-height #f]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-background (make-object color% 193 200 210))
            (send dc clear)   
                (send dc set-scale 1 1)
                (send dc set-pen "white" 1 'transparent)
                (send dc set-brush "0, 25, 168" 'solid)
                (send dc set-font (make-font #:size 14 #:family 'decorative #:weight 'bold))                
                (send dc set-text-foreground (make-object color% 255 255 255))
                (send dc draw-text "Here's your journey info:" 0 0))]
             [vert-margin 0]	 
   	 	[horiz-margin 0])

(new canvas% [parent frame]
     [min-height 30]
     [min-width 100]
     [stretchable-width #t]	 
     [stretchable-height #f]
     [paint-callback
              (lambda (canvas dc)
                (send dc set-background (make-object color% 193 200 210))
            (send dc clear)
                
                (send dc set-scale 1 1)
                (send dc set-pen "white" 1 'transparent)
                (send dc set-brush "0, 25, 168" 'solid)
                (send dc set-font (make-font #:size 14 #:family 'decorative #:weight 'bold))
                ;(send dc draw-rectangle 0 0 100 30)
                
                (send dc set-text-foreground (make-object color% 255 255 255))
                (send dc draw-text "Enter journey info:" 0 0))]
             [vert-margin 0]	 
   	 	[horiz-margin 0])

(new canvas% [parent frame3]
     [min-height 30]
     [min-width 100]
     	[stretchable-width #t]	 
   	 	[stretchable-height #f]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-background "Red")
            (send dc clear)
                
                (send dc set-scale 1 1)
                (send dc set-pen "white" 1 'transparent)
                (send dc set-brush "0, 25, 168" 'solid)
                (send dc set-font (make-font #:size 14 #:family 'decorative #:weight 'bold))
                ;(send dc draw-rectangle 0 0 100 30)
                
                (send dc set-text-foreground (make-object color% 255 255 255))
                (send dc draw-text "ERROR" 0 0))]
             [vert-margin 0]	 
   	 	[horiz-margin 0])
;S3
(define panel1(new panel%
                   [parent frame]
                   [alignment '(center bottom)]
                   [min-height 300]
                   	[style (list 'border )]
                   ))
(define panel2(new panel%
                   [parent frame]
                   [alignment '(center top)]
                   	[style (list 'border )]
                   ))
(define panel3(new panel%
                   [parent frame2]
                   [alignment '(left bottom)]
                   [min-height 300]
                   	[style (list 'border )]
                   ))
(define panel4(new horizontal-panel%
                   [parent frame2]
                   [alignment '(center bottom)]
                   	[style (list 'border )]
                   ))


;S6                       
(define starting_location
  (new choice% [parent panel1]
       [vert-margin 40]
       [label "Starting Location:"]
       [choices train_lines]
       [font font_app]))

(define line
  (new choice% [parent panel1]
       [vert-margin 10]
       [label  "                          "]
       [choices train_lines2]
       [font font_app]
       [callback (λ (c e) (cond ((equal? (send line get-string (send line get-selection)) "Northern Line") (send starting_location clear)(for ([i train_lines]) (send starting_location append i)))
                            ((equal? (send line get-string (send line get-selection)) "Victoria Line") (send starting_location clear)(for ([i train_lines3]) (send starting_location append i)))
                                
                                ))]))

(define destination
  (new choice% [parent panel2]
       [vert-margin 5]
       [label "Destination:        "]
       [choices train_lines]
       [font font_app]))
(define line2
  (new choice% [parent panel2]
       [vert-margin 40]
       [label  "                          "]
       [choices train_lines2]
       [font font_app]
       [callback (λ (c e) (cond ((equal? (send line2 get-string (send line2 get-selection)) "Northern Line") (send destination clear)(for ([i train_lines]) (send destination append i)))
                            ((equal? (send line2 get-string (send line2 get-selection)) "Victoria Line") (send destination clear)(for ([i train_lines3]) (send destination append i)))))]))

(send starting_location set-selection (random 10))
(send destination set-selection (random 10))
;S4
;This is the section defining the buttons an their related functions in the gui
; e.g. button when clicked it sends the items selected in the choice boxs and sends it to the list box in the secondd frame
(define button (new button%
                    (parent panel2)
                    (label "Go")
                    (vert-margin 80)
                    	(min-width 50)
                        [font font_app]
   	 	(min-height 43)
                (callback (lambda (button event)
                        (cond ((equal? (send starting_location get-string (send starting_location get-selection)) (send destination get-string (send destination get-selection))) (message-box "Error" "Starting location and destination are the same." frame '(stop ok)))
                         (else (send list-box set (get-path1 (send starting_location get-string (send starting_location get-selection)) (send destination get-string (send destination get-selection)) (send northern_line train_connections_get) '())
                               (for/list ( [i (get-path1 (send starting_location get-string (send starting_location get-selection)) (send destination get-string (send destination get-selection)) (send northern_line train_connections_get) '())]) (string-join (for/list ([i (filter (λ (x) (list? (assoc i (send x train_station)))) lines)])  (send i line_name_get))))
                               (for/list ([i (for/list ([j (get-path1 (send starting_location get-string (send starting_location get-selection)) (send destination get-string (send destination get-selection)) (send northern_line train_connections_get) '())])(for/or ([i (for/list ([i lines]) (send i step_free j))]) i))]) (cond ((equal? i #t) "Yes") (else "No")))
                               )(send frame2 show #t) (send frame show #f))
                          )))))
(define message (new message%
                     (parent panel2)
                     (vert-margin 150)
                     (label "")                   
                     (stretchable-width #f)	 
   	 (stretchable-height #f)
                	(auto-resize #f)))
(define button2 (new button%
                    (parent panel4)
                    (label "Go back")
                    (font font_app)
                    (min-width 50)	 
                    (min-height 43)
                    (callback (lambda (button event) (send frame show #t) (send frame2 show #f)))))

;s5
;This is the list box where data about the journey is sent too
(define list-box (new list-box%
                      (label "")
                      (parent panel3)
                      (choices '()  )
                      (font font_app)
                      (style (list 'single 'variable-columns 'column-headers ))
                      (columns (list "Train station" "Train line" "Step free"))))

(send list-box set-column-width	 0 140 100 300)
(send list-box set-column-width	 1 175 100 300)
(send list-box set-column-width	 2 125 100 300)
(send frame show #t)
