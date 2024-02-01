#lang racket/gui
(require racket/gui/base)

(define a-graph (list
                 (list "A" "C" 50)
                 (list "B" "D" 40)
                 (list "C" "E" 20) (list "C" "A" 50)
                 (list "D" "E" 35) (list "D" "B" 40)
                 (list "E" "F" 100) (list "E" "C" 20) (list "E" "D" 35)
                 (list "F" "E" 100)
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

;grand-children + (remove n) = grandchildren without source node
(define grand-children (lambda (n graph)
                         (remove n (flatten (map (lambda (node) (children node graph)) (children n graph))))))

(define weight (lambda (n1 n2 graph)
                 (first (reverse (edge n1 n2 graph)))))

(define get-path1 (lambda (n1 n2 graph directions)
                   (cond
                     ([and (not (not (member n1 directions))) (not (not (member n2 directions)))] (reverse directions))
                     ([equal? #t (not (not (member n2 (children n1 graph))))] (get-path1 n1 n2 graph (cons n2 (cons n1 directions))))
                     ([equal? #t (not (not (member n1 directions)))] (get-path1 (first (reverse (children n1 graph))) n2 graph (cons n1 directions)))
                     ([equal? #t (not (not (member n1 directions)))] (get-path1 (first (rest (children n1 graph))) n2 graph (cons n1 directions)))
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
                       



(define train_lines(list "Morden"
"South Wimbledon"
"Colliers Wood"
"Tooting Broadway"
"Tooting Bec"
"Balham"
"Clapham South"
"Clapham Common"
"Clapham North"
"Stockwell"
"Oval"
"Kennington"
"Nine Elms"
"Battersea Power Station"
"Waterloo"
"Embankment"
"Charing Cross"
"Leicester Square"
"Tottenham Court Road"
"Goodge Street"
"Warren Street"
"Euston"
"Mornington Crescent"
"Camden Town"
"Chalk Farm"
"Belsize Park"
"Hampstead"
"Golders Green"
"Brent Cross"
"Hendon Central"
"Colindale"
"Burnt Oak"
"Edgware"))

(define journey(list "Hendon Central" "Brent Cross"))
 ;(send my-box get-string (send my-box get-selection))
(define choiceschoices(list "Field" "field" "field"))
(define frame (new frame% [label "Example"]
                   [alignment '(left top)]
                   	[stretchable-width #f]	 
   	 	[stretchable-height #f]
                   [min-width 400]	 
   	 	[min-height 600]))
(define frame2 (new frame% [label "Example"]
                   [alignment '(left top)]
                   
                   [min-width 400]	 
   	 	[min-height 600]))

(define frame3 (new frame% [label "Error"]
                   [alignment '(left top)]
                   
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
                (send dc set-font (make-font #:size 16 #:family 'decorative
                             #:weight 'bold))
                ;(send dc draw-rectangle 0 0 100 30)
                
                (send dc set-text-foreground (make-object color% 255 255 255))
                (send dc draw-text "Transport app" 0 0))]
             [vert-margin 0]	 
   	 	[horiz-margin 0])

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
                (send dc set-font (make-font #:size 16 #:family 'decorative
                             #:weight 'bold))
                ;(send dc draw-rectangle 0 0 100 30)
                
                (send dc set-text-foreground (make-object color% 255 255 255))
                (send dc draw-text "Transport app" 0 0))]
             [vert-margin 0]	 
   	 	[horiz-margin 0])

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
                   	[style (list 'border )]
                   ))
(define panel4(new panel%
                   [parent frame2]
                   [alignment '(center top)]
                   	[style (list 'border )]
                   ))


                       
(define starting_location
  (new choice% [parent panel1]
       
       [label "Some choices"]
       [choices train_lines]))
(define destination
  (new choice% [parent panel2]
       
       [label "Some choices"]
       [choices train_lines]))

(define button (new button%
                    (parent panel2)
                    (label "Button")
                    (vert-margin 50)
                    	(min-width 50)	 
   	 	(min-height 50)
                (callback (lambda (button event)
                        (cond
                         
                          )))))
(define message (new message%
                     (parent panel2)
                     (vert-margin 100)
                     (label "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Aenean lobortis nibh vel tellus efficitur pretium.
Aenean semper tristique ipsum, nec. ")
                     
                     [stretchable-width #f]	 
   	 	[stretchable-height #f]
                	[auto-resize #f]))
(define button2 (new button%
                    (parent panel4)
                    (label "Button")
                    (vert-margin 100)
                    	[min-width 50]	 
   	 	(min-height 50)
                (callback (lambda (button event)
                        (send frame show #t) (send frame2 show #f)))))
(define button3 (new button%
                    (parent panel4)
                    (label "filter")
                    (vert-margin 50)
                    	[min-width 50]	 
   	 	(min-height 50)
                (callback (lambda (button event)
                        (send frame show #t) (send frame2 show #f)))))
(define something(list "hello" "world"))
(define list-box (new list-box%
                      (label "")
                      (parent panel3)
                      (choices something  )
                      (style (list 'single
                                   'variable-columns 'column-headers))
                      (columns (list "Train stations"))))

;(send list-box append "choices" [0])
(send frame show #t)

(define First_letter(λ (x) (string-ref x 0)))

(define First_letter2(λ (x) (for/list ([i x]) (string-ref i 0))))

;(define First_Letter3(λ (x y) (for/list ([i x]) (cond ((equal? (string-ref i 0) y) i) (else ) ))))
(filter (λ (x) (equal? (string-ref x 0) #\h )) '("hello"))
