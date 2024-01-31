#lang racket/gui
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
                   [alignment '(left bottom)]
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

(define combo-field (new combo-field%
                         (label "Combo")
                         (parent panel1)
                         
                         (choices choiceschoices)
                         (init-value "Field")))
(define combo-field2 (new combo-field%
                         (label "Combo")
                         (parent panel2)
                         


                         
                         (choices (list "Field" "field" "field"))
                         (init-value "Field")))
(define button (new button%
                    (parent panel2)
                    (label "Button")
                    (vert-margin 50)
                    	(min-width 50)	 
   	 	(min-height 50)
                (callback (lambda (button event)
                        (send frame show #f) (send frame2 show #t)))))
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
(define something(list "hello"))
(define list-box (new list-box%
                      (label "")
                      (parent panel3)
                      (choices something)
                      (style (list 'single
                                   'variable-columns 'column-headers))
                      (columns (list "First Column"))))
(send list-box append-column "Hello")
;(send list-box append "choices" [0])
(send frame show #t)

(define First_letter(λ (x) (string-ref x 0)))

(define First_letter2(λ (x) (for/list ([i x]) (string-ref i 0))))

;(define First_Letter3(λ (x y) (for/list ([i x]) (cond ((equal? (string-ref i 0) y) i) (else ) ))))
(filter (λ (x) (equal? (string-ref x 0) #\h )) '("hello"))