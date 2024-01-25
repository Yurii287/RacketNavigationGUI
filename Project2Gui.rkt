#lang racket/gui
(require racket/gui/base)
(require graph)

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

(define grand-children (lambda (n graph)
                         (flatten (map (lambda (node) (children node graph)) (children n graph)))))

(define weight (lambda (n1 n2 graph)
                 (first (reverse (edge n1 n2 graph)))))

;get-path
; - check what n1 connects to, is that equal to n2
; - if not, append edge1 to a list, and check what n2 connects to, is that equal to n2?
;     - ignore the n2 edge that connects back to n1
; - call recursivly with the new n1 and n2 being the new edge that you are checking
(define get-path (lambda (n1 n2 graph)
                   (cond
                     ([equal? n2 (second (edge n1 n2 graph))] #t)
                     )
                   )
  )

