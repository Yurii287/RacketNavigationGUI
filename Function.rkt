#lang racket/gui
(require racket/gui/base)
(require racket/trace)

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
                   ([equal? n "E"] branch-nodes)
                   ([not (equal? n "E")] (branch (first (children n graph)) graph (cons n branch-nodes)))
                   )
                 )
  )

(define get-path1 (lambda (n1 n2 graph directions)
                   (cond
                     ([and (not (not (member n1 directions))) (not (not (member n2 directions)))] (remove-duplicates (reverse directions)))
                     ([not (not (member n2 (children n1 graph)))] (get-path1 n1 n2 graph (cons n2 (cons n1 directions))))
                     ([and (equal? n1 "E") (not (not (member n2 (branch "A" graph '()))))] (get-path1 (second (children "E" graph)) n2 graph (cons n1 directions)))
                     ([and (equal? n1 "E") (not (not (member n2 (branch "B" graph '()))))] (get-path1 (third (children "E" graph)) n2 graph (cons n1 directions))) 
                     ([and (equal? n1 "E") (not (not (member n2 (branch "G" graph '()))))] (get-path1 (first (children "E" graph)) n2 graph (cons n1 directions))) 
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
                       
(trace get-path1)
