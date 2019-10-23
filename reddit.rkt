#lang racket

(require "xword.rkt")

(provide (prefix-out reddit: format-xw))

(define (format-num n)
  (if n (format "^~a" n) ""))

(define (format-filled c)
  (match c
    ["." "*.*"]
    ["0" " "]
    [c c]))

(define (format-blank c)
  (match c
    ["." "*.*"]
    [c " "]))

(define (format-cell c n)
  (let [(char (format-filled c))
        (num (format-num n))]
    (string-append char num)))

(define (row->str row)
  (string-append
   "|" (string-join (vector->list row) "|") "|"))

(define (format-grid xword)
  (let* [(width (xword-cols xword))
         (cells (format-cells xword format-cell))
         (grid-lines (vector-map row->str cells))
         (sep (row->str (make-vector width "--")))]
    (string-append
     (vector-ref grid-lines 0) "\n"
     sep "\n"
     (string-join (vector->list (vector-drop grid-lines 1)) "\n"))))

(define (format-clue clue)
  string-append "  "
  (regexp-replace #px"^(\\s*)(\\d+)\\." clue "\\1\\2\\$\\\\."))
  
(define (format-clues xword key)
  (let [(clues (hash-ref (xword-data xword) key))]
    (string-join (map format-clue clues) "\n")))   

(define (format-xw xword)
  (renumber xword)
  (let [(grid (format-grid xword))
        (ac (format-clues xword 'clues-across))
        (dn (format-clues xword 'clues-down))]
    (string-append grid "\n\n"
                   "**Across**" "\n\n"
                   ac "\n\n"
                   "**Down**" "\n\n"
                   dn "\n")))