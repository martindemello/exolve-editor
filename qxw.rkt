#lang racket

(require "xword.rkt")

(provide (prefix-out qxw: parse-file))

(define s string->number)

(define (parse-list-v3 lines)
  (let [(xw #f)]
    (for [(line lines)]
      (match (string-split line)
        [(list "GP" "0" w h _ _ _) (set! xw (make-xword (s w) (s h)))]
        [(list "SQ" col row _ _ "1") (set-square xw (s col) (s row) ".")]
        [(list "SQ" col row _ _ "0") (set-square xw (s col) (s row) "0")]
        [(list "SQ" col row _ _ "0" c) (set-square xw (s col) (s row) c)]
        [_ #f]))
    xw))


(define (parse-list-v5 lines)
  (let [(xw #f)]
    (for [(line lines)]
      (match (string-split line)
        [(list "GP" "0" w h _ _ _) (set! xw (make-xword (s w) (s h)))]
        [(list "SQCT" col row "0" c)
         (let [(col (s col))
               (row (s row))
               (c (string-trim c "\""))]
           (match c
             ["." (set-square xw col row ".")]
             ["" (set-square xw col row "0")]
             [c (set-square xw col row c)]))]
        [_ #f]))
    xw))

(define (parse-file f)
  (let [(lines (file->lines f))]
    (match (first (string-split (first lines)))
      ["#QXW2v3" (parse-list-v3 lines)]
      ["#QXW2v5" (parse-list-v5 lines)])))
