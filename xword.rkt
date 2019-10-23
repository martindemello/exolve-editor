#lang racket

(provide make-xword
         set-square
         renumber
         format-cells
         word-ac+
         word-dn+
         extract-words
         letter
         number
         (struct-out xword))

(struct xword (rows cols grid nums data))

(define (make-grid rows cols)
  (build-vector rows (lambda x (make-vector cols #f))))

(define (set-grid grid i j v)
  (vector-set! (vector-ref grid j) i v))

(define (make-xword rows cols)
  (let* [(grid (make-grid rows cols))
         (nums (make-grid rows cols))
         (data (make-hash))]      
    (xword rows cols grid nums data)))
          
  (define (-- x) (- x 1))
  (define (++ x) (+ x 1))

  (define (max-row xw) (-- (xword-rows xw)))
  (define (max-col xw) (-- (xword-cols xw)))

  (define (in-xw? xw i j)
    (and (>= i 0) (<= i (max-row xw))
         (>= j 0) (<= j (max-col xw))))

  ; accessors
  (define (letter xw i j) (vector-ref (vector-ref (xword-grid xw) j) i))
  (define (number xw i j) (vector-ref (vector-ref (xword-nums xw) j) i))

  ; define anything outside the grid boundaries as black
  (define (black? xw i j) (or (not (in-xw? xw i j))
                              (eq? (letter xw i j) ".")))
  (define (white? xw i j) (not (black? xw i j)))
  (define (blank? xw i j) (eq? (letter xw i j) "0"))


  (define (set-number xw i j n)
    (set-grid (xword-nums xw) i j n))

  (define (set-square xw i j c)
    (set-grid (xword-grid xw) i j c))

  ; numbering
  (define (start-across? xw i j)
    (and
     (white? xw i j)
     (black? xw (-- i) j)
     (white? xw (++ i) j)))

  (define (start-down? xw i j)
    (and
     (white? xw i j)
     (black? xw i (-- j))
     (white? xw i (++ j))))

  (define (start-sqr? xw i j)
    (or (start-across? xw i j) (start-down? xw i j)))

  (define (renumber xw #:on-ac [on-ac #f] #:on-dn [on-dn #f])
    (let* [(n 0)
           (inc-n (thunk (set! n (++ n))))]
      (for* [(j (xword-rows xw))
             (i (xword-cols xw))]
        (if (start-sqr? xw i j) 
            (begin (inc-n)
                   (set-number xw i j n)
                   (cond [(and on-ac (start-across? xw i j)) (on-ac i j n)])
                   (cond [(and on-dn (start-down? xw i j)) (on-dn i j n)]))
            (set-number xw i j #f)))))

  ; words
  (define (word-ac+ xw i j)
    (for/list [(k (stop-before (in-range i (xword-cols xw)) 
                               (λ (k) (black? xw k j))))]
      (letter xw k j)))

  (define (word-dn+ xw i j)
    (for/list [(k (stop-before (in-range j (xword-rows xw)) 
                               (λ (k) (black? xw i k))))]
      (letter xw i k)))

  (define (format-light cells n)
    (let [(word (string-join cells ""))
          (num (number->string n))
          (enum (number->string (length cells)))]
      (string-append num ". " word " (" enum ")")))

  (define (extract-words xw)
    (let [(ac '())
          (dn '())
          (fmt (λ (fn i j n) (format-light (fn xw i j) n)))]
      (renumber xw
                #:on-ac (λ (i j n) (set! ac (cons (fmt word-ac+ i j n) ac)))
                #:on-dn (λ (i j n) (set! dn (cons (fmt word-dn+ i j n) dn))))
      (values (reverse ac) (reverse dn))))

  ; format
  (define (format-cells xw cell->str)
    (for/vector [(j (xword-rows xw))]
      (for/vector [(i (xword-cols xw))]
        (cell->str (letter xw i j)
                   (number xw i j)))))