#lang racket

(provide make-xword
	 set-square
	 renumber
	 word-ac+
	 word-dn+
	 (struct-out xword))

(struct xword (rows cols grid nums))

(define (make-grid rows cols)
  (build-vector rows (lambda x (make-vector cols))))

(define (set-grid grid i j v)
  (vector-set! (vector-ref grid j) i v))

(define (make-xword rows cols)
  (xword rows cols (make-grid rows cols) (make-grid rows cols))) 

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
