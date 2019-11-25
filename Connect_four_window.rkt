#lang slideshow
(require racket/class
         racket/gui/base)
(require (prefix-in 2dtp: 2htdp/image ) )
(provide (all-defined-out))
(define (ch list1 number1) (cond [(not (= 0 (- number1 1) ))(append (ch list1 (- number1 1)) list1)]
                                 [(= 0 (- number1 1) )  (for/list ([i (in-range 20 150 20)]) i)]))
;(ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;(sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <)
;(sort (ch  (for/list ([i (in-range 20 150 20)]) i) 6) <)

;(define (ch2 list1 list2 number1) (cond [(not (= 0 (- number1 1) ))(append (ch2 (cdr list1) list2 (- number1 1)) (car list1))]
;                                [(= 0 (- number1 1) )(list 1 2)]))
(define (maker-helper list1 list2 vector1)
  (maker list1 list2 (vector->list vector1)) )
(define (maker list1 list2 list3)  (cond
                                     [(empty? list1) (2dtp:rectangle 200 200 "solid" "blue")]
                                     ; [(vector? list3)(let () (vector->list list3)((maker list1 list2 list3)))]

                                     [(and (not (empty? list1)) (= (car list3) 0)) (pin-over (maker  (cdr list1) (cdr list2) (cdr list3)) (car list1) (car list2)  (2dtp:circle 5 "solid" "white")) ]

                                     [(and (not (empty? list1)) (= (car list3) 1)) (pin-over (maker  (cdr list1) (cdr list2) (cdr list3)) (car list1) (car list2)  (2dtp:circle 5 "solid" "red"))]
                                     [(and (not (empty? list1)) (= (car list3) 2)) (pin-over (maker  (cdr list1) (cdr list2) (cdr list3)) (car list1) (car list2)  (2dtp:circle 5 "solid" "green"))]))
;(maker  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;        (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (list 0 1 2 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 ))


(define f (new frame% [label "My Art"]
               [width 200]
               [height 200]
               [alignment '(center center)]))
 
(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
         [style '(border)]
         [paint-callback (lambda (self dc)
                           (drawer dc 0 0))])))
;(pin-over   (2dtp:rectangle 200 200 "solid" "black")
;          0 0
;            (2dtp:circle 10 "solid" "white"))
;
;
;(add-drawing (maker-helper  (ch  (for/list ([i (in-range 20 150 20)]) i) 6)
;                            (sort (ch  (for/list ([i (in-range 20 130 20)]) i) 7) <) (vector 0 1 2 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 )))
;(send f show #t)