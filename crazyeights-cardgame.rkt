;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname crazyeights-cardgame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct card (suit value))
;; A Card is a (make-card Sym Nat)
;; requires: suit is one of ’hearts, ’diamonds, ’clubs, or ’spades
;; value between 1 and 13, using 11 for Jack, 12 for Queen, and 13 for King.
(define card1 (make-card 'diamonds 8))
(define card2 (make-card 'spades 2))
(define card3 (make-card 'hearts 3))
(define card4 (make-card 'hearts 2))
(define card5 (make-card 'clubs 11))
;; A wild-card is a card with value 8
(define wild-points 50)
;; A face card is a Jack(represented by value 10), Queen (represented by value 11) or King (represented by value 13)
(define face-card-points 10)
(define crazy-value 8)
;;****************************************************
;; (crazy-count list center) consumes a list of cards and the center card, and produces the number
;;  of cards that can be played.
;; crazy-count: (listof Card) Card -> Num
;; Examples:
(check-expect (crazy-count (cons card1 (cons card2 (cons card3 empty)))card4)3)

(define (crazy-count list center)
  (length (filter
           (lambda (card)
             (or (= crazy-value (card-value card)) (= (card-value card) (card-value center))
                 (symbol=? (card-suit card) (card-suit center))))list)))

;; Tests:
(check-expect (crazy-count (cons card1 (cons card2 (cons card3 empty)))card5)1)
(check-expect (crazy-count (cons (make-card 'spades 8) empty) (make-card 'diamonds 2))1)
;; ***************************************************
;; (crazy-dumb list center) consumes a list of cards and the center card
;;   and produces the first playable card in the list consumed, if exists. Else it produces false.
;; crazy-dumb: (listof Card) Card -> (anyof false Card)
(define (crazy-dumb list center)
  (local [(define lo-playable-card
            (filter
             (lambda (card)
               (or (= crazy-value (card-value card)) (= (card-value card) (card-value center))
                   (symbol=? (card-suit card) (card-suit center)))) list))]
    (cond
      [(cons? lo-playable-card) (first lo-playable-card)]
      [else false])))
;; Tests:
(check-expect (crazy-dumb empty card3 )false)
(check-expect (crazy-dumb (list card2 card3) card5)false)
(check-expect(crazy-dumb (cons (make-card 'spades 8) (cons (make-card 'hearts 3) empty))
                         (make-card 'hearts 1))(make-card 'spades 8))
;; ***************************************************
;; (smart-card? list center) produces true if there's a card in the list that matches
;;   the center card in suit or value, else produces false 
;; smart-card?: (listof Card) Card -> Bool
;; Examples: 
(check-expect (smart-card? (cons (make-card 'hearts 4) empty) (make-card 'spades 4))true)

(define (smart-card? list center)
(cons? (filter (lambda (c) (or (= (card-value c) (card-value center))
                          (symbol=? (card-suit c) (card-suit center)))) list)))
;; Tests:
(check-expect (smart-card? empty card5) false)
(check-expect (smart-card? (cons card1 empty) card5) false)
;; ***************************************************
;; (crazy-smart list center) produces the first card that matches the center in
;;    value or suit that isn't an 8. If no such card exists, it produces an 8.
;;     If no playable card exists, it produces false.
;; crazy-smart: (listof Card) Card -> (anyof Card false)
;; Examples: 
(check-expect (crazy-smart (list card1 card2 card3) card4 )card2)

(define (crazy-smart list center)
  (local
    [(define not-eights
       (filter
        (lambda (card)
          (or (= (card-value card) (card-value center))
              (symbol=? (card-suit card) (card-suit center)))) list))
     (define eights
       (filter (lambda (card) (= crazy-value (card-value card)))  list))]
    (cond
      [(cons? not-eights) (first not-eights)]
      [(cons? eights) (first eights)]
      [else false])))
;; Tests:
(check-expect (crazy-smart (cons card4 (cons card3 (cons card2 (cons card1 empty))))card4)card4)
(check-expect (crazy-smart (cons card2 empty) card5)false)
(check-expect (crazy-smart empty card4) false)
(check-expect (crazy-smart (cons card2 (cons card1 (cons card3 (cons card4 empty)))) card5) card1)
(check-expect (crazy-smart empty (make-card 'hearts 3))false)
;; ***************************************************
;; (crazy-points list) consumes a list of Card structures and produces the number
;;  of points it would be worth if another played had those cards at the end of
;;    a game.
;; A wild-card is worth 50 points (wild-points)
;; A face card is worth 10 points (face-card-points)
;; Any other card is worth its value 
;; crazy-points: (listof Card) -> Num
;; Examples: 
(check-expect (crazy-points (cons card1 (cons card2 empty)))52)

(define (crazy-points listofcards)
  (local
    [(define list-of-values
       (map
        (lambda (card)
          (cond
            [(> (card-value card) face-card-points)  face-card-points]
            [(= (card-value card) crazy-value) wild-points]
            [ else (card-value card)]))
        listofcards))]
    (foldr + 0 list-of-values)))

;; Tests:
(check-expect (crazy-points (cons card5 (cons card3 (cons (make-card 'diamonds 1) empty))))14)
(check-expect (crazy-points (cons card5 (cons card3 (cons (make-card 'diamonds 12) empty))))23)
(check-expect (crazy-points (cons card5 (cons card3 (cons (make-card 'diamonds 13) empty))))23)
(check-expect (crazy-points empty) 0)