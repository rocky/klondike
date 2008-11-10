; AisleRiot - klondike.scm
; Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
; 
; Changes to make this game less lame: rocky@gnu.org
;
; This game is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
; USA

(define deal-one #t)    ;deal one card at a time from stock to waste
(define deal-three #f)  ;deal three cards at a time from stock to waste
(define no-redeal #f)   ;stock deals a card face-up to each pile in the tableau instead of to waste
(define kings-only #t)  ;only allow kings to be moved to empty slots

(define max-redeal 2)   ;number of redeals, -1 for unlimited

; The set up:

(define tableau '(6 7 8 9 10 11 12))
(define foundation '(2 3 4 5))
(define stock 0)
(define waste 1)
(define selected-card "unint")

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot DECK)

  (if deal-three
      (add-partially-extended-slot '() right 3)
      (add-normal-slot '()))

  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-tableau tableau)
  
  (map flip-top-card tableau)
  (for-each plop-if-ace-or-deuce '(6 7 8 9 10 11 12))

  (give-status-message)

  (list 7 3.1)
)

(define (deal-tableau tableau)
  (if (not (null? tableau))
      (begin
        (deal-cards stock tableau)
        (deal-tableau (cdr tableau)))))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string)
					"  -- " selected-card)))

(define (get-redeals-string)
  (if (< max-redeal 0) ""
      (string-append (_"Redeals left:") " "
		     (number->string (- max-redeal FLIP-COUNTER)))))

(define (get-stock-no-string)
  (string-append (_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (set! selected-card (get-name (car (reverse (get-cards slot-id)))))
  (and (or (> slot-id 1)
	   (and (= slot-id 1)
		(= (length card-list) 1)))
       (is-visible? (car (reverse card-list)))))

; Check if slot is an ace or a deuce; if so move it to the foundation and 
; check the next exposed card...
(define (plop-if-ace-or-deuce slot)
  (if (or (is-ace? slot) (is-deuce? slot))
	  (button-double-clicked slot)))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (member start-slot foundation)
      (add-to-score! -1))
  (if (member end-slot foundation)
      (add-to-score! 1))
  (if (and (not (empty-slot? start-slot)) 
	   (member start-slot tableau))
      (make-visible-top-card start-slot))
  ; See if we can move an exposed card to the foundation
  ; However don't try if the pile is empty or we just tried
  ; to move *down* from the foundation.
  (if (not (or (empty-slot? start-slot)
	       (member start-slot foundation)))
	   (plop-if-ace-or-deuce start-slot))
  #t)

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (complete-transaction start-slot card-list end-slot) 
  #f))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (member end-slot tableau)
		(if (empty-slot? end-slot)
		    (or (not kings-only)
		        (= king (get-value (car (reverse card-list)))))
		    (and (not (eq? (is-red? (get-top-card end-slot))
				   (is-red? (car (reverse card-list)))))
			 (= (get-value (get-top-card end-slot))
			    (+ (get-value (car (reverse card-list))) 1)))))
	   (and (member end-slot foundation)
		(= 1 (length card-list))
		(if (empty-slot? end-slot)
		    (= ace (get-value (car card-list)))
		    (and (= (get-suit (get-top-card end-slot))
			    (get-suit (car card-list)))
			 (= (get-value (get-top-card end-slot)) 
			    (- (get-value (car card-list)) 1))))))))

(define (button-clicked start-slot)
  (if (= start-slot stock)
      (begin
	(flip-stock stock waste max-redeal 
                               (if deal-three 3 1))
	(button-double-clicked waste))))

; If a the top card in start-slot can be moved to the foundation, do
; it.
(define (button-double-clicked start-slot)
  (or (and (member start-slot foundation)
	   (autoplay-foundations))
      (and (member start-slot (cons waste tableau))
	   (not (empty-slot? start-slot))
	   (let* ((target-card
		   (cond ((= (get-value(get-top-card start-slot)) ace) '())
			 (#t (add-to-value (get-top-card start-slot) -1))))
		  (end-slot (search-foundation target-card foundation)))
	     (and end-slot
		  (complete-transaction start-slot 
					(list (remove-card start-slot)) 
					end-slot))))))

(define (search-foundation card foundations)
  (or-map (lambda (slot) (if (equal? card (get-top-card slot))
			     slot
			     #f)) foundations))

(define (autoplay-foundations)
  (define (autoplay-foundations-tail)
    (if (or-map button-double-clicked (cons waste tableau))
        (delayed-call autoplay-foundations-tail)
        #t))
  (if (or-map button-double-clicked (cons waste tableau))
      (autoplay-foundations-tail)
      #f))

; Global variables used in searching (keeping it simple):

(define card #f)
(define color 0)
(define suit 0)
(define value 0)
(define slot-id1 0)

(define (match? slot-id2)
  (and (not (empty-slot? slot-id2))
       (= suit (get-suit (get-top-card slot-id2)))
       (= value (get-value (get-top-card slot-id2)))
       (list 1 (get-name (get-top-card slot-id2)) (get-name card))))

(define (ploppable? slot-id)
  (and (not (empty-slot? slot-id))
       (set! card (get-top-card slot-id))
       (set! suit (get-suit card))
       (set! value (+ (get-value card) 1))
       (or-map match? (cons waste tableau))))

(define (is-ace? slot-id)
  (and (not (empty-slot? slot-id))
       (= ace (get-value (get-top-card slot-id)))
       (list 2 (get-name (get-top-card slot-id)) (_"an empty slot" ))))

(define (is-deuce? slot-id)
  (and (not (empty-slot? slot-id))
       (= 2 (get-value (get-top-card slot-id)))
       (list 2 (get-name (get-top-card slot-id)) (_"an empty slot" ))))

(define (shiftable? slot-id2)
  (and (not (= slot-id2 slot-id1))
       (if (empty-slot? slot-id2)
	   (and (= value king)
		(list 2 (get-name card) (_"an empty slot")))
	   (and (= (get-value (get-top-card slot-id2)) (+ 1 value))
		(not (= (get-color (get-top-card slot-id2)) color))
		(list 1 (get-name card) (get-name (get-top-card slot-id2)))))))

(define (check-visible card)
  (and (is-visible? card) card))

(define (shiftable-iter slot-id)
  (and (not (empty-slot? slot-id))
       (let ((card-list (reverse (get-cards slot-id))))
	 (set! card (or-map check-visible card-list))
	 (set! color (get-color card))	
	 (set! value (get-value card))
	 (set! slot-id1 slot-id)
	 (and (not (and (= value king)
			(eq? card (car card-list))))
	      (or-map shiftable? tableau)))))

(define (addable? slot-id)
  (if (empty-slot? slot-id)
      (and (= (get-value card) king)
	   (list 2 (get-name card) (_"an empty slot" )))
      (and (= (get-value (get-top-card slot-id)) (+ 1 (get-value card)))
	   (not (= (get-color (get-top-card slot-id)) (get-color card)))
	   (list 1 (get-name card) (get-name (get-top-card slot-id))))))

(define (any-slot-nonempty? slots)
  (if (eq? slots '())
      #f
      (or (not (empty-slot? (car slots)))
          (any-slot-nonempty? (cdr slots)))))

(define (get-hint)
  (or (or-map is-ace? (cons waste tableau))
      (or-map shiftable-iter tableau)
      (and (not (empty-slot? waste))
	   (set! card (get-top-card waste))
	   (or-map addable? tableau))
      (or-map ploppable? foundation)
      (and (not kings-only)
           (any-slot-empty? tableau)
           (any-slot-nonempty? (cons waste tableau))
           (list 0 (_"Consider moving something into an empty slot")))
      (and (or (and (or (= max-redeal -1)
			(< FLIP-COUNTER max-redeal))
		    (not (empty-slot? waste)))
	       (not (empty-slot? stock))) 
	   (list 0 (_"Deal a new card from the deck")))
; FIXME: need to give proper hints for this case too ...
      (and (not (and-map empty-slot? foundation))
           (list 0 (_"Try moving cards down from the foundation")))
      (list 0 (_"No hint available right now"))))

; The game is won if all of the cards are in the foundation.  However
; to reduce mind-numbing tedium in an end came of stacking cards to
; finish the game, the program observers that the game is also won if
; all the cards in the tableau are revealed *and* this is not the last
; redeal. 
;
; If it is the last redeal it's possible cards are in the waste that
; are needed to go to the foundation have already been passed over.
; For example: waste: 2H 10S 10C 9H -- 9H exposed. Clearly we can't
; move the 9H, so there's no chance of getting to the 2H.
(define (game-won)
  (or 
   ; FIXME: DRY
   (and (= 13 (length (get-cards 2)))
	(= 13 (length (get-cards 3)))
	(= 13 (length (get-cards 4)))
	(= 13 (length (get-cards 5))))
   (and 
    (or (< max-redeal 0) (> (- max-redeal FLIP-COUNTER) 0))
    ; slot 6 is always visible or empty
    ; FIXME: DRY
    (or (empty-slot? 7) (is-visible? (car (reverse (get-cards 7)))))
    (or (empty-slot? 8) (is-visible? (car (reverse (get-cards 8)))))
    (or (empty-slot? 9) (is-visible? (car (reverse (get-cards 9)))))
    (or (empty-slot? 10) (is-visible? (car (reverse (get-cards 10)))))
    (or (empty-slot? 11) (is-visible? (car (reverse (get-cards 11)))))
    (or (empty-slot? 12) (is-visible? (car (reverse (get-cards 12))))))))
   

; The hints still miss some useful reversible moves:
;
; 1) unplopping cards to assist in shifting groups,
; 2) unplopping cards to assist in plopping cards in other suits, 
; 3) shifting groups to assist in plopping & unplopping cards.
;
; so we must NOT report game-over when they run out.

(define (game-over)
  (give-status-message)
  (not (game-won)))

(define (get-options)
  (list 'begin-exclusive 
	(list (_"Three card deals") deal-three)
	(list (_"Single card deals") deal-one)
	(list (_"No redeals") no-redeal)
	'end-exclusive))

(define (apply-options options)
  (set! deal-three (cadr (list-ref options 1)))
  (set! deal-one (cadr (list-ref options 2)))
  (set! no-redeal (cadr (list-ref options 3)))
  (set! max-redeal (cond (no-redeal 0)
			 (deal-one 2)
			 (#t -1))))

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
