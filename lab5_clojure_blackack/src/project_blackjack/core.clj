;; Zachary Minott
;; CECS 424 
;; 12/15/2019

(ns project-blackjack.core
  (:gen-class))

;; When debugging, you can use (pprint x) to do a "pretty print" of the variable x,
;; instead of x being printed on a single line.

;; A card is a kind (integer 1-13) and suit (string)
;; Creates a struct map card 
(defn make-card [kind suit]
  {:kind kind, :suit suit})

;; Selector for a card's Kind.
(defn kind [card]
  (:kind card))

;; Selector for a card's Suit.
(defn suit [card]
  (:suit card))

;; A "tostring" method for cards.
(defn card-str [card]
  ;; DONE: replace the following line with logic that converts the card's kind to a string.
  ;; Reminder: a 1 means "Ace", 11 means "Jack", 12 means "Queen", 13 means "King".
  ;; Any other kind should be converted directly to a string, e.g., 2 becomes "2".
  (let [kind (cond
               (= (kind card) 1) "Ace"
               (= (kind card) 11) "Jack"
               (= (kind card) 12) "Queen"
               (= (kind card) 13) "King"
               :else (str (kind card)))
        
        ;; DONE: then do the same thing for the card's suit. 0 = "Spades", 1 = "Clubs",
        ;; 2 = "Diamonds", 3 = "Hearts"
        suit (cond 
               (= (suit card) 0) "Spades"
               (= (suit card) 1) "Clubs"
               (= (suit card) 2) "Diamonds"
               (= (suit card) 3) "Hearts")]

    ;; Returns a string of the form "[kind] of [suit]"
    (str " " kind " of " suit " ")))

;; Returns the integer value of a card.
(defn card-value [card]
  (case (kind card)
    1 11 ;; ace
    11 10 ;; face cards
    12 10
    13 10
    ;; the final case is the "default"
    (kind card)))

;; Returns the total number of "points" in the hand.
(defn hand-total [hand]
  (let [;; DONE: modify the next line to sum the card values of each card in the hand.
        ;; HINT: map and reduce
        sum (reduce + (map #(card-value %) hand)) ;; CHECK!
        ;; DONE: modify the next line to count the number of aces in the hand.
        ;; HINT: filter and count
        num-aces (count (filter #(= 1 (kind %)) hand ))] ;; CHECK!
    (if (or (<= sum 21) (zero? num-aces))
      sum ;; no adjustment if the sum doesn't exceed 21 or there are no aces
      (let [max-aces (int (Math/ceil (/ (- sum 21) 10)))]
        ;; if we exceed 21, then reduce by 10 points for each ace until we are good
        (- sum (* 10 (min num-aces max-aces)))))))

;; Constructs a new unshuffled deck as a list of cards
(defn make-deck []
  (for [suit (range 0 4)
        kind (range 1 14)]
    (make-card kind suit)))

;; The game state consists of the deck to draw from, the player's hand, and the dealer's hand.
(defn make-state [draw player dealer]
  {:deck draw, :player player, :dealer dealer})

;; Selector for the player's hand.
(defn player-hand [game-state]
  (:player game-state))

;; Selector for the dealer's hand.
(defn dealer-hand [game-state]
  (:dealer game-state))

;; Selector for the deck (draw pile).
(defn deck [game-state]
  (:deck game-state))

;; Given an owner that is either :player or :dealer, selects that owner's hand from the game state.
(defn hand [game-state owner]
  (owner game-state))


;; A new game is started by taking a new deck, shuffling it, giving the first and third
;; card to the player, and the second and fourth to the dealer.
(defn new-game []
  (let [new-deck (make-deck)
        shuffled-deck (shuffle new-deck)
        player-hand (list (first shuffled-deck) (nth shuffled-deck 2))
        dealer-hand (list (second shuffled-deck) (nth shuffled-deck 3))
        draw-pile (drop 4 shuffled-deck)]
    (make-state draw-pile player-hand dealer-hand)))


;; Given a game state and an owner that is either :player or :dealer,
;; deal one card from the deck and add it to the front of the given owner's hand.
;; Return the new game state.
(defn hit [game-state owner]
  ;; DONE: take the top (first) card from the game state's deck and cons it onto the hand
  ;; for the given owner. Return the new game state, including a new deck with the top
  ;; card removed.

  (let [next-card (first (game-state :deck))
        new-player-hand (if (= :player owner) 
                          (cons next-card (player-hand game-state))
                          (player-hand game-state))
        new-dealer-hand (if (= :dealer owner)
                          (cons next-card (dealer-hand game-state))
                          (dealer-hand game-state))
        new-deck (rest (game-state :deck))]
    (make-state new-deck new-player-hand new-dealer-hand))
  )

;; Given a game state, takes the dealer's turn and returns a new game state after the 
;; dealer has acted.
(defn dealer-turn [game-state]
  ;; Get the dealer's hand and total score.
  (let [dealer (hand game-state :dealer)
        score (hand-total dealer)]

    ;; CHECK / DONE: the following line prints the cards in the dealer's hand, but with ugly output
    ;; because Clojure doesn't know how to format a card variable for output. Transform each card
    ;; in the hand to a string using card-str.
    (println (str "Dealer's hand: "
                  (clojure.string/join ", " (map card-str dealer))
                  "; "
                  score
                  " points."))

    ;; Dealer rules: must hit if score < 17
    (cond
      (> score 21)
      ;; do allows us to have more than one statement in a branch.
      (do (println "Dealer busts!")
          game-state)

      (< score 17)
      (do (println "Dealer hits")
          ;; the game state is changed; the result of "hit" is the new state.
          ;; the dealer gets to take another action using the new state.
          (dealer-turn (hit game-state :dealer)))

      :else
      (do (println "Dealer must stay")
          game-state))))

;; Given a game state and a strategy, takes the player's entire turn by recursively applying
;; the strategy until the strategy decides to stay (not hit). Returns the new game state
;; after the player's turn is complete.
(defn player-turn [game-state player-strategy]
  ;; DONE: code this method using dealer-turn as a guide. Follow the same standard
  ;; of printing output. The function must reutrn the new game state after the player's action has finished.
  (let [player (hand game-state :player)
        score (hand-total player)]

    (println (str "Players Hand: "
                  (clojure.string/join ", " (map card-str player))
                  "; "
                  score
                  " points."))

  (cond
    (> score 21)
    (do
      (println "You bust!") game-state)
    
    (true? (player-strategy game-state))
    (do (println "You hit!")
        (player-turn (hit game-state :player) player-strategy))
    
    :else
    (do (println "You stayed")
            game-state)))
    )
    
    
    
    
    
  ;; Unlike the dealer, the player gets to make choices about whether they will hit or stay.
  ;; The (< score 17) branch from dealer-turn is inappropriate; in its place, we will allow a
  ;; "strategy" to decide whether to hit. A strategy is a function that accepts the current
  ;; game state and returns true if the player should hit, and false otherwise.
  ;; player-turn must call the player-strategy function to decide whether to hit or stay.


;; A type for the log of results from many games.
(defn make-log [player-wins dealer-wins draws]
  {:player-wins player-wins
   :dealer-wins dealer-wins
   :draws draws})

;; Adds two game log objects into a single sum log.
(defn add-logs [log1 log2]
  (make-log
   (+ (:player-wins log1) (:player-wins log2))
   (+ (:dealer-wins log1) (:dealer-wins log2))
   (+ (:draws log1) (:draws log2))))

;; Plays one game of blackjack in which the player follows the given strategy.
;; Returns a log of the result of the game.
(defn one-game [game-state player-strategy]
  ;; Done: replace the 0 on the next line with the card-str of the dealer's first card.
  (println "Dealer is showing: " (card-str (first (dealer-hand game-state))))

  ;; Done: play the game! First the player gets their turn. The dealer then takes their turn,
  ;; using the state of the game after the player's turn finished.
  (cond 
    (and (= (hand-total (hand game-state :dealer)) 21) (= (hand-total (hand game-state :player)) 21) )
    (do 
      (println "DRAW! Both player and dealer have a natural blackjack!!!")
      (make-log 0 0 1))
    
    (= (hand-total (hand game-state :dealer)) 21)
    (do 
      (println "Dealer has a Natural Blackjack!")
      (make-log 0 1 0))
    
    (= (hand-total (hand game-state :player)) 21)
    (do 
      (println "Player has a Natural Blackjack!")
      (make-log 1 0 0))
    
      ;; Done: determine the winner! Get the hand scores for the dealer and the player.
    
    :else (let [player-play (player-turn game-state player-strategy)
                dealer-play (dealer-turn game-state)
                dealer-score (hand-total (dealer-hand dealer-play))
                player-score (hand-total (player-hand player-play))]
            (cond
              (and (<= player-score 21) (or (> player-score dealer-score) (> dealer-score 21)))
              (do (println "You Win!")
                  (println "=================================")
                  (make-log 1 0 0))

              (and (and (<= player-score 21) (<= dealer-score 21)) (= player-score dealer-score))
              (do (println "Draw")
                  (println "=================================")
                  (make-log 0 0 1))

              :else 
              (do (println "The Dealer Wins!")
                  (println "=================================")
                  (make-log 0 1 0))

              )))
  )

;; Plays n games of blackjack with the given player strategy. Returns a game log
;; summarizing the number of wins, losses, and draws.
(defn many-games [n player-strategy]
  (letfn [;; This defines an inner helper function for doing the tail recursion.
          (many-games-tail [n player-strategy accumulated-log]     
                           (if (= n 1)
                             (add-logs accumulated-log (one-game (new-game) player-strategy))
                             (many-games-tail (dec n) player-strategy (add-logs accumulated-log (one-game (new-game) player-strategy))))
                           )]
    ;; Start the tail recursion with a blank accumulated-log.
    (many-games-tail n player-strategy (make-log 0 0 0))))


;;; Player strategies.

;; The interactive strategy asks the user if they want to hit.
(defn interactive-player-strategy [game-state]
  (println "(h)it or (s)tay?")
  (flush)
  (let [input (read-line)]
    (= "h" input))) ;; return true if the user enters "h", false otherwise.

(defn inactive-player-strategy [game-state]
  (println "Player stays"))

(defn greedy-player-strategy [game-state]
  (let [score (hand-total (hand game-state :player))]
    (< score 21)))

(defn coin-flip-player-strategy [game-state]
  (let [coin-flip (rand-int 2)]
    (println "Coin Flip : " coin-flip)
    (= coin-flip 1)))

(defn basic-player-strategy [game-state]
  (let [dealer-first (first (hand game-state :dealer))
        player-total (hand-total (hand game-state :player))
        num-aces (count (filter #(= 1 (kind %)) (hand game-state :player)))]
    (cond
      (and (and (<= (kind dealer-first) 6) (>= (kind dealer-first) 2)) (< player-total 12)) true
      (and (and (<= (kind dealer-first) 13) (>= (kind dealer-first) 7)) (<= player-total 16)) true
      (and (= (kind dealer-first) 1) (or (and (>= num-aces 1) (<= player-total 16)) (<= player-total 11))) true
      :else false
      )))


(defn -main [& args]
  ;;(print (many-games 1 interactive-player-strategy))
  ;;(print (many-games 1000 inactive-player-strategy))
  ;;(print (many-games 1000 greedy-player-strategy))
  ;;(print (many-games 1000 coin-flip-player-strategy))
  (print (many-games 1000 basic-player-strategy))
  )

(-main)