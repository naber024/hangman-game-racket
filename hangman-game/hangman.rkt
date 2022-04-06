;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hangman) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; GAME PROJECT
;; Guess the word!

;; FEATURES:
;; Recursion used in: show-lives, choose-word, guessed-word, fill
;; Worldstate: Hangman-Structure (list, list, number, string)
;; 
;; EXTRA FEATURES:
;; Local variables used in: check-guess, lose-one-life, replace
;; Map: get-guess
;; Lambda: get-guess
;; Explode: get-secret-word
;; Implode: render
;; Randomness: choose-word

(require 2htdp/image)
(define heart-image (bitmap "heart.png"))

(define width 500)
(define height 300)
(define half-w (/ width 2))
(define half-h (/ height 2))
(define background (empty-scene width height))
(define heart (scale 0.5 heart-image))

;; words: list of possible words for the game
(define words (list "bear" "mouse" "eagle" "hawk" "giraffe" "lion" "zebra" "bunny" "rabbit" "goat" "duck" "fox"
                    "elephant" "cat" "dog" "otter" "penguin" "frog" "alligator" "ant" "dolphin" "flamingo"
                    "hedgehog" "kangaroo" "leopard" "lobster" "pig" "raccoon"))

;; choose-word : list-of-strings, number -> string
;; takes a non-empty list of words and a number n and returns the n word in the list
(define (choose-word los n)
  (cond [(or (empty? (rest los)) (= n 1)) (first los)]
        [else (choose-word (rest los) (- n 1))]))

;; get-secret-word: los -> list-of-1Strings
;; takes the list of words and returns a list of 1Strings,
;; based on a random word in the list
(define (get-secret-word los)
  (explode (choose-word los (random (length los)))))

;; guessed-word: los -> los
;; takes a list of Strings and returns a list where each member in the list is replaced by an underscore
(define (get-guess los)
  (map (lambda (x) "_") los))

;; the ws is a structure of two lists and one number
;; the secret-word, guessed-word and the number of lives left
(define-struct hangman [word guess lives state])

;; new-hangman: los -> hangman
;; takes a list of words and returns a hangman-structure (the starting point for the game)
(define (new-hangman los)
  (local ((define secret-word (get-secret-word los)))
    (make-hangman secret-word (get-guess secret-word) 6 "GUESS")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; BIG BANG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-key check-guess]))

;; render: ws -> image
;; takes the ws and renders the image
(define (render ws)
  (cond [(string=? "GUESS" (hangman-state ws)) (place-image (show-lives (hangman-lives ws)) half-w 80
                                               (place-image (text (implode (hangman-guess ws)) 25 "black") half-w half-h background))]
        [(string=? "LOST" (hangman-state ws))  (place-image (above (text (string-append "You lost. The secret word was "
                                                                   (implode (hangman-word ws)) ".") 25 "black")
                                                                   (text "Press SPACE to play again." 25 "blue")) half-w half-h background)]
        [(string=? "WON" (hangman-state ws))   (place-image (above (text "You won!" 25 "black")
                                                                   (text "Press SPACE to play again." 25 "blue")) half-w half-h background)]
        [else                                  (place-image (above (text "Guess the secret word!" 25 "black")
                                                                   (text "Restart by pressing SPACE." 25 "blue")) half-w half-h background)]))

;; show-lives: number -> image
;; takes the number of lives left and returns and image of all the hearts placed
;; next to each other
(define (show-lives n)
  (cond [(>= 0 n) empty-image]
        [else (beside heart (show-lives (- n 1)))]))

;; check-guess: ws, key -> ws
;; takes a world state and a key pressed and returns a new world state
;; if the guess was right, it shows the according symbol
;; if it was wrong you loose one life
(define (check-guess ws key)
  (local ((define word (hangman-word ws))
          (define guess (hangman-guess ws))
          (define lives (hangman-lives ws)))
  (cond [(key=? key " ") (new-hangman words)]
        [(string=? "START" (hangman-state ws)) (make-hangman word guess lives "GUESS")]
        [(not (member? "_" guess))             (make-hangman word guess lives "WON")]
        [(won-or-lost? ws)                     ws]
        [(member? key word)                    (replace ws key)]
        [else                                  (lose-one-life ws)])))

;; won-or-lost?: ws -> boolean
;; takes a worldstate and returns #true if the player lost or won the game
(define (won-or-lost? ws)
  (or (string=? "WON" (hangman-state ws)) (string=? "LOST" (hangman-state ws))))

;; lose-one-life: ws -> ws
;; takes a ws and returns the ws with one life lost
(define (lose-one-life ws)
  (local ((define lives (- (hangman-lives ws) 1)))
  (if (> 0 lives)
      (make-hangman (hangman-word ws) (hangman-guess ws) lives "LOST")
      (make-hangman (hangman-word ws) (hangman-guess ws) lives "GUESS"))))

;; replace: ws, key -> ws
;; takes a ws and a key and replaces the letter in the secret word with the key pressed
(define (replace ws key)
  (local ((define guess (fill (hangman-word ws) (hangman-guess ws) key)))
        (make-hangman (hangman-word ws) guess (hangman-lives ws) "GUESS")))

;; fill: list, list, key -> list
;; takes the list of the secret word, the guessed word and the key
;;returns the guessed-word-list with all the corresponding letters replaced
(check-expect (fill (list "b" "e" "a" "r") (list "_" "_" "_" "_") "a") (list "_" "_" "a" "_"))
(check-expect (fill (list "b" "e" "e" "r") (list "b" "_" "_" "_") "e") (list "b" "e" "e" "_"))
(check-expect (fill (list "b" "e" "a" "r") (list "_" "_" "_" "_") "r") (list "_" "_" "_" "r"))

(define (fill secret guess key)
  (cond [(empty? guess)                empty]
        [(string=? (first secret) key) (cons key (fill (rest secret) (rest guess) key))]
        [ else                         (cons (first guess) (fill (rest secret) (rest guess) key))]))

;; START THE GAME
(define secret-word (get-secret-word words))
(define guessed-word (get-guess secret-word))
(main (make-hangman secret-word guessed-word 6 "START"))
