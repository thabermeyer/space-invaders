;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; ~~~ SPACE INVADERS ~~~
;; a simple interactive game. Shoot down the invaders before they touch the bottom of the screen!

;; start game with the following command: main(G0)


;; === CONSTANTS ==================================================================================================================

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define TANK-Y (- HEIGHT 15))

(define MISSILE-START (- TANK-Y 10))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; === DATA DEFINITIONS ==================================================================================================================

;; ++ GAME

(define-struct game (invaders missiles tank))
;; Game is (make-game  ListOfInvader ListOfMissile Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom       (game-missiles g))
       (fn-for-tank      (game-tank g))))



;; ++ TANK

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center moving left
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank (/ WIDTH 2) -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



;; ++ INVADER

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100             INVADER-X-SPEED))    ;not landed, moving right
(define I2 (make-invader 150 HEIGHT       (- INVADER-X-SPEED)))   ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10)   INVADER-X-SPEED))    ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))



;; ++ LIST OF INVADER

;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of all the invaders currently on the MTS

(define LOI0 empty)
(define LOI1 (cons (make-invader 100 0 10) empty)) ; a single invader appearing at the top of the screen at the x-coordinate 100, moving at 10px per tick
(define LOI2 (cons (make-invader 100 0 10) (cons (make-invader 250 30 10) empty))) ; two invaders on screen

#;
(define (fn-for-loi loi)
  (cond [(empty? loi)(...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi     (rest loi)))]))



;; ++ MISSILE

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1 
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1       

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



;; ++ LIST OF MISSILE

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of all the missiles currently on the MTS

(define LOM0 empty)
(define LOM1 (cons (make-missile 100 400) empty)) ; a single missle that was shot when the tank was at the x-coordinate 100
(define LOM2 (cons (make-missile 100 200) (cons (make-missile 250 15) empty))) ; two missles, one shot at x-coordinate 100 and the other shot at x-coordinate 250

#;
(define (fn-for-lom lom)
  (cond [(empty? lom)(...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom     (rest lom)))]))



;; GAME STATE EXAMPLES

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game empty empty T2))



;; === FUNCTIONS ==================================================================================================================

;; ++ MAIN

;; Game -> Game
;; start the world with (main G0)

(define (main g)
  (big-bang g                            ; Game
            (on-tick   advance-game)     ; Game -> Game
            (to-draw   render-game)      ; Game -> Image
            (stop-when stop-game)        ; Game -> Boolean
            (on-key    control-tank)))   ; Game KeyEvent -> Game



;; ++ advance-game (on-tick)

;; Game -> Game
;; produce the next state of Game to render onscreen

(check-expect (advance-game G0) (make-game (add-invader (advance-invaders (remove-invaders empty empty)))
                                           (advance-missiles (remove-missiles empty empty))
                                           (advance-tank T0)))
(check-expect (advance-game G1) (make-game (add-invader (advance-invaders (remove-invaders empty empty)))
                                           (advance-missiles (remove-missiles empty empty))
                                           (advance-tank T1)))
(check-expect (advance-game G2) (make-game (add-invader (advance-invaders (remove-invaders (list M1)(list I1))))
                                           (advance-missiles (remove-missiles (list M1)(list I1)))
                                           (advance-tank T1)))
(check-expect (advance-game G3) (make-game (add-invader (advance-invaders (remove-invaders (list M1 M2)(list I1 I2))))
                                           (advance-missiles (remove-missiles (list M1 M2)(list I1 I2)))
                                           (advance-tank T1)))

;(define (advance-game g) g) ;stub

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom       (game-missiles g))
       (fn-for-tank      (game-tank g))))


(define (advance-game g)
  (make-game (add-invader (advance-invaders (remove-invaders (game-missiles g)(game-invaders g))))
             (advance-missiles  (remove-missiles (game-missiles g)(game-invaders g)))
             (advance-tank (game-tank g))))



;; ++ render-game (to-draw)

;; Game -> Image
;; render the current state of Game on the MTS 

(check-expect (render-game G0) (place-image TANK (/ WIDTH 2) TANK-Y MTS))
(check-expect (render-game G1) (place-image TANK 50 TANK-Y MTS))
(check-expect (render-game G2) (place-image INVADER 150 100 (place-image MISSILE 150 300 (place-image TANK 50 TANK-Y MTS))))
(check-expect (render-game G3) (place-image INVADER 150 100 (place-image INVADER 150 HEIGHT (place-image MISSILE 150 300 (place-image MISSILE 150 110 (place-image TANK 50 TANK-Y MTS))))))

;(define (render-game g) empty-image) ;stub

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))

(define (render-game g)
  (render-invaders (game-invaders g)(render-missiles (game-missiles g)(render-tank (game-tank g)))))



;; ++ stop-game (stop-when)

;; Game -> Boolean
;; stop the game when an invader touches the bottom of the screen (y = HEIGHT)

(check-expect (stop-game (make-game (list (make-invader 100 150 -1) (make-invader 200 100 1)) empty T0))          false)
(check-expect (stop-game (make-game (list (make-invader 100 (- HEIGHT 1) -1) (make-invader 200 100 1)) empty T0)) false)
(check-expect (stop-game (make-game (list (make-invader 100 150 -1) (make-invader 200 (- HEIGHT 1) 1)) empty T0)) false)
(check-expect (stop-game (make-game (list (make-invader 100 HEIGHT -1) (make-invader 200 100 1)) empty T0))        true)
(check-expect (stop-game (make-game (list (make-invader 100 150 -1) (make-invader 200 HEIGHT 1)) empty T0))        true)

;(define (stop-game g) false) ;stub

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))

(define (stop-game g)
  (hit-bottom? (game-invaders g)))



;; ++ control-tank (on-key)

;; Game KeyEvent -> Game
;; move the tank to the left or right when pressing the left or right arrow keys
;; tank should continue moving in either left or right direction after pressing the left or right arrow key once
;; shoot a missile from the tank with the spacebar key

(check-expect (control-tank G0 "up")     G0)
(check-expect (control-tank G0 "down")   G0)
(check-expect (control-tank G0 "right")  G0)
(check-expect (control-tank G0 "left")   (make-game empty empty (make-tank (- (/ WIDTH 2) TANK-SPEED) -1)))
(check-expect (control-tank G4 "right")  (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED)  1)))
(check-expect (control-tank G4 "left")   G4)
(check-expect (control-tank G0 " ")      (make-game empty (list (make-missile (tank-x (game-tank G0)) MISSILE-START)) (game-tank G0)))

;(define (control-tank g key) g) ;stub

#;
(define (control-tank g key)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))

(define (control-tank g key)
  (cond [(and (key=? key "right")(= (tank-dir (game-tank g)) -1)) (make-game (game-invaders g)(game-missiles g)(move-tank (game-tank g) "right"))]
        [(and (key=? key "left") (= (tank-dir (game-tank g)) 1))  (make-game (game-invaders g)(game-missiles g)(move-tank (game-tank g) "left"))]
        [(key=? key " ")                                          (make-game (game-invaders g)(shoot-missile (game-missiles g)(tank-x (game-tank g)))(game-tank g))]
        [else g]))



;; === HELPER FUNCTIONS ==================================================================================================================

;; ++ add-invader

;; for: advance-game

;; ListOfInvader -> ListOfInvader
;; produce a ListOfInvader that randomly adds a new invader to the top of the list

(check-member-of (add-invader empty)
                 empty
                 (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)))
(check-member-of (add-invader (list (make-invader 300 200 INVADER-X-SPEED)))
                 (list (make-invader 300 200 INVADER-X-SPEED))
                 (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)(make-invader 300 200 INVADER-X-SPEED)))
(check-member-of (add-invader (list (make-invader 0 200 (- INVADER-X-SPEED))(make-invader 300 200 INVADER-X-SPEED)))
                 (list (make-invader 0 200 (- INVADER-X-SPEED))(make-invader 300 200 INVADER-X-SPEED))
                 (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)(make-invader 0 200 (- INVADER-X-SPEED))(make-invader 300 200 INVADER-X-SPEED)))

;(define (add-invader loi) loi) ;stub

#;
(define (fn-for-loi loi)
  (cond [(empty? loi)(...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi     (rest loi)))]))


(define (add-invader loi)
  (if (<= (random INVADE-RATE) 3)
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
      loi))



;; ++ advance-invaders

;; -- for: advance-game

;; ListOfInvader -> ListOfInvader
;; produce a ListOfInvader with the x and y of each Invader incremented

(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list I1))     (list (make-invader (+ 150    INVADER-X-SPEED) (+ 100    INVADER-Y-SPEED)    INVADER-X-SPEED)))
(check-expect (advance-invaders (list I1 I2))  (list (make-invader (+ 150    INVADER-X-SPEED) (+ 100    INVADER-Y-SPEED)    INVADER-X-SPEED)
                                                     (make-invader (+ 150 (- INVADER-X-SPEED))(+ HEIGHT INVADER-Y-SPEED) (- INVADER-X-SPEED))))

;(define (advance-invaders loi) loi) ;stub

#;
(define (fn-for-loi loi)
  (cond [(empty? loi)(...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi     (rest loi)))]))


(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader  (first loi))
               (advance-invaders (rest loi)))]))



;; ++ advance-invader

;; -- for: advance-invaders

;; Invader -> Invader
;; produce a single Invader with the x and y of each incremented

(check-expect (advance-invader I1)                                       (make-invader (+ 150    INVADER-X-SPEED)  (+ 100    INVADER-Y-SPEED)   INVADER-X-SPEED))
(check-expect (advance-invader I2)                                       (make-invader (+ 150 (- INVADER-X-SPEED)) (+ HEIGHT INVADER-Y-SPEED)(- INVADER-X-SPEED)))
(check-expect (advance-invader (make-invader 300 200  INVADER-X-SPEED))  (make-invader (+ 300 (- INVADER-X-SPEED)) (+ 200    INVADER-Y-SPEED)(- INVADER-X-SPEED)))
(check-expect (advance-invader (make-invader 0 200 (- INVADER-X-SPEED))) (make-invader (+ 0      INVADER-X-SPEED)  (+ 200    INVADER-Y-SPEED)   INVADER-X-SPEED))

;(define (advance-invader i) i) ;stub

#;
(define (fn-for-invader i)
  (... (invader-x i) (invader-y i) (invader-dx i)))


(define (advance-invader i)
  (if (or (>= (+ (invader-x i)(invader-dx i)) WIDTH)(<= (+ (invader-x i)(invader-dx i)) 0))
      (make-invader (+ (invader-x i)(- (invader-dx i)))(+ (invader-y i) INVADER-Y-SPEED)(- (invader-dx i)))
      (make-invader (+ (invader-x i)   (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED)   (invader-dx i))))



;; ++ advance-missiles

;; -- for: advance-game

;; ListOfMissile -> ListOfMissile
;; produce a ListOfMissile with the y of each Missile incremented
;; if Missle's y is < 0, remove it from the list

(check-expect (advance-missiles empty)                        empty)
(check-expect (advance-missiles LOM1)                         (list (make-missile 100 (- 400 MISSILE-SPEED))))
(check-expect (advance-missiles LOM2)                         (list (make-missile 100 (- 200 MISSILE-SPEED)) (make-missile 250 (- 15 MISSILE-SPEED))))
(check-expect (advance-missiles (list (make-missile 150 10))) (list (make-missile 150 0)))
(check-expect (advance-missiles (list (make-missile 150 9)))  empty)

;(define (advance-missiles lom) lom) ;stub

#;
(define (fn-for-lom lom)
  (cond [(empty? lom)(...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom     (rest lom)))]))


(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [(visible-missile? (advance-missile (first lom)))
         (cons (advance-missile  (first lom))
               (advance-missiles (rest lom)))]
        [else
         (advance-missiles (rest lom))]))



;; ++ advance-missile

;; -- for: advance-missiles

;; Missile -> Missile
;; produce a single Missile with the y subtracted by MISSILE-SPEED

(check-expect (advance-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (advance-missile M2) (make-missile 150 (- 110 MISSILE-SPEED)))

;(define (advance-missile m) m) ;stub

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; ++ advance-tank

;; for: advance-game

;; Tank -> Tank
;; produce the next iteration of a given tank, moving in the direction of its dir value
;; if tank hits either of outer edges of MTS (0 and WIDTH), it stops

(check-expect (advance-tank (make-tank 50  1))    (make-tank (+ 50 TANK-SPEED)  1))
(check-expect (advance-tank (make-tank 50 -1))    (make-tank (- 50 TANK-SPEED) -1))
(check-expect (advance-tank (make-tank WIDTH  1)) (make-tank WIDTH  1))
(check-expect (advance-tank (make-tank 0 -1))     (make-tank 0 -1))

;(define (advance-tank t) t) ;stub

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define (advance-tank t)
  (if (or (>= (tank-x t) WIDTH)(<= (tank-x t) 0))
      t
      (if (= (tank-dir t) 1)
          (make-tank (+ (tank-x t) TANK-SPEED)  1)
          (make-tank (- (tank-x t) TANK-SPEED) -1))))



;; ++ visible-missile?

;; -- for: advance-missiles

;; Missile -> Boolean
;; produce true if the Missile's y coordinate is >= 0

(check-expect (visible-missile? (make-missile 150 (- 100 MISSILE-SPEED))) true)
(check-expect (visible-missile? (make-missile 150 (- 10 MISSILE-SPEED))) true)
(check-expect (visible-missile? (make-missile 150 (- 9 MISSILE-SPEED))) false)

;(define (visible-missile? m) false) ;stub

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define (visible-missile? m)
  (>= (missile-y m) 0))



;; ++ remove-invaders

;; -- for: advance-game

;; ListOfMissiles ListOfInvaders -> ListOfInvaders
;; produce a ListOfInvaders with Invaders removed that have been hit by a Missile

(check-expect (remove-invaders empty empty) empty) ; no invaders provided; return empty
(check-expect (remove-invaders (list (make-missile 150 200)) empty) empty) ; missiles but no invaders; return empty
(check-expect (remove-invaders empty (list (make-invader 150 200 INVADER-X-SPEED))) (list (make-invader 150 200 INVADER-X-SPEED))) ; invaders but no missiles; return invaders

(check-expect (remove-invaders (list (make-missile 150 200))(list (make-invader 139 200 INVADER-X-SPEED))) (list (make-invader 139 200 INVADER-X-SPEED))) ; missile-x is out of range of invader-x (low)
(check-expect (remove-invaders (list (make-missile 150 200))(list (make-invader 161 200 INVADER-X-SPEED))) (list (make-invader 161 200 INVADER-X-SPEED))) ; missile-x is out of range of invader-x (high)

(check-expect (remove-invaders (list (make-missile 150 200))(list (make-invader 150 200 INVADER-X-SPEED))) empty) ; a single missile hits a single invader; return empty

(check-expect (remove-invaders (list (make-missile 150 200))(list (make-invader 150 200 INVADER-X-SPEED)
                                                                  (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED))) (list (make-invader 100 200 INVADER-X-SPEED)
                                                                                                                 (make-invader 101 200 INVADER-X-SPEED))) ; a single missle + hit invader #1 = return invader #2 & 3

(check-expect (remove-invaders (list (make-missile 150 200))(list (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 150 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED))) (list (make-invader 100 200 INVADER-X-SPEED)
                                                                                                                 (make-invader 101 200 INVADER-X-SPEED))) ; a single missle + hit invader #2 = return invader #1 & 3

(check-expect (remove-invaders (list (make-missile 150 200))(list (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED)
                                                                  (make-invader 150 200 INVADER-X-SPEED))) (list (make-invader 100 200 INVADER-X-SPEED)
                                                                                                                 (make-invader 101 200 INVADER-X-SPEED))) ; a single missle + hit invader #3 = return invader #1 & 2



(check-expect (remove-invaders (list (make-missile 150 200)
                                     (make-missile 138 200)
                                     (make-missile 139 200))(list (make-invader 150 200 INVADER-X-SPEED)
                                                                  (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED))) (list (make-invader 100 200 INVADER-X-SPEED)
                                                                                                                 (make-invader 101 200 INVADER-X-SPEED)))  ; missile #1 hit invader #1 = return invader #2 & #3

(check-expect (remove-invaders (list (make-missile 138 200)
                                     (make-missile 150 200)
                                     (make-missile 139 200))(list (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 150 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED))) (list (make-invader 100 200 INVADER-X-SPEED)
                                                                                                                 (make-invader 101 200 INVADER-X-SPEED))) ; missile #2 hit invader #2 = return invader #1 & #3

(check-expect (remove-invaders (list (make-missile 138 200)
                                     (make-missile 139 200)
                                     (make-missile 150 200))(list (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED)
                                                                  (make-invader 150 200 INVADER-X-SPEED))) (list (make-invader 100 200 INVADER-X-SPEED)
                                                                                                                 (make-invader 101 200 INVADER-X-SPEED))) ; missile #3 hit invader #3 = return invader #1 & #2

;(define (remove-invaders lom loi) loi) ;stub

#;
(define (fn-for-loi loi)
  (cond [(empty? loi)(...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi     (rest loi)))]))


(define (remove-invaders lom loi)
  (cond [(empty? lom) loi]
        [(empty? loi) empty]
        [else (if (hit-invader? lom (first loi))
              (remove-invaders (rest lom)(rest loi))
              (cons (first loi)(remove-invaders lom (rest loi))))]))



;; ++ remove-missiles

;; -- for: advance-game

;; ListOfMissiles ListOfInvaders  -> ListOfMissiles
;; produce a ListOfMissiles with Missiles removed that have hit an Invader

(check-expect (remove-missiles empty empty) empty) ; no missiles provided; return empty
(check-expect (remove-missiles (list (make-missile 150 200)) empty) (list (make-missile 150 200))) ; missiles but no invaders; return missiles
(check-expect (remove-missiles empty (list (make-invader 150 200 INVADER-X-SPEED))) empty) ; invaders but no missiles; return empty

(check-expect (remove-missiles (list (make-missile 139 200))(list (make-invader 150 200 INVADER-X-SPEED))) (list (make-missile 139 200))) ; missile-x is out of range of invader-x (low)
(check-expect (remove-missiles (list (make-missile 161 200))(list (make-invader 150 200 INVADER-X-SPEED))) (list (make-missile 161 200))) ; missile-x is out of range of invader-x (high)

(check-expect (remove-missiles (list (make-missile 150 200))(list (make-invader 150 200 INVADER-X-SPEED))) empty) ; a single missile hits a single invader; return empty

(check-expect (remove-missiles (list (make-missile 150 200))(list (make-invader 150 200 INVADER-X-SPEED)
                                                                  (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED))) empty) ; a single missle + hit invader #1 = return empty

(check-expect (remove-missiles (list (make-missile 150 200))(list (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 150 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED))) empty) ; a single missile + hit invader #2 = return empty

(check-expect (remove-missiles (list (make-missile 150 200))(list (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED)
                                                                  (make-invader 150 200 INVADER-X-SPEED))) empty) ; a single missile + hit invader #3 = return empty

(check-expect (remove-missiles (list (make-missile 150 200)
                                     (make-missile 138 200)
                                     (make-missile 139 200))(list (make-invader 150 200 INVADER-X-SPEED)
                                                                  (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED))) (list (make-missile 138 200)
                                                                                                                 (make-missile 139 200))) ; missile #1 hit invader #1 = return missile #2 & #3
(check-expect (remove-missiles (list (make-missile 138 200)
                                     (make-missile 139 200)
                                     (make-missile 150 200))(list (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED)
                                                                  (make-invader 150 200 INVADER-X-SPEED))) (list (make-missile 138 200)
                                                                                                                 (make-missile 139 200))) ; missile #3 hit invader #3 = return missile #1 & #2
(check-expect (remove-missiles (list (make-missile 138 200)
                                     (make-missile 150 200)
                                     (make-missile 139 200))(list (make-invader 100 200 INVADER-X-SPEED)
                                                                  (make-invader 150 200 INVADER-X-SPEED)
                                                                  (make-invader 101 200 INVADER-X-SPEED))) (list (make-missile 138 200)
                                                                                                                 (make-missile 139 200))) ; missile #2 hit invader #2 = return missile #1 & #3

;(define (remove-missiles lom loi) lom) ; stub

#;
(define (fn-for-lom lom)
  (cond [(empty? lom)(...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom     (rest lom)))]))


(define (remove-missiles lom loi)
  (cond [(empty? loi) lom]
        [(empty? lom) empty]
        [else (if (hit-missile? (first lom) loi)
              (remove-missiles (rest lom)(rest loi))
              (cons (first lom)(remove-missiles (rest lom) loi)))]))



;; ++ hit-invader?

;; -- for: remove-invader

;; ListOfMissile Invader -> Boolean
;; produce true if the following is true for ANY of the Missiles in ListOfMissile:
;; - the Missile's x-coordinate falls within the Invader's x-coordinate +/- HIT-RANGE
;; AND
;; - the Missile's y-coordinate falls within the Invader's y-coordinate +/- HIT-RANGE

(check-expect (hit-invader? empty (make-invader 150 200 INVADER-X-SPEED)) false) ; no missiles to match against

(check-expect (hit-invader? (list (make-missile 139 200)) (make-invader 150 200 INVADER-X-SPEED)) false) ; missile-x is out of range of invader-x (low)
(check-expect (hit-invader? (list (make-missile 161 200)) (make-invader 150 200 INVADER-X-SPEED)) false) ; missile-x is out of range of invader-x (high)

(check-expect (hit-invader? (list (make-missile 150 189)) (make-invader 150 200 INVADER-X-SPEED)) false) ; missile-y is out of range of invader-y (low)
(check-expect (hit-invader? (list (make-missile 150 211)) (make-invader 150 200 INVADER-X-SPEED)) false) ; missile-y is out of range of invader-y (high)

(check-expect (hit-invader? (list (make-missile 150 200)
                                  (make-missile 100 200)
                                  (make-missile 101 200)) (make-invader 150 200 INVADER-X-SPEED)) true) ; hit @ Missile #1

(check-expect (hit-invader? (list (make-missile 100 200)
                                  (make-missile 150 200)
                                  (make-missile 101 200)) (make-invader 150 200 INVADER-X-SPEED)) true) ; hit @ Missile #2

(check-expect (hit-invader? (list (make-missile 100 200)
                                  (make-missile 101 200)
                                  (make-missile 150 200)) (make-invader 150 200 INVADER-X-SPEED)) true) ; hit @ Missile #3

;(define (hit-invader? lom i) false) ;stub

#;
(define (fn-for-lom lom)
  (cond [(empty? lom)(...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom     (rest lom)))]))

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define (hit-invader? lom i)
  (cond [(empty? lom) false]
        [(hit? (first lom) i) true]
        [else
         (hit-invader? (rest lom) i)]))



;; ++ hit-missile?

;; -- for: remove-missile

;; Missile ListOfInvader -> Boolean
;; produce true if the following is true for ANY of the Invaders in ListOfInvader:
;; - the Missile's x-coordinate falls within the Invader's x-coordinate +/- HIT-RANGE
;; AND
;; - the Missile's y-coordinate falls within the Invader's y-coordinate +/- HIT-RANGE

(check-expect (hit-missile? (make-missile 130 200) empty) false) ; no invaders to match against

(check-expect (hit-missile? (make-missile 150 200) (list (make-invader 139 200 INVADER-X-SPEED))) false) ; missile-x is out of range of invader-x (low)
(check-expect (hit-missile? (make-missile 150 200) (list (make-invader 161 200 INVADER-X-SPEED))) false) ; missile-x is out of range of invader-x (high)

(check-expect (hit-missile? (make-missile 150 200) (list (make-invader 150 189 INVADER-X-SPEED))) false) ; missile-y is out of range of invader-y (low)
(check-expect (hit-missile? (make-missile 150 200) (list (make-invader 150 211 INVADER-X-SPEED))) false) ; missile-y is out of range of invader-y (high)

(check-expect (hit-missile? (make-missile 150 200) (list (make-invader 150 200 INVADER-X-SPEED)
                                                         (make-invader 100 200 INVADER-X-SPEED)
                                                         (make-invader 101 200 INVADER-X-SPEED))) true) ; hit @ Invader #1

(check-expect (hit-missile? (make-missile 150 200) (list (make-invader 100 200 INVADER-X-SPEED)
                                                         (make-invader 150 200 INVADER-X-SPEED)
                                                         (make-invader 101 200 INVADER-X-SPEED))) true) ; hit @ Invader #2

(check-expect (hit-missile? (make-missile 150 200) (list (make-invader 100 200 INVADER-X-SPEED)
                                                         (make-invader 101 200 INVADER-X-SPEED)
                                                         (make-invader 150 200 INVADER-X-SPEED))) true) ; hit @ Invader #3

;(define (hit-missile? m loi) false) ;stub 

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi)(...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi     (rest loi)))]))

(define (hit-missile? m loi)
  (cond [(empty? loi) false]
        [(hit? m (first loi)) true]
        [else
         (hit-missile? m (rest loi))]))



;; ++ hit?

;; -- for: hit-invader? AND hit-missile?

;; Missile Invader -> Boolean
;; produce true if:
;; - the Missile's x-coordinate falls within the Invader's x-coordinate +/- HIT-RANGE
;; AND
;; - the Missile's y-coordinate falls within the Invader's y-coordinate +/- HIT-RANGE

(check-expect (hit? (make-missile 140 190)(make-invader 150 200 INVADER-X-SPEED)) true)  ; Missile x/y is within Invader x/y + HIT-RANGE (low band)
(check-expect (hit? (make-missile 150 200)(make-invader 150 200 INVADER-X-SPEED)) true)  ; Missile x/y is within Invader x/y + HIT-RANGE (mid band)
(check-expect (hit? (make-missile 160 210)(make-invader 150 200 INVADER-X-SPEED)) true)  ; Missile x/y is within Invader x/y + HIT-RANGE (top band)

(check-expect (hit? (make-missile 139 200)(make-invader 150 200 INVADER-X-SPEED)) false) ; Missile x is too low
(check-expect (hit? (make-missile 161 200)(make-invader 150 200 INVADER-X-SPEED)) false) ; Missile x is too high

(check-expect (hit? (make-missile 150 189)(make-invader 150 200 INVADER-X-SPEED)) false) ; Missile y is too low
(check-expect (hit? (make-missile 150 211)(make-invader 150 200 INVADER-X-SPEED)) false) ; Missile y is too high

;(define (hit-invader? m i) false) ;stub

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

(define (hit? m i)
  (and
   (and
    (<= (missile-x m) (+ (invader-x i) HIT-RANGE))  
    (>= (missile-x m) (- (invader-x i) HIT-RANGE)))  
   (and
    (<= (missile-y m) (+ (invader-y i) HIT-RANGE))  
    (>= (missile-y m) (- (invader-y i) HIT-RANGE)))))             



;; ++ render-tank

;; -- for: render-missile

;; Tank -> Image
;; render the current Tank on the MTS

(check-expect (render-tank T0) (place-image TANK (/ WIDTH 2) TANK-Y MTS))
(check-expect (render-tank T1) (place-image TANK 50 TANK-Y MTS))

;(define (render-tank t) empty-image) ;stub

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y MTS))



;; ++ render-missiles

;; -- for: render-invader

;; ListOfMissile Image -> Image
;; render ListOfMissile on the Image provided by render-tank

(check-expect (render-missiles empty (render-tank T0))                                                        (place-image TANK (/ WIDTH 2) TANK-Y MTS))
(check-expect (render-missiles (list (make-missile 10 300)) (render-tank T0))                                 (place-image MISSILE 10 300 (place-image TANK (/ WIDTH 2) TANK-Y MTS)))
(check-expect (render-missiles (list (make-missile 130 MISSILE-START)(make-missile 10 300)) (render-tank T0)) (place-image MISSILE 130 MISSILE-START (place-image MISSILE 10 300 (place-image TANK (/ WIDTH 2) TANK-Y MTS))))

;(define (render-missile lom) empty-image) ;stub

#;
(define (fn-for-lom lom)
  (cond [(empty? lom)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom     (rest lom)))]))

(define (render-missiles lom i)
  (cond [(empty? lom) i]
        [else
         (place-image MISSILE (missile-x (first lom))(missile-y (first lom))(render-missiles (rest lom) i))]))



;; ++ render-invaders

;; -- for: render-game

;; ListOfInvader Image -> Image
;; render ListofInvader on the Image provided by render-missile

(check-expect (render-invaders empty (render-missiles empty (render-tank T0)))
                (place-image TANK (/ WIDTH 2) TANK-Y MTS))
(check-expect (render-invaders (list (make-invader 100 50 10)) (render-missiles (list (make-missile 10 300)) (render-tank T0)))
                (place-image INVADER 100 50 (place-image MISSILE 10 300 (place-image TANK (/ WIDTH 2) TANK-Y MTS))))
(check-expect (render-invaders (list (make-invader 200 150 10)(make-invader 100 50 10)) (render-missiles (list (make-missile 130 MISSILE-START)(make-missile 10 300)) (render-tank T0)))
                (place-image INVADER 200 150 (place-image INVADER 100 50 (place-image MISSILE 130 MISSILE-START (place-image MISSILE 10 300 (place-image TANK (/ WIDTH 2) TANK-Y MTS))))))

;(define (render-invader loi) empty-image) ;stub

#;
(define (fn-for-loi loi)
  (cond [(empty? loi)(...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi     (rest loi)))]))

(define (render-invaders loi i)
  (cond [(empty? loi) i]
        [else
         (place-image INVADER (invader-x (first loi))(invader-y (first loi))(render-invaders (rest loi) i))]))



;; ++ hit-bottom?

;; -- for: stop-game

;; ListOfInvader -> Boolean
;; produce true if any Invader in the ListOfInvader has a y >= HEIGHT

(check-expect (hit-bottom? empty)                                                              false)
(check-expect (hit-bottom? (list (make-invader 100 150 -1) (make-invader 200 100 1)))          false)
(check-expect (hit-bottom? (list (make-invader 100 (- HEIGHT 1) -1) (make-invader 200 100 1))) false)
(check-expect (hit-bottom? (list (make-invader 100 150 -1) (make-invader 200 (- HEIGHT 1) 1))) false)
(check-expect (hit-bottom? (list (make-invader 100 HEIGHT -1) (make-invader 200 100 1)))        true)
(check-expect (hit-bottom? (list (make-invader 100 150 -1) (make-invader 200 HEIGHT 1)))        true)

;(define (hit-bottom? loi) false) ;stub

#;
(define (fn-for-loi loi)
  (cond [(empty? loi)(...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi     (rest loi)))]))


(define (hit-bottom? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (hit-bottom?  (rest loi)))]))



;; ++ move-tank

;; -- for: control-tank

;; Tank String -> Tank
;; produce a right-moving tank when "right" is given for the second argument
;; product a left-moving tank when "left" is given for the second argument
;; if the tank is at the left-most or right-most edge of the screen, cannot move beyond.
;; ASSUME: String will always be either "right" or "left"

(check-expect (move-tank (make-tank (/ WIDTH 2) 1) "right")     (make-tank (+ (/ WIDTH 2) TANK-SPEED)  1))
(check-expect (move-tank (make-tank (/ WIDTH 2) 1) "left")      (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (move-tank (make-tank 0 -1) "left")               (make-tank 0 -1))
(check-expect (move-tank (make-tank 0 -1) "right")              (make-tank (+ 0 TANK-SPEED) 1))
(check-expect (move-tank (make-tank WIDTH 1) "left")            (make-tank (- WIDTH TANK-SPEED) -1))
(check-expect (move-tank (make-tank WIDTH 1) "right")           (make-tank WIDTH 1))

;(define (move-tank t s) t) ;stub

(define (move-tank t s)
  (cond [(and (string=? s "right")(<= (+ (tank-x t) TANK-SPEED) WIDTH)) (make-tank (+ (tank-x t) TANK-SPEED)  1)]
        [(and (string=? s "left") (>= (- (tank-x t) TANK-SPEED) 0))     (make-tank (- (tank-x t) TANK-SPEED) -1)]
        [else t]))



;; ++ shoot-missile

;; -- for: control-tank

;; ListOfMissile Natural -> ListOfMissile
;; produce a ListofMissile with a new missile entry added that matches the x-coordinate (Natural) of the tank at time of creation

(check-expect (shoot-missile empty 10)                         (list (make-missile 10 MISSILE-START)))
(check-expect (shoot-missile (list (make-missile 10 300)) 130) (list (make-missile 130 MISSILE-START)(make-missile 10 300)))

;(define (shoot-missile lom n) lom) ;stub

#;
(define (fn-for-lom lom x)
  (cond [(empty? lom)(... x)]
        [else
         (... x (fn-for-missile (first lom))
                (fn-for-lom     (rest lom)))]))

(define (shoot-missile lom x)
  (cond [(empty? lom)(list (make-missile x MISSILE-START))]
        [else
         (cons (make-missile x MISSILE-START) lom)]))