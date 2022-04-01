(use freja/flow)

(def state @{})
(def gos @[])
(def game-size [460 600])
(def ball @{})
(def paddle @{})

(defn ball-update
  [self]
  (update self :pos v/v+
          (v/v* (self :vel) (get-frame-time)))

  (var collided false)

  (loop [go :in gos
         :when (not= self go)
         :let [{:pos p :size s} go]
         :when (check-collision-circle-rec (self :pos)
                                           (self :radius)
                                           [;p ;s])]
    (when (go :break)
      (:break go self))

    (set collided true))

  (let [{:pos p
         :radius r} self
        [x y] p]
    (when (<= (- y r) 0)
      (set collided true))
    
    (when (<= (- x r) 0)
      (set collided true))
    
    (when (>= (- x r) (game-size 0))
      (set collided true))

    (when (> (+ y r) (game-size 1))
      (put state :lost true)))

  (when collided
    (update self :vel v/v* -1.05)
    (update-in self [:vel 0] + (-> (- (* 2 (math/random)) 1) (* 100)))
    (update self :pos v/v+ (v/v* (self :vel) (get-frame-time)))))

(defn ball-render
  [{:pos pos
    :radius radius}]
  (draw-circle-v pos radius [0.7 0.3 0.7]))

(defn paddle-render
  [{:pos pos :size size}]
  (draw-rectangle ;(map math/floor pos) ;size :blue))

(defn paddle-update
  [self]
  (update self :vel v/v+
          (v/v* (self :in)
                (* (self :speed)
                   (get-frame-time))))

  (update-in self [:vel 0]
             |(max (- (self :max-speed))
                   (min (self :max-speed) $)))

  (when (zero? (get-in self [:in 0]))
    (put-in self [:vel 0] 0))

  (update self :pos v/v+ (v/v* (self :vel) (get-frame-time))))

(defn render-tile
  [{:pos pos
    :size size}]
  (draw-rectangle ;pos ;size :red))

(defn break-tile
  [self breaker]
  (put self :remove true))

(defn noop [& args])

(defn new-tile
  [pos size]
  @{:pos pos
    :size size
    :break break-tile
    :update noop
    :render render-tile})

(defn create-grid
  [gos]
  (let [padding [50 10 400 10]
        [w h] game-size
        inner-size [(- w (padding 1) (padding 3))
                    (- h (padding 0) (padding 2))]
        spacing [3 3]
        tiles-per-axis [10 12]
        tile-size (-> (map (fn [v1 v2]
                             (/ v1 v2))
                           inner-size
                           tiles-per-axis)
                      (v/v- spacing))
        tile-size (map math/floor tile-size)]
    (loop [y :range [0 (tiles-per-axis 1)]
           x :range [0 (tiles-per-axis 0)]
           :let [pos [(-> (* x (+ (tile-size 0) (spacing 0)))
                          (+ (padding 3))
                          math/floor)
                      (-> (* y (+ (tile-size 1)
                                  (spacing 1)))
                          (+ (padding 0))
                          math/floor)]]]
      (array/push gos (new-tile pos tile-size)))))

(defn render-ui
  []
  (when (state :lost)
    (draw-rectangle 0 0 ;game-size [0.1 0.1 0.1])
    (draw-text "You lost! :(\nPress R to restart."
               (v/v* game-size 0.5)
               :center true
               :color :white))

  (when (state :won)
    (draw-rectangle 0 0 ;game-size [0.9 0.9 0.9])
    (draw-text "You won! :)\nPress R to restart."
               (v/v* game-size 0.5)
               :center true
               :color :black)))

(defn init
  []
  (table/clear state)
  (table/clear ball)
  (merge-into ball
              {:update ball-update
               :render ball-render
               :radius 15
               :vel @[0 -300]
               :pos @[;(v/v* game-size 0.5)]})
  (table/clear paddle)
  (merge-into paddle
              {:update paddle-update
               :render paddle-render
               :size [100 20]
               :vel @[0 0]
               :speed 7500
               :max-speed 900
               :in @[0 0]
               :pos @[(-> (* 0.5 (game-size 0))
                          (- 50))
                      (- (game-size 1)
                         40)]})
  (put state :lost false)
  (put state :won false)
  (array/clear gos)
  (array/push gos ball)
  (array/push gos paddle)
  (create-grid gos))

(defn render
  [el]
  (try
    (do (clear-background [0.2 0.2 0.2 1])
      (unless (or (state :lost)
                  (state :won))
        (loop [go :in gos]
          (:update go))

        (when (= 2 (length gos))
          (put state :won true))

        (var i 0)
        (while (< i (length gos))
          (if ((in gos i) :remove)
            (array/remove gos i)
            (++ i)))

        (loop [go :in gos]
          (:render go)))

      (render-ui))
    ([err fib]
      (debug/stacktrace fib err ""))))

(defn on-event
  [_ ev]
  (match ev
    {:key/down :r}
    (init)

    {:key/down :a}
    (update-in paddle [:in 0] dec)

    {:key/down :d}
    (update-in paddle [:in 0] inc)

    {:key/release :a}
    (update-in paddle [:in 0] inc)

    {:key/release :d}
    (update-in paddle [:in 0] dec)))

(start-game {:render render
             :on-event on-event
             :size game-size
             :border [0.05 0.05 0.05]})

(when (dyn :freja/loading-file)
  (init))
