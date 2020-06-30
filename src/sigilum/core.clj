(ns sigilum.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def width 500)
(def height 500)
(def center-x (/ width 2))
(def center-y (/ height 2))
(def center-coord [center-x center-y])
(def τ (* 2 Math/PI))
(def x first)
(def y last)

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  {:angle 0})

(defn update [state]
  {:angle (+ (:angle state) 0.01)})

(defn gen-angles
  ([num-points]
   (gen-angles num-points + 0))
  ([num-points orientation]
   (gen-angles num-points orientation 0))
  ([num-points orientation offset]
   (let [half-pi (orientation (/ Math/PI 2))]
     (range (+ half-pi offset) (+ τ half-pi offset) (/ τ num-points)))))

(defn coord-from-angle [center radius angle]
  (vector (+ (* radius (Math/cos angle)) (x center))
          (+ (* radius (Math/sin angle)) (y center))))

(defmulti generate-line-list (fn [coords skip] (class skip)))

(defmethod generate-line-list java.lang.Long [coords skip]
  (let [num-points (count coords)]
    (take num-points
          (map flatten
               (partition 2 1
                          (map last
                               (filter
                                #(= 0 (mod (first %) skip))
                                (keep-indexed vector (cycle coords)))))))))

(defmethod generate-line-list clojure.lang.Fn [coords skip]
  ;; TODO: implement
  )

(defn gen-points [angles center radius]
  (map (partial coord-from-angle center radius) angles))

(defn draw-gram
  ([num-points skip center radius]
   (draw-gram num-points skip center radius + 0))
  ([num-points skip center radius orientation]
   (draw-gram num-points skip center radius orientation 0))
  ([num-points skip center radius orientation offset]
   (doall
    (map (partial apply q/line) 
         (generate-line-list
          (gen-points (gen-angles num-points orientation offset) center radius)
          skip)))))

(defn text-in-circle [name radius offset]
  (map vector name (gen-points (gen-angles (count name) + offset) center-coord radius)))

(defn draw-name-circle [outer inner font name offset]
  (let [outer-c (* 2 outer)
        inner-c (* 2 inner)
        middle (/ (+ inner outer) 2)]
    (q/ellipse center-x center-y outer-c outer-c)
    (q/ellipse center-x center-y inner-c inner-c)
    (q/text-font (q/create-font font 20 true))
    (q/text-align :center :center)
    (q/fill 255)
    (doall
     (map
      #(q/text (str (first %)) (x (last %)) (y (last %)))
      (text-in-circle name middle offset))
     )))

(defn draw [state]
  (q/background 0)
  (q/fill 0 0 0)
  (q/stroke-weight 6)
  (q/stroke 255)
  (let [outer 250
        inner (* 0.85 outer)]
    (draw-name-circle outer inner "Arial" "o shit waddup * " (:angle state))
    (draw-gram 7 3 center-coord inner -)
    (draw-gram 5 2 [200 120] (- inner 150) + (- (:angle state)))
    )
  
  )


(q/defsketch sigilum
  :title "Hail"
  :size [500 500]
  ;; setup function called only once, during sketch initialization.
  :setup setup
  ;; update-state is called on each iteration before draw-state.
  :update update
  :draw draw
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode]
  )
