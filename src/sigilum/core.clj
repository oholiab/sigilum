(ns sigilum.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [gil.core :as g]))

(def width 800)
(def height 800)
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
     (take num-points (range (+ half-pi offset) (+ τ half-pi offset) (/ τ num-points))))))

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

(defn draw-our-cross [height width center-coords]
  (let [thickness (/ width 10)
        c-x (x center-coords)
        c-y (y center-coords)
        vertical-bar-y (- c-y (/ height 2))
        vertical-bar-x (- c-x (/ thickness 2))
        long-horizontal-bar-width (/ width 1.5) 
        long-horizontal-bar-x (- c-x (/ long-horizontal-bar-width 2))
        long-horizontal-bar-y (+ vertical-bar-y (* height 0.7))
        short-horizontal-bar-width (/ long-horizontal-bar-width 2) 
        short-horizontal-bar-x (- c-x (/ short-horizontal-bar-width 2))
        short-horizontal-bar-y (+ vertical-bar-y (* height 0.55))
        ]
    (q/fill 255)
    (q/rect vertical-bar-x vertical-bar-y thickness height)
    (q/rect long-horizontal-bar-x long-horizontal-bar-y long-horizontal-bar-width thickness)
    (q/rect short-horizontal-bar-x short-horizontal-bar-y short-horizontal-bar-width (* thickness 0.7))
    ))

(defn draw-name-circle [outer inner font name offset]
  (let [outer-c (* 2 outer)
        inner-c (* 2 inner)
        middle (/ (+ inner outer) 2)]
    (q/ellipse center-x center-y outer-c outer-c)
    (q/ellipse center-x center-y inner-c inner-c)
    (q/text-font (q/create-font font 40 true))
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
  (let [outer (/ width 2)
        inner (* 0.85 outer)]
    (draw-name-circle outer inner "Copperplate" "International House of Brütals * " (- (:angle state)))
    (draw-gram 7 3 center-coord inner - (:angle state))
    #_(draw-gram 5 2 [200 120] (- inner 150) + (- (:angle state)))
    (let [height (* 2 inner)]
      (draw-our-cross height height center-coord))
    )
  #_(g/save-animation "brutals.gif" 200 10)
  (q/text (str "angle: " (:angle state)) 10 20)
  )


(q/defsketch sigilum
  :title "Hail"
  :size [width height]
  :setup setup
  :update update
  :draw draw
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode]
  :settings #(.pixelDensity (quil.applet/current-applet) (.displayDensity (quil.applet/current-applet))))
