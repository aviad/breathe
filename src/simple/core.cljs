(ns simple.core
  (:require [reagent.dom]
            [re-frame.core :as rf]
            [clojure.string :as str]
            ["vega-embed" :as vega]))

;; A detailed walk-through of this source code is provided in the docs:
;; https://day8.github.io/re-frame/dominoes-live/

;; -- Domino 1 - Event Dispatch -----------------------------------------------

(defn dispatch-timer-event
  []
  (rf/dispatch [:step]))  ;; <-- dispatch used

;; Call the dispatching function every second.
;; `defonce` is like `def` but it ensures only one instance is ever
;; created in the face of figwheel hot-reloading of this file.
(defonce do-timer (js/setInterval dispatch-timer-event 1500))


;; -- Domino 2 - Event Handlers -----------------------------------------------


(rf/reg-event-db              ;; sets up initial application state
 :initialize                 ;; usage:  (dispatch [:initialize])
 (fn [_ _]                   ;; the two parameters are not important here, so use _
   {:step 0        ;; What it returns becomes the new application state
    :people (assoc (into {} (for [x (range 10) y (range 10)] [[x y] (rand-int 100)]))
                   [11 9] 0
                   [11 0] 100)}))    ;; so the application state will initially be a map with two keys


(declare simulation-step) ;; FIXME: move

(rf/reg-event-db                 ;; usage:  (dispatch [:timer])
 :step                           ;; every second an event of this kind will be dispatched
 (fn [db [_ _]]                  ;; note how the 2nd parameter is destructured to obtain the data value
  ;; (prn "stepping")
   (let [step (inc (:step db))
          ;; This is the point where the simulation does another step and people are updated
         people (simulation-step (:people db))]
     (assoc db :step step :people people))))       ;; compute and return the new application state


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
 :step
 (fn [db _]     ;; db is current app state. 2nd unused param is query vector
   (:step db))) ;; return a query computation over the application state


(rf/reg-sub
 :people
 (fn [db _]     ;; db is current app state. 2nd unused param is query vector
   (:people db))) ;; return a query computation over the application state

;; -- Domino 5 - View Functions ----------------------------------------------
(declare sim-plot) ;; FIXME: remove
(declare sim-plotf) ;; FIXME: remove

(defn simulation
  []
  [:div
   [:div#simulation]
   [:div#hidden (do (vega/embed "#simulation"
;;				sim-plot
                                (sim-plotf @(rf/subscribe [:people])))
                    (str "step " @(rf/subscribe [:step])))]])

(defn ui
  []
  [:div
   [:h1 "Breathe"]
   [simulation]])

;; -- Entry Point -------------------------------------------------------------

(defn render
  []
  (reagent.dom/render [ui]
                      (js/document.getElementById "app")))

(defn ^:dev/after-load clear-cache-and-render!
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code. We force a UI update by clearing
  ;; the Reframe subscription cache.
  (rf/clear-subscription-cache!)
  (render))

(defn run
  []
  (rf/dispatch-sync [:initialize]) ;; put a value into application state
  (render)                         ;; mount the application's ui into '<div id="app" />'
  )

;; -- FIXME: MOVE! ----------------------------------------------------------- 

;; (def sim-plot
;;   (clj->js 
;;     {:$schema
;;      "https://vega.github.io/schema/vega-lite/v4.json",
;;      :data
;;      {:values (vec (for [x (range 10) y (range 10)] {:x x :y y :z (rand-int 100)}))}
;;      :mark "rect",
;;      :encoding
;;      {:y {:field "y", :type "ordinal"},
;;       :x {:field "x", :type "ordinal"},
;;       :color {:field "z", :type "quantitative"}},
;;      :config
;;      {:axis {:grid true, :tickBand "extent"},
;;       :range {:heatmap {:scheme "viridis"}}
;;      }})) 


(defn sim-plotf
  [people]
  ;;(prn :people people)
  (clj->js
   {:$schema
    "https://vega.github.io/schema/vega-lite/v4.json",
    :data
    {:values (for [[[x y] z] (seq people)] {:x x :y y :z z})},
    :mark "rect",
    :encoding
    {:y {:field "y", :type "ordinal"},
     :x {:field "x", :type "ordinal"},
     :color {:field "z", :type "quantitative", :title "Phase shift"}},
    :config
    {:axis {:grid true, :tickBand "extent" :ticks false :labels false :title nil},
     :range {:heatmap {:scheme  #_"plasma" "lighttealblue"}},
      ;;:legend {:title "Phase shift"}
     }}))

(defn neighbors
  [person]
  (vec (for [x (range (dec (first person)) (+ 2 (first person)))
             y (range (dec (second person)) (+ 2 (second person)))
             :when (and (<= 0 x 9) (<= 0 y 9) (or (not= x (first person)) (not= y (second person))))]
         [x y])))

(defn simulation-step
  [people]
 ;; (prn :initial people)
  (loop [to-sync (shuffle (filter #(<= (first %) 9) (keys people))) people people]
   ;; (prn :entry-to-loop-ppl people)
   ;; (prn :entry-to-loop-to-sync to-sync)
    (if (empty? to-sync)
      (do (prn :simulation-step people) people)
      (let [person (first to-sync)
            neighbor (rand-nth (neighbors person))
;;             _ (when-not (sequential? person) (prn "======================= :person " person " ======================"))
;;             _ (when-not (sequential? neighbor) (prn "======================= :neighbor " neighbor :neighbors (neighbors person) :person person " ======================"))
            new-z (Math/floor (/ (+ (people person) (people neighbor)) 2))]
        (recur (rest to-sync) (assoc people person new-z neighbor new-z))))))

