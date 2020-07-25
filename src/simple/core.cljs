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
(defonce do-timer (js/setInterval dispatch-timer-event 3000))


;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db              ;; sets up initial application state
 :initialize                 ;; usage:  (dispatch [:initialize])
 (fn [_ _]                   ;; the two parameters are not important here, so use _
   {:step 0        ;; What it returns becomes the new application state
    :people (assoc (into {} (for [x (range 10) y (range 10)] [[x y] (rand-int 100)]))
                   [11 9] 0
                   [11 0] 100)}
    ))    ;; so the application state will initially be a map with two keys


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
                    (str "step " @(rf/subscribe [:step])))]
                     ])

(defn ui
  []
  [:div
   [:h1 "Breathe"]
   [simulation]
])

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

(def sim-plot
  (clj->js 
    {:$schema
     "https://vega.github.io/schema/vega-lite/v4.json",
     :data
     {:values (vec (for [x (range 10) y (range 10)] {:x x :y y :z (rand-int 100)}))}
     :mark "rect",
     :encoding
     {:y {:field "y", :type "ordinal"},
      :x {:field "x", :type "ordinal"},
      :color {:field "z", :type "quantitative"}},
     :config
     {:axis {:grid true, :tickBand "extent"},
      :range {:heatmap {:scheme "magma"}}
     }})) 


(defn sim-plotf
 [people]
  ;;(prn :people people)
  (clj->js 
    {:$schema
     "https://vega.github.io/schema/vega-lite/v4.json",
     :data
     {:values people}
     :mark "rect",
     :encoding
     {:y {:field "y", :type "ordinal"},
      :x {:field "x", :type "ordinal"},
      :color {:field "z", :type "quantitative" :max 200 :scale {:rangeMax 200}}},
     :config
     {:axis {:grid true, :tickBand "extent"},
      :range {:heatmap {:scheme "magma" :count 100 ;; :extent [0, 100]
}}
;;      :background "#99ccff"
     }})) 

(defn new-z
 [person people]
 (let [neighbors (for [x (range (dec (:x person)) (inc (:x person)))
                       y (range (dec (:y person)) (inc (:y person)))
                       :when (and (<= 0 9 x) (<= 0 9 y) (not= x (:x person)) (not= y (:y person)))]

)

(defn simulation-step
 [people]
does not work any more - need to iterate on the map
 (for [person people] (assoc person :z (if (= (:x person) 11) (:z person) (new-z person people))))
)

