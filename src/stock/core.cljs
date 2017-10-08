(ns stock.core
    (:require
      [reagent.core :as r]
      [net.cgrand.xforms :as xf]))

(let [next-g (atom nil)]
  (defn next-gaussian []
    (if-let [g @next-g]
      (do (reset! next-g nil) g)
      (loop []
        (let [v1 (- (* 2 (Math/random)) 1)
              v2 (- (* 2 (Math/random)) 1)
              s (+ (* v1 v1) (* v2 v2))]
          (if (or (>= s 1) (zero? s))
            (recur)
            (let [mult (Math/sqrt (* -2 (/ (Math/log s) s)))]
              (reset! next-g (* mult v2))
              (* mult v1))))))))

(defn gaussian [mean stdev]
  #(+ mean (* stdev (next-gaussian))))

(defn gaussian-density [mean stdev]
  (let [double-variance (* 2 stdev stdev)
        coeff (/ (Math/sqrt (* Math/PI double-variance)))]
    #(* coeff (Math/exp (- (/ (Math/pow (- % mean) 2) double-variance))))))

(defn power-law [[x0 x1] n]
  (fn []
    (let [n+1 (inc n)
          x0-to-n+1 (Math/pow x0 n+1)]
      (-> x1
        (Math/pow n+1)
        (- x0-to-n+1)
        (* (Math/random))
        (+ x0-to-n+1)
        (Math/pow (/ n+1))))))

(defn clamp
  ([low x]
   (max
     (if (nil? low) -Infinity low)
     x))
  ([low high x]
   (max
     (if (nil? low) -Infinity low)
     (min
       (if (nil? high) Infinity high)
       x))))

(defn linear [[x1 x2] [y1 y2]]
  (let [m (/ (- y2 y1) (- x2 x1))
        b (- y1 (* m x1))]
    #(+ b (* m %))))

(def extent (juxt #(apply min %) #(apply max %)))

(defn round [x d]
  (* (Math/round (/ x d)) d))

(defn round-to-largest-place [x]
  (let [m (Math/pow 10 (Math/floor (Math/log10 (Math/abs x))))]
    (* m (Math/floor (/ x m)))))

(defn rand-by-frequency [probs]
  (let [xs (map first probs)
        total (reduce + xs)
        r (Math/random)]
    (loop [probs probs
           freqs (drop 1 (reductions #(+ %1 (/ %2 total)) 0 xs))]
      (if (<= r (first freqs))
        (second (first probs))
        (recur (rest probs) (rest freqs))))))

(defn path [pts]
  (transduce
    (comp
      (map (fn [[x y]] (str x "," y)))
      (interpose "L"))
    str "M" pts))

(defn fmt-currency [x]
  (when x
    (.toLocaleString x "en-US" #js {:style "currency" :currency "USD"})))

(defn fmt-percent [x]
  (str (.toFixed (round (* 100 x) 0.1) 1) "%"))

(defn factor->gaussian [{:keys [aggressive probable]}]
  (let [mean (js/parseFloat aggressive 10)
        stdev (/ (Math/abs (- mean (js/parseFloat probable 10))) 1.282)]
    [mean stdev]))

(defn factor->generator [{t :type minimum :min :as factor}]
  (condp = t
    :gaussian (apply gaussian (factor->gaussian factor))
    :power-law (power-law ((juxt :min :max) factor) -3)
    :select-one #(rand-by-frequency (factor :choices))
    (throw (js/Exception. "Unsupported distribution type."))))

(defn sample [factors generators]
  (let [base (into {}
                   (map (fn [[k generator]]
                          [k (generator)]))
                   generators)]
    (into {}
          (map (fn [[k s]]
                 (let [{minimum :min maximum :max} (get factors k)
                       minimum (if (keyword? minimum)
                                 (get base minimum)
                                 minimum)
                       maximum (if (keyword? maximum)
                                 (get base maximum)
                                 maximum)]
                   [k (clamp minimum maximum s)])))
          base)))

(defn salary [{:keys [starting-salary raise-percent raises-per-year annual-bonuses deductions years] :as factors}]
  (if (pos? years)
    (let [years-per-raise (/ raises-per-year)]
      (-> starting-salary
        (- deductions)
        (* (min years years-per-raise))
        (+ (salary (-> factors
                     (update :starting-salary * (+ 1 (/ raise-percent 100)))
                     (update :years - years-per-raise)
                     (assoc :annual-bonuses 0))))
        (+ (* years annual-bonuses))))
    0))

(defn options-cost [{:keys [options-purchased strike-price exercise-value capital-gains-tax-rate]}]
  (let [purchase-cost (* options-purchased strike-price)
        stock-value (* options-purchased exercise-value)
        capital-gain (- stock-value purchase-cost)]
    (+ purchase-cost (* capital-gain capital-gains-tax-rate))))

(defn stock-value [{:keys [exit investor-payback common-shares options-purchased]}]
  (-> exit
    (- investor-payback)
    (/ common-shares)
    (* options-purchased)))

(defn calculate [{:keys [years] :as factors}]
  (if (pos? years)
    (-> (salary factors)
      (- (options-cost factors))
      (+ (stock-value factors))
      (/ years))
    0))

(defn start-simulation [state]
  (let [{:keys [factors]} @state
        generators (into {}
                         (map (fn [[k factor]]
                                [k (factor->generator factor)]))
                         factors)]
    (swap! state assoc :running? true)
    (letfn [(simulate []
              (when (@state :running?)
                (swap! state
                       (fn [s]
                         (let [sim (sample factors generators)
                               result (calculate sim)]
                           (-> s
                             (update :simulations conj (assoc sim :annual-value result))
                             (update :results conj result)))))
                (js/setTimeout simulate 0))
              0)]
      (js/setTimeout simulate 0))))

(defn stop-simulation [state]
  (swap! state assoc :running? false))

(defn reset-simulation [state]
  (swap! state
         (fn [s]
           (-> s
             (assoc :simulations [])
             (assoc :results [])))))

;; -------------------------
;; Views

(defn input [state path]
  [:input
   {:type :number
    :value (get-in @state path)
    :on-change #(swap! state assoc-in path (.. % -target -value))}])

(defn gaussian-curve [factor]
  (let [w 500, h 50
        [mean stdev] (factor->gaussian factor)
        xs (range (- mean (* 3 stdev)) (+ mean (* 3 stdev)) (/ stdev 10))
        density (map (gaussian-density mean stdev) xs)]
    (when (seq xs)
      [:svg {:width w :height h}
       [:path {:transform (str "translate(" 0 "," h ")" "scale(" (/ w (count xs)) "," (- (/ h (apply max density))) ")")
               :d (path (map-indexed vector density))
               :fill "steelblue"}]])))

(defn controls [state]
  [:div
   [:button
    {:disabled (@state :running?)
     :on-click #(start-simulation state)}
    "Start"]
   [:button
    {:disabled (not (@state :running?))
     :on-click #(stop-simulation state)}
    "Stop"]
   [:button
    {:disabled (empty? (@state :results))
     :on-click #(reset-simulation state)}
    "Reset"]
   [:span " Simulations: " (count (@state :results))]])

(defn histogram [kvs]
  (let [w 500, h 100
        [left right] (extent (map first kvs))
        [_ top] (extent (map second kvs))
        x (linear [left right] [10 (- w 10)])
        y (linear [0 top] [(- h 15) 0])
        x0 (x 0), y0 (y 0)
        interval (round-to-largest-place (/ (- right left) 5))]
    (when (pos? interval)
      [:svg {:width w :height h}
       [:defs
        [:path {:id :distribution
                :d (path
                     (concat [[-1 y0]]
                             (map (fn [[k v]]
                                    [(x k) (y v)])
                                  kvs)
                             [[(inc w) y0]]))}]
        [:clipPath {:id :loss} [:rect {:width (max 0 x0) :height h}]]
        [:clipPath {:id :profit} [:rect {:x x0, :width (max 0 (- w x0)), :height h}]]]
       [:use {:xlinkHref "#distribution"
              :fill :firebrick
              :clipPath "url(#loss)"}]
       [:use {:xlinkHref "#distribution"
              :fill :forestgreen
              :clipPath "url(#profit)"}]
       [:g
        (for [d (concat (range (- interval) left (- interval)) (range 0 right interval))
              :let [d' (Math/abs d)]]
          [:text {:key d :x (x d) :y y0, :dy 13, :text-anchor :middle, :fill :gray, :font-size 14}
           "$" (cond
                 (zero? d') "0"
                 (< d' 1e6) (str (/ d' 1000) "k")
                 (< d' 1e9) (str (/ d' 1e6) "M"))])]])))

(defn scenario [{:keys [capital-gains-tax-rate strike-price exit investor-payback starting-salary raises-per-year years options-purchased deductions raise-percent common-shares annual-bonuses exercise-value annual-value] :as factors}]
  [:ul {:className "scenario"}
   [:li [:em "Total salary: "] (fmt-currency (salary factors))]
   [:li [:em "Total options cost: "] (fmt-currency (options-cost factors))]
   [:li [:em "Total stock value: "] (fmt-currency (stock-value factors))]
   [:li [:em "Starting salary: "] (fmt-currency starting-salary)]
   [:li [:em "Raises per year: "] (.toFixed (round raises-per-year 0.1) 1)]
   [:li [:em "Size of raises: "] (fmt-percent raise-percent)]
   [:li [:em "Annual bonuses: "] (fmt-currency annual-bonuses)]
   [:li [:em "Annual deductions: "] (fmt-currency deductions)]
   [:li [:em "Years: "] (.toFixed years 1)]
   [:li [:em "Options purchased: "] (Math/round options-purchased) " shares"]
   [:li [:em "Strike price: "] (fmt-currency strike-price)]
   [:li [:em "Exercise value: "] (fmt-currency exercise-value)]
   [:li [:em "Capital gains tax rate: "] (fmt-percent capital-gains-tax-rate)]
   [:li [:em "Exit value: "] (fmt-currency exit)]
   [:li [:em "Investor payback: "] (fmt-currency investor-payback)]
   [:li [:em "Common shares: "] (Math/round common-shares) " shares"]])

(defn results [sims totals]
  (when (seq totals)
    (let [c (count totals)
          interval 5000
          kvs (sort-by first
                       (sequence
                         (comp
                           (xf/by-key #(* interval (Math/floor (/ % interval))) xf/count)
                           (map (fn [[k v]] [k (/ v c)])))
                         totals))
          likely (apply max-key second kvs)
          best (apply max-key :annual-value sims)
          worst (apply min-key :annual-value sims)]
      [:div
       [histogram kvs]
       [:div
        [:strong "Most likely: "]
        (fmt-currency (first likely)) " per year"
        " (" (fmt-percent (second likely)) " chance)"]
       [:div {:className "scenarios"}
        [:div
         [:strong "Best-case: "]
         (fmt-currency (:annual-value best)) " per year"
         [scenario best]]
        [:div
         [:strong "Worst-case: "]
         (fmt-currency (:annual-value worst)) " per year"
         [scenario worst]]]])))

(defn home-page [state]
  [:div
   [:h1 "Compensation Estimator"]
   [:p "The following inputs will generate " [:em "probabilistic"] " answers that will be used to generate several thousand simulations."]
   [:ol
    [:li
     [:div.heading "Dollar amount of your " [:strong "starting salary"] " before bonuses, raises, and deductions."]
     [:label [:strong "50%"] " chance of being over $" [input state [:factors :starting-salary :aggressive]]]
     [:label [:strong "90%"] " chance of being over $" [input state [:factors :starting-salary :probable]]]]
    [:li
     [:div.heading [:strong "Size of raises"] " granted during the year."]
     [:label [:strong "50%"] " chance over " [input state [:factors :raise-percent :aggressive]] "% of annual salary"]
     [:label [:strong "90%"] " chance over " [input state [:factors :raise-percent :probable]] "% of annual salary"]]
    [:li
     [:div.heading [:strong "Number of raises"] " granted during any given year."]
     [:label [:strong "50%"] " chance of more than " [input state [:factors :raises-per-year :aggressive]]]
     [:label [:strong "90%"] " chance of more than " [input state [:factors :raises-per-year :probable]]]]
    [:li
     [:div.heading "Dollar amount of all " [:strong "annual bonuses"] "."]
     [:label [:strong "50%"] " chance of at least $" [input state [:factors :annual-bonuses :aggressive]]]
     [:label [:strong "90%"] " chance of at least $" [input state [:factors :annual-bonuses :probable]]]]
    [:li
     [:div.heading "Dollar amount of all " [:strong "deductions"] "."]
     [:label [:strong "50%"] " chance of at least $" [input state [:factors :deductions :aggressive]]]
     [:label [:strong "90%"] " chance of at least $" [input state [:factors :deductions :probable]]]]
    [:li
     [:div.heading [:strong "Number of years as an employee"] "."]
     [:label [:strong "50%"] " chance of at least " [input state [:factors :years :aggressive]] " years"]
     [:label [:strong "90%"] " chance of at least " [input state [:factors :years :probable]] " years"]]
    [:li
     [:div.heading [:strong "Number of options purchased"] "."]
     [:label [:strong "50%"] " chance of at least " [input state [:factors :options-purchased :aggressive]] " shares"]
     [:label [:strong "90%"] " chance of at least " [input state [:factors :options-purchased :probable]] " shares"]]
    [:li
     [:div.heading [:strong "Strike price"] " of your options."]
     [:label [:strong "50%"] " chance of at least $" [input state [:factors :strike-price :aggressive]] " per share"]
     [:label [:strong "90%"] " chance of at least $" [input state [:factors :strike-price :probable]] " per share"]]
    [:li
     [:div.heading [:strong "Value of your shares when exercised"] "."]
     [:label [:strong "50%"] " chance of at least $" [input state [:factors :exercise-value :aggressive]]]
     [:label [:strong "90%"] " chance of at least $" [input state [:factors :exercise-value :probable]]]]
    [:li
     [:div.heading [:strong "Capital gains tax rate"] " varies depending on your situation, but in California is usually either 20% or 39.6%."]]
    [:li
     [:div.heading [:strong "Exit value"] " of the company. Small exits are more likely; large exits are rare. Assuming the smallest possible exit
                                          is $10 million and the largest is $10 billion."]]
    [:li
     [:div.heading "Amount of the exit price " [:strong "paid back to investors"] "."]
     [:label [:strong "50%"] " chance of at least $" [input state [:factors :investor-payback :aggressive]]]
     [:label [:strong "90%"] " chance of at least $" [input state [:factors :investor-payback :probable]]]]
    [:li
     [:div.heading "Number of shares of " [:strong "common stock"] "."]
     [:label [:strong "50%"] " chance of at least " [input state [:factors :common-shares :aggressive]] " shares"]
     [:label [:strong "90%"] " chance of at least " [input state [:factors :common-shares :probable]] " shares"]]]
   [controls state]
   [results (@state :simulations) (@state :results)]])

;; -------------------------
;; Initialize app

(defonce state
  (r/atom
    {:factors {:starting-salary {:type :gaussian :aggressive "100000" :probable "60000" :min 0}
               :raise-percent {:type :gaussian :aggressive "7" :probable "4" :min 0}
               :raises-per-year {:type :gaussian :aggressive "2" :probable "1" :min 0.05}
               :years {:type :gaussian :aggressive "4" :probable "3" :min 0.1}
               :annual-bonuses {:type :gaussian :aggressive "2000" :probable "500" :min 0}
               :deductions {:type :gaussian :aggressive "10000" :probable "20000" :min 0}
               :options-purchased {:type :gaussian :aggressive "1000" :probable "500" :min 0}
               :strike-price {:type :gaussian :aggressive "0.50" :probable "1" :min 0}
               :exercise-value {:type :gaussian :aggressive "2" :probable "1" :min 0}
               :exit {:type :power-law :min 10e6 :max 10e9}
               :investor-payback {:type :gaussian :aggressive "20000000" :probable "10000000" :min 0 :max :exit}
               :common-shares {:type :gaussian :aggressive "1500000" :probable "1000000" :min 0}
               :capital-gains-tax-rate {:type :select-one :choices [[3 0.2] [1 0.396]]}}
     :running? false
     :simulations []
     :results []}))

(defn mount-root []
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
