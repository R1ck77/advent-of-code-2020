(ns advent-of-code-2020.logging)

(def ^:dynamic *profile* true)

(def profile-data-atom (atom {}))

(defn reset-profile []
  (reset! profile-data-atom {}))

(defmacro profile
  [f-call]
  (if *profile*
    (let [name (str (first f-call))]
      `(let [start# (System/currentTimeMillis)
             result# ~f-call
             end# (System/currentTimeMillis)]
         (swap! profile-data-atom (fn [pdata#]                    
                      (update pdata# ~name (fn [old-value#]
                                                   (let [diff# (- end# start#)]
                                                     (if old-value#
                                                       (+ old-value# diff#)
                                                       diff#))))))
         result#))
    f-call))

(def ^:private history (atom []))

(defn reset-history []
  (reset! history []))

(defn write-history
  ([] (write-history "/tmp/history.txt"))
  ([filename]
   (let [text (apply str (interpose "\n" (map #(interpose " " %) @history)))]
     (spit filename text))))

(defn history-point [& values]
  (if *profile*
   (swap! history (fn [history]
                    (conj history (vec values))))))

(def ^:private speed-info-atom (atom {:start-time nil
                                      :moves-done 0
                                      :total 0}))

(defn reset-speed []
  (reset! speed-info-atom {:start-time nil
                           :moves-done 0
                           :total 0}))
(defn speed
  ([] (speed 1000))
  ([interval-ms]
   (swap! speed-info-atom
          (fn [{:keys [start-time moves-done total]}]
            (let [start-time (if (not start-time) (System/currentTimeMillis) start-time)
                  moves-done (inc moves-done)
                  now (System/currentTimeMillis)
                  diff (- now start-time)
                  total (inc total)]
              (if (< diff interval-ms)
                {:start-time start-time
                 :moves-done moves-done
                 :total total}
                (do (println (format "Events/s: %.2f (total: %d)" (* 1000.0 (/ moves-done diff)) total))
                    {:start-time now
                     :moves-done 0
                     :total total})))))))

(defn reset []
  (reset-profile)
  (reset-history)
  (reset-speed))
