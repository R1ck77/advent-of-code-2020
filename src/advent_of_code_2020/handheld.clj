(ns advent-of-code-2020.handheld)

(defprotocol CpuState
  (getIP [this] "Get the current not yet executed instruction (empty pointer)")
  (getAcc [this] "Get the current value of the accumulator")
  (getProgram [this] "Get the list of opcodes currently loaded")
  (evolve [this] "create a new CpuState with the new state"))

(def ^:private create-cpu-state)

(defn- do-acc [cpu-state value]
  (create-cpu-state (.getProgram cpu-state)
                    (inc (.getIP cpu-state))
                    (+ value (.getAcc cpu-state))))

(defn- do-jmp [cpu-state value]
  (create-cpu-state (.getProgram cpu-state)
                    (+ value (.getIP cpu-state))
                    (.getAcc cpu-state)))

(defn- step [cpu-state]
  (let [[op value] (nth (.getProgram cpu-state) (.getIP cpu-state))]
    (case op
        :acc (do-acc cpu-state value)
        :nop (do-jmp cpu-state 1)
        :jmp (do-jmp cpu-state value)
        (throw (IllegalStateException. "invalid instruction encountered")))))

(defn create-cpu-state
  ([opcodes]
   (create-cpu-state opcodes 0 0))
  ([opcodes ip accumulator]
   (reify CpuState
     (getIP [this] ip)
     (getAcc [this] accumulator)
     (getProgram [this] opcodes)
     (evolve [this]
       (step this)))))
