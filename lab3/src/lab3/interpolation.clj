(ns lab3.interpolation)

; ЛИНЕЙНАЯ ИНТЕРПОЛЯЦИЯ

(defn find-segment
  "Находит пару соседних точек [p1 p2], таких что x1 <= x <= x2."
  [points x]
  (when (>= (count points) 2)
    (some (fn [[p1 p2]]
            (when (and (<= (:x p1) x)
                       (<= x (:x p2)))
              [p1 p2]))
          (partition 2 1 points))))

(defn linear-value
  "Линейная интерполяция: возвращает y или nil."
  [points x]
  (when-let [[p1 p2] (find-segment points x)]
    (let [x1 (:x p1)
          y1 (:y p1)
          x2 (:x p2)
          y2 (:y p2)]
      (if (= x1 x2)
        y1
        (let [t (/ (- x x1) (- x2 x1))]
          (+ y1 (* t (- y2 y1))))))))

; НЬЮТОН

(defn choose-window
  "Выбирает окно из n точек для интерполяции Ньютона"
  [points n x]
  (let [cnt (count points)]
    (cond
      (zero? cnt) []
      (<= cnt n)  (vec points)

      :else
      (let [closest-idx
            (->> points
                 (map-indexed (fn [i p]
                                [i (Math/abs ^double (- x (:x p)))]))
                 (apply min-key second)
                 first)

            n' (min n cnt)
            half (quot (dec n') 2)
            raw-start (- closest-idx half)
            start (-> raw-start
                      (max 0)
                      (min (- cnt n')))]
        (subvec (vec points) start (+ start n'))))))

(defn divided-differences
  "Строит таблицу разделённых разностей.
   Возвращает вектор коэффициентов [a0 a1 a2 ...]"
  [points]
  (let [xs (mapv :x points)
        ys (mapv :y points)
        n  (count points)]
    (loop [k 0 table ys acc []]
      (if (= k n)
        acc
        (let [acc' (conj acc (first table))
              table'
              (if (= k (dec n))
                []
                (mapv (fn [i]
                        (/ (- (table (inc i))
                              (table i))
                           (- (xs (+ i k 1))
                              (xs i))))
                      (range 0 (- n k 1))))]
          (recur (inc k) table' acc'))))))

(defn newton-eval
  "Вычисляет значение полинома Ньютона в точке x"
  [coeffs points x]
  (let [xs (mapv :x points)
        n  (count coeffs)]
    (loop [k (dec n)
           acc (nth coeffs (dec n))]
      (if (zero? k)
        acc
        (let [k' (dec k)]
          (recur k'
                 (+ (nth coeffs k')
                    (* (- x (xs k')) acc))))))))

(defn newton-value
  "Расчёт y методом Ньютона"
  [points n x]
  (let [window (choose-window points n x)
        coeffs (divided-differences window)]
    (newton-eval coeffs window x)))

; ПОТОКОВАЯ ОБРАБОТКА 

(defn normalize-zero [x]
  (let [d (double x)]
    (if (= d -0.0) 0.0 d)))

(defn init-state
  []
  {:points []
   :next-x nil})

(defn ready?
  [opts points]
  (or (and (:linear? opts)
           (>= (count points) 2))
      (and (:newton? opts)
           (>= (count points) (:n opts)))))

(defn interpolate-at-x
  "Возвращает вектор структур {:alg :linear :x x :y y}, ..."
  [opts points x]
  (let [res []]

    ; linear
    (let [res (if (:linear? opts)
                (let [y (linear-value points x)]
                  (if (some? y)
                    (conj res {:alg :linear :x (normalize-zero x) :y (normalize-zero y)})
                    res))
                res)]

      ; newton
      (let [res (if (:newton? opts)
                  (if (>= (count points) (:n opts))
                    (let [y (newton-value points (:n opts) x)]
                      (conj res {:alg :newton :x (normalize-zero x) :y (normalize-zero y)}))
                    res)
                  res)]
        res))))

(defn produce-outputs
  [opts points next-x max-x]
  (if-not (ready? opts points)
    {:next-x next-x
     :outputs []}

    (let [step (:step opts)
          start-x (or next-x (:x (first points)))]
      (loop [x start-x
             outs []]
        (if (> x max-x)
          {:next-x x
           :outputs outs}
          (let [vals (interpolate-at-x opts points x)
                outs' (into outs vals)]
            (recur (+ x step) outs')))))))

(defn process-point
  [opts state point]
  (let [points' (conj (:points state) point)
        max-x (:x point)
        {:keys [next-x outputs]}
        (produce-outputs opts points' (:next-x state) max-x)]
    {:state {:points points'
             :next-x next-x}
     :outputs outputs}))