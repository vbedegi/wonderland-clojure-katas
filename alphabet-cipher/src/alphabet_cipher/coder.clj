(ns alphabet-cipher.coder)

(def alphabet (cycle "abcdefghijklmnopqrstuvwxyz"))

(defn char->int [c]
  (- (int c) 97))

(defn int->char [i]
  (char (+ i 97)))

(defn codec [f keyword message]
  (apply str (map f message (cycle keyword))))

(defn encode [keyword message]
  (let [encode-chars (fn [c1 c2]
                       (let [offset (+ (char->int c1)
                                       (char->int c2))]
                         (-> offset (drop alphabet) first)))]
    (codec encode-chars keyword message)))

(defn decode [keyword message]
  (letfn [(decode-chars [c1 c2]
                        (->> (drop (char->int c2) alphabet)
                             (take-while #(not= % c1))
                             count
                             int->char))]
    (codec decode-chars keyword message)))
