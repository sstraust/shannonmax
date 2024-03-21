(ns emacskeys.core
  (:require [clojure.data.csv :as csv]
            [libpython-clj2.python :as py :refer [py. ->python]]
            [clojure.java.io :as io]))
(if (.exists (io/file "/Users/sam/anaconda3/bin/python3"))
  (py/initialize! :python-executable "/Users/sam/anaconda3/bin/python3"
                  :library-path "/Users/sam/anaconda3/lib/python3.10")
  (py/initialize!))

(require '[libpython-clj2.require :refer [require-python]])



(def keylog-file (slurp "/Users/sam/emacs-logged-keys2"))

(def keylog-csv (doall (csv/read-csv keylog-file)))

(defn row-to-map [csv-row]
  {:command-name (nth csv-row 0 nil)
   :elisp-time (nth csv-row 1 nil)
   :major-mode (nth csv-row 2 nil)
   :keysequence (nth csv-row 3 nil)
   :filename (nth csv-row 4 nil)
   :cursor-position (nth csv-row 5 nil)})

(def keylog-commands (map row-to-map keylog-csv))


(def keylog-frequencies (frequencies
                         (map :command-name keylog-commands)))



(def keylog-frequencies (frequencies
                         (map :command-name keylog-commands)))

(take 10 (sort-by second > keylog-frequencies))


(def keylog-frequencies-cleaned (drop 5 (sort-by second > keylog-frequencies)))


(def total-keys-pressed (apply + (map second keylog-frequencies-cleaned)))

(float (/ (second (first keylog-frequencies-cleaned))
          total-keys-pressed))

(defn command-probability [command]
  (float (/ (second command)
            total-keys-pressed)))

(def command-probabilities (map command-probability keylog-frequencies-cleaned))
(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(def entropy (apply + (map (fn [p] (* -1 p (log2 p))) command-probabilities)))

(def bits-per-symbol (log2 52))

(/ entropy bits-per-symbol)

;; (println "hi!")

(require-python '[dahuffman :refer [HuffmanCodec]])

(def codec (py. HuffmanCodec from_frequencies
                (->python (into {} keylog-frequencies-cleaned))))


(def codec (py. HuffmanCodec from_frequencies
                (->python (into {} keylog-frequencies-cleaned))))

(def code-table (py. codec get_code_table))

(defn decimal-to-binary [n]
  (if (zero? n)
    "0"
    (clojure.string/join ""
      (reverse
        (loop [num n
               result []]
          (if (zero? num)
            result
            (recur (quot num 2) (conj result (str (mod num 2))))))))))

(defn pad-right [x]
  (str x (apply str (repeatedly (Math/max 0 (- 5 (count x))) (fn [] "0")))))
(defn pad-left
  ([x] (pad-left x 5))
  ([x amount]
   (str (apply str (repeatedly (Math/max 0 (- amount (count x))) (fn [] "0"))) x)))

(def number-lookup-table
  {0 "a"
   1 "b"
   2 "c"
   3 "d"
   4 "e"
   5 "f"
   6 "g"
   7 "h"
   8 "i"
   9 "j"
   10 "k"
   11 "l"
   12 "m"
   13 "n"
   14 "o"
   15 "p"
   16 "q"
   17 "r"
   18 "s"
   19 "t"
   20 "u"
   21 "v"
   22 "w"
   23 "x"
   24 "y"
   25 "z"
   26 "1"
   27 "2"
   28 "3"
   29 "4"
   30 "5"
   31 "6"
   32 "7"
   33 "8"
   34 "9"
   35 "0"
   36 "-"
   37 "="
   38 "`"
   39 "["
   40 "]"
   41 "\\"
   42 ";"
   43 "'"
   44 "SHIFT"
   45 ","
   46 "."
   47 "/"
   48 "CTRL"
   49 "CMD"
   50 "TAB"
   51 "DEL"})

(def binary-lookup-table
 (map (fn [[n symb]] [(pad-left (decimal-to-binary n)) symb])
      (sort-by first > number-lookup-table)))

(reverse binary-lookup-table)

(defn get-first-symbol [bits-remaining]
  (first (filter (fn [x] (.startsWith (pad-right bits-remaining) (first x))) binary-lookup-table)))
(take 10 (reverse binary-lookup-table))
(first binary-lookup-table)
(get-first-symbol (pad-right "001"))

(defn convert-bits-to-chars [codec-bits]
  (loop [bits-remaining codec-bits
         curr-representation []]
    (if (empty? bits-remaining)
      curr-representation
      (let [bits-remaining (pad-right bits-remaining)
            first-symbol (get-first-symbol bits-remaining)]
        ;; (println first-symbol)
        (recur (subs bits-remaining (count (first first-symbol)))
               (conj curr-representation (second first-symbol)))))))

(def code-symbols-table
  (for [[fn-name code-info] code-table]
    (let [[num-bits val] code-info]
      [fn-name (convert-bits-to-chars (pad-left (Integer/toBinaryString val) num-bits))])))

(take 10 (reverse (sort-by (comp count second)  code-symbols-table)))

;; (first code-table)
;; (let [[num-bits val] (second (second code-table))]
;;   (pad-left (Integer/toBinaryString val) num-bits))

;; 1
;; (convert-bits-to-chars "1")
    

;; no character sequence can proceed another character sequence, so
;; I'm just going to break this into blocks of 5
;; you need to pad it on the _right_ with 0s. This is very important


;; (log2 (/ 1 probability))
(def bindings-file (slurp "resources/bindings_list.txt"))

(def bindings-lines (clojure.string/split bindings-file #"\n"))

(def bindings-lines-split
  (map #(drop 1 (re-find #"(.+)\s+([a-z-]+)$" %)) bindings-lines))

(defn is-valid-keyseq [keyseq]
  (and (not (nil? keyseq))
       (not (re-find #"<" keyseq))))

(def bindings-lines-commands
  (remove #(not (is-valid-keyseq (second %)))
  (map (fn [[keyseq cmd]] [(clojure.string/trim cmd) (clojure.string/trim keyseq)])
       (filter #(= 2 (count %))  bindings-lines-split))))




(def zz (map (fn [[x y]] {x y}) bindings-lines-commands))
(defn update-binding-for-god-mode [input-binding]
  (clojure.string/replace
   (clojure.string/replace 
    (clojure.string/replace  
     (clojure.string/replace 
      (clojure.string/replace  input-binding
                              #" ([a-zA-Z!])[ $]"
                              " godmode-esc $1")
      #"C-M-"
      "g ")
     #"C-"
     "")
    #"s-"
    "super ")
   #"M-"
   "meta "))
(defn keyseq-len [keyseq]
  (count (clojure.string/split keyseq #" ")))
(def commands-to-bindings
  (apply merge-with (fn [keyseq1 keyseq2]
                    (let [gb1 (update-binding-for-god-mode keyseq1)
                          gb2 (update-binding-for-god-mode keyseq2)]
                      (if (> (keyseq-len gb1) (keyseq-len gb2))
                        keyseq2
                        keyseq1)))
         zz))

(def commands-with-bindings
  (for [[command frequencies] keylog-frequencies-cleaned
      :when (get commands-to-bindings command)]
    [command [frequencies (get commands-to-bindings command)
              (update-binding-for-god-mode (get commands-to-bindings command))
              ]]))

(def commands-with-bindings-total
  (apply + (map (fn [command-with-binding] (first (second command-with-binding))) commands-with-bindings)))


(def keybindings-correctness-data
  (for [[command freq-data] commands-with-bindings
      :let [probability (/ (first freq-data)
                           commands-with-bindings-total)
            optimal-length (/ (log2 (/ 1 probability))
                              (log2 52))
            actual-length (count (clojure.string/split (nth freq-data 2) #" "))]]
  {:command command
   :probability (float probability)
   :frequencies (first freq-data)
   :optimal-length optimal-length
   :actual-length actual-length
   :difference (float (- actual-length optimal-length))
   :actual-keys (nth freq-data 2)}))

(take 20
      (map #(select-keys % [:command :actual-keys :difference])
      (sort-by :difference <
                  (for [binding keybindings-correctness-data]
                    binding
                    ))))




