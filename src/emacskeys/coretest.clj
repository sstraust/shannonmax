(ns emacskeys.coretest)

(ns emacskeys.core
  (:require [clojure.data.csv :as csv]))


(def keylog-file (slurp "/Users/sam/emacs-logged-keys"))

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

(get commands-to-bindings "cider-eval-last-sexp")
;; (count commands-to-bindings)



(first commands-to-bindings)

(get commands-to-bindings
     (first (first (drop 8 keylog-frequencies-cleaned))))

(take 10 keylog-frequencies-cleaned)

(take 20  bindings-lines-commands)

(take 20 (reverse
          (filter #(= 2 (count %))  bindings-lines-split)))




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
      (sort-by :difference >
                  (for [binding keybindings-correctness-data
                        :when (> (:frequencies binding) 10)
                        ]
                    binding
                    ))))

(take 20
      (map #(select-keys % [:command :actual-keys :difference])
      (sort-by :difference <
                  (for [binding keybindings-correctness-data
                        ]
                    binding
                    ))))

;; I also want to get the keys that are fully unbound
(count (map #(select-keys % [:command :actual-keys :difference])
      (sort-by :difference <
                  (for [binding keybindings-correctness-data
                        ]
                    binding
                    ))))
