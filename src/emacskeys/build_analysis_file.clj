(ns emacskeys.build-analysis-file
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn row-to-map [csv-row]
  {:command-name (nth csv-row 0 nil)
   :elisp-time (nth csv-row 1 nil)
   :major-mode (nth csv-row 2 nil)
   :keysequence (nth csv-row 3 nil)
   :filename (nth csv-row 4 nil)
   :cursor-position (nth csv-row 5 nil)})


(defn invalid-command? [command-name]
  (boolean
   (or (re-find #"(lambda)|(\()|(\[)" command-name)
       (empty? command-name))))

(defn get-command-keyseqs [commands-list]
  (let [commands-grouped  (group-by :command-name commands-list)]
    (into {}
          (for [[command-name command-contents] commands-grouped]
            [command-name (distinct (map :keysequence command-contents))]))))

(defn filter-keyseq-list [keyseq-list]
  (remove #(or (not (string? %))
               (empty? (clojure.string/trim %))
               (re-find #"\(|mouse|<" %)) keyseq-list))



(defn write-frequency-maps [filename]
  (let [keylog-file (slurp filename)
        keylog-csv (doall (csv/read-csv keylog-file))
        keylog-commands (map row-to-map keylog-csv)
        keylog-commands (remove #(invalid-command? (:command-name %)) keylog-commands)
        keylog-frequencies (frequencies
                            (map :command-name keylog-commands))
        actual-commands-map (get-command-keyseqs keylog-commands)]
    (doseq [command (keys keylog-frequencies)]
      (when (not (empty? (into [] (filter-keyseq-list (get actual-commands-map command)))))
      (println
       (clojure.string/join
        ","
        [command (get keylog-frequencies command) (into [] (filter-keyseq-list (get actual-commands-map command)))])))


      )))

(defn -main [& args]
  (write-frequency-maps (first args)))

;; (write-frequency-maps "/Users/sam/emacs-logged-keys4")
