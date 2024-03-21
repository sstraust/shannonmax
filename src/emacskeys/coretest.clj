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



