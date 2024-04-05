(ns emacskeys.scratch)

(use 'emacskeys.core)

(defn invalid-regex [input-str]
  (re-find #"(lambda)|(\()|(\[)" input-str))
(count (remove #(invalid-regex (first %)) keylog-frequencies-cleaned))
(take 3 (map first  keylog-frequencies-cleaned))

(def commands-grouped  (group-by :command-name keylog-commands))
(:keysequence (first (second (first commands-grouped))))

(let [command-group (second commands-grouped)]
  [(first command-group)
   (distinct (map :keysequence (second command-group)))])
