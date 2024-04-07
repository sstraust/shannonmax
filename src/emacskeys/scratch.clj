(ns emacskeys.scratch)

(use 'emacskeys.core)

(defn invalid-regex [input-str]
  (re-find #"(lambda)|(\()|(\[)" input-str))
(count (remove #(invalid-regex (first %)) keylog-frequencies-cleaned))
(take 3 (map first  keylog-frequencies-cleaned))

(defn is-invalid-command [command-name]
  (re-find #"(lambda)|(\()|(\[)" command-name))

(def keylog-commands2 (remove #(is-invalid-command (:command-name %)) keylog-commands))

(def commands-grouped  (group-by :command-name keylog-commands2))
(:keysequence (first (second (first commands-grouped))))

(let [command-group (nth (seq commands-grouped) 4)]
  [(first command-group)
   (distinct (map :keysequence (second command-group)))])
