(ns emacskeys.build-analysis-file-test
  (:require [clojure.test :refer :all]
            [emacskeys.build-analysis-file :refer :all]))


(deftest is-invalid-command-test
  (testing "empty-command"
    (is (= (invalid-command? "")
           true)))
  (testing "special commands"
    (is (= (invalid-command? "(lambda nil (interactive) (ido-numbered-select-number 4))")
           true))
    (is (= (invalid-command? "#[257 \307!\310!\211\301\242=\204 \306\300\311\240\210\211@\262\302=\203+ \312!\203+ \313\301\242\305#\207\314 AA\211?\206T \211\303W\203D \315\302\303Z\316\301\242$\207\211\304Y\205T \315\302\304ZT\316\301\242$\207 [(mouse-movement (#<window 27 on build_analysis_file.clj> 1259 (456 . 464) 698306064 nil 1259 (76 . 42) nil (456 . 2) (6 . 11))) (1259) #<window 27 on build_analysis_file.clj> 0 78 0 t event-end posn-point mouse-movement integer-or-marker-p mouse--drag-set-mark-and-point mouse-position mouse-scroll-subr nil auto-hscroll-mode] 9 

(fn EVENT) e]")
           true)))
  (testing "normal commands"
    (is (= (invalid-command? "recenter-top-bottom")
           false))
    (is (= (invalid-command? "undo")
           false))))


;; for the way you want to test write-frequency-maps
;; it makes much more sense to just write an integration
;; test with input/output on the existing JAR file
