(setq current-lines '())
(save-excursion
  (set-buffer (get-buffer "name1"))
  (goto-char (point-min))
  (while (< (forward-line) 1)
    (setq current-lines (cons (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))
			      current-lines))))

(setq current-lines (seq-filter (lambda (x)
				  (string-match "," x)) current-lines))




(setq command-freqs (mapcar #'extract-to-struct current-lines))
(setq total-freq-count (apply '+ (mapcar #'command-info-freq command-freqs)))

(mapcar (lambda (x)
	  (setf (command-info-probability x)
		(/ (command-info-freq x)
		   (float total-freq-count))))
	command-freqs)

(start-process "my-process1" "name1"
	       "lein" "run" "-m" "emacskeys.build-analysis-file/write-frequency-maps" "/Users/sam/emacs-logged-keys4")


