;;; -*- lexical-binding: t -*-
(defvar output-buffer (get-buffer-create "shannonmax-results"))
(defvar alphabet-size 52)
(defconst shannonmax-process-name "shannonmax-gather-frequencies")

(defun keyseq-to-keylist (keyseq)
  (split-string
   (string-trim keyseq) " "))

(defun keyseqs-to-list (keyseqs-list-string)
  (seq-filter
   (lambda (x) (not (string-blank-p x)))
   (butlast (cdr (split-string keyseqs-list-string "\"")))))

(cl-defstruct command-info command-name freq keyseqs probability theoretical-keylength actual-keylength)
(defun extract-to-struct (x)
  (let ((split-contents (split-string x ",")))
    (make-command-info :command-name (car split-contents)
		       :freq (string-to-number (cadr split-contents))
		       :keyseqs (mapcar #'keyseq-to-keylist
					(keyseqs-to-list (caddr split-contents)))
		       :probability nil
		       :actual-keylength nil
		       )))

(defun shannonmax-total-freq-count (command-freq-input)
  (apply '+ (mapcar #'command-info-freq command-freq-input)))

(defun commands-from-process-output (output-buffer)
  (let ((current-lines '())
	(command-freqs nil)
	(total-freq-count nil))
    (save-excursion
      (set-buffer (get-buffer output-buffer))
      (goto-char (point-min))
      (while (< (forward-line) 1)
	(setq current-lines (cons (buffer-substring-no-properties
				   (line-beginning-position)
				   (line-end-position))
				  current-lines))))
    (setq current-lines (seq-filter (lambda (x)
				      (string-match "," x)) current-lines))
    (setq command-freqs (mapcar #'extract-to-struct current-lines))
    (setq total-freq-count (total-freq-count command-freqs))
    (mapcar (lambda (x)
	  (setf (command-info-probability x)
		(/ (command-info-freq x)
		   (float total-freq-count))))
	    command-freqs)
    command-freqs))
    
(defun log2 (x)
  (/ (log x) (log 2)))

(defun command-entropy (command-freqs-input)
      (apply '+ (mapcar (lambda (x)
	  (* -1
	     (command-info-probability x)
	     (log2 (command-info-probability x))))
	command-freqs-input)))


(defun keypress-cost (keypress)
  (length (split-string keypress "-")))

(defun keyseq-cost (keyseq)
  (apply '+ (mapcar 'keypress-cost keyseq)))

(defun command-info--actual-keylength (command-info-input)
  (apply 'min (mapcar 'keyseq-cost (command-info-keyseqs command-info-input))))

(defun command-info--theoretical-keylength (command-info-input)
  (* -1 (/ (log2 (command-info-probability command-info-input))
	   (float (log2 alphabet-size)))))
(defun sort-by (input-list num-func)
  (sort (copy-list input-list)
	(lambda (x y) (< (apply num-func (list x))
			 (apply num-func (list y))))))

(defun command-info--shortest-keybinding (command-info-input)
  (car
   (sort-by
    (command-info-keyseqs command-info-input)
    'keyseq-cost)))

(defun top-commands-to-shorten (command-freqs-input)
  (-take 10
	 (seq-filter (lambda (x) (> (command-info-freq x) 5))
		     (sort-by command-freqs-input (lambda (x)
				 (- (command-info--theoretical-keylength x)
				    (command-info--actual-keylength x)))))))

(defun top-commands-to-lengthen (command-freqs-input)
  (-take 10
	 (seq-filter (lambda (x) (> (command-info-freq x) 5))
	  (sort-by command-freqs-input (lambda (x)
				 (* -1 (- (command-info--theoretical-keylength x)
					  (command-info--actual-keylength x))))))))
(defun table-header ()
    (concat "|" "command-name"
	  "|" "freq"
	  "|" "prob"
	  "|" "theory-len"
	  "|" "actual"
	  "|" "difference"
	  "|" "\n"
	  "|<30>|<10>|<10>|<12>|<20>||\n"
	  "|-\n"))

(defun command-table-str (command-info)
  (concat "|" (command-info-command-name x)
		    "|" (number-to-string (command-info-freq x))
		    "|" (format  "%.3f" (command-info-probability x))
		    "|" (format "%.3f" (command-info--theoretical-keylength x))
		    "|" (format "%s" (command-info--shortest-keybinding x))
		    "|" (format "%.3f"
				(- (command-info--theoretical-keylength x)
				   (command-info--actual-keylength x)))
		    "|" "\n"))
(defun instantiate-table ()
    (previous-line)
    (move-beginning-of-line nil)
    (org-cycle)
    (org-table-toggle-column-width)
    (org-cycle)
    (org-table-toggle-column-width)
    (org-cycle)
    (org-table-toggle-column-width)
    (org-cycle)
    (org-table-toggle-column-width)
    (org-cycle)
    (org-table-toggle-column-width)
    (next-line))

;; (length command-freqs)
(defun display-shannon-output (command-freqs-input)
  (with-current-buffer output-buffer
    (erase-buffer)
    (insert "Total Commands Logged: " (number-to-string (total-freq-count command-freqs-input)) "\n")
    (insert "Entropy: " (number-to-string (command-entropy command-freqs-input)) "\n\n\n")
  (insert "Keybindings that are too long: \n")
  (insert (table-header))
  
  (mapcar (lambda (x) (insert (command-table-str x)))
	  (top-commands-to-shorten command-freqs-input))
  (instantiate-table)
  (insert "\n\n\n\n")
  (insert "Keybindings that are too short: \n")
  (insert (table-header))
  (mapcar (lambda (x) (insert (command-table-str x)))
	  (top-commands-to-lengthen command-freqs-input))
  (instantiate-table)))


(defun run-proces-and-display-results ()
  (progn
    (with-current-buffer shannonmax-process-name
      (erase-buffer))
    (let* ((process-name "shannonmax-process1")
	 (compute-freqs-process (start-process process-name shannonmax-process-name
					       "lein" "run" "-m" "emacskeys.build-analysis-file/write-frequency-maps"
					       "/Users/sam/emacs-logged-keys4")))
      (set-process-sentinel (get-process process-name)
			    (lambda (process event)
			      (display-shannon-output (commands-from-process-output shannonmax-process-name)))))))
