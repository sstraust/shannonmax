;;; shannon-max.el --- Analyze your keybindings with information theory -*- lexical-binding: t -*-

;; Copyright (C) 2024 Sam Strus

;; Author: Sam Straus <sam_straus@alumni.brown.edu>
;; URL: https://github.com/sstraust/shannonmax
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Uses information theory to analyze your Emacs usage and suggest better keybindings.

;; Add (shannon-max-start-logger) to your .emacs configuration file to start collecting logs.
;; Once you have enough data, call M-x shannon-max-analyze to see the results.

;; See the project readme for more detailed instructions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(prefer-coding-system 'utf-8)

(declare-function org-table-toggle-column-width "org-table" (&optional arg))

(defgroup shannon-max nil
  "Customization group for ShannonMax."
  :package-version '(shannon-max . "0.1.0")
  :group 'local
  :prefix "shannon-max")

(defcustom shannon-max-output-buffer-name
  "shannon-max-results"
  "The name of the buffer to display the results."
  :type 'buffer
  :group 'shannon-max)

(defcustom shannon-max-alphabet-size
  52
  "The number of available keys to be used for keybindings."
  :type 'integer
  :group 'shannon-max)

(defcustom shannon-max-keylog-file-name
  (expand-file-name "~/emacs-logged-keys")
  "The file to store the keylogged keys and commands."
  :type 'string
  :group 'shannon-max)


(defcustom shannon-max-custom-keypress-cost
  nil
  "A custom function to compute the cost of a given keypress."
  :type 'function
  :group 'shannon-max)

(defcustom shannon-max-filtered-commands
  '("self-insert-command" "markdown-enter-key" "org-kill-line" "god-mode-describe-key" "org-return" "isearch-delete-char" "undefined" "describe-key" "cider-repl-return" "nil")
  "A list of the Emacs commands to ignore from the output.
We also filter all commands matching lambda, or open brackets."
  :type '(repeat string)
  :group 'shannon-max)

(defcustom shannon-max-filter-commands-fn
  (lambda (command-name)
    (string-match-p "ido-.*" command-name))
  "A function of the Emacs commands to ignore from the output."
  :type 'function
  :group 'shannon-max)


(defcustom shannon-max-jar-download-location
  (expand-file-name (locate-user-emacs-file "shannon-max/emacskeys-0.1.1-SNAPSHOT-standalone.jar"))
  "Location to store the jar file, if downloaded through shannonmax."
  :type 'file
  :group 'shannon-max)

(defvar shannon-max-jar-file nil)

(defvar shannon-max--jar-download-path "https://github.com/sstraust/shannonmax/releases/download/v0.1.1b/emacskeys-0.1.1-SNAPSHOT-standalone.jar")


(defconst shannon-max-process-buffer "shannon-max-gather-frequencies")


(defvar shannon-max-logged-events '())
(defvar shannon-max-last-command-info nil)
(defvar shannon-max-current-results-page-to-shorten 0)
(defvar shannon-max-current-results-page-to-lengthen 0)
(defvar shannon-max-logger-enabled t)

(defun shannon-max--set-last-command-info ()
  "Store info about the last-run command in a variable.

Useful so that we can associate `real-last-command'
with the stored information."
  (when (and (key-description (this-command-keys-vector))
	     (not (= (length (key-description (this-command-keys-vector))) 0)))
    (setq shannon-max-last-command-info
	  (vector
	   (current-time)
	   major-mode
	   (key-description (this-command-keys-vector))
	   (buffer-file-name)
	   (cons (line-number-at-pos)
		 (current-column))))))


(defun shannon-max-logger-log ()
  "Log an command-entered event to shannon-max.

Store in a variable so that saving
can be done in batch."
  (when shannon-max-logger-enabled
    (when shannon-max-last-command-info
      (push (vconcat (vector
		      real-last-command)
		     shannon-max-last-command-info)
	    shannon-max-logged-events)
      (setq shannon-max-last-command-info nil))
    (shannon-max--set-last-command-info)))

(defun shannon-max-logger-save ()
  "Save the in-memory shannon-max log to a file."
  (when shannon-max-logged-events
    (with-temp-buffer
      (dolist (k shannon-max-logged-events)
	(insert (format
		 "%s, %s, %S, %s, %s, (%d %d)\n"
		 (aref k 0)
		 (aref k 1)
		 (aref k 2)
		 (aref k 3)
		 (aref k 4)
		 (car (aref k 5))
		 (cdr (aref k 5)))))
      (let ((silent save-silently)
	    (coding-system-for-write 'raw-text))
	  (setq save-silently t)
	  (append-to-file nil nil shannon-max-keylog-file-name)
	  (setq save-silently silent))
	(setq shannon-max-logged-events '()))))

(defun shannon-max-logger-autosave ()
  "Start a timer to save the log every 2 secs."
  (run-with-idle-timer 2 t #'shannon-max-logger-save))

;; (shannon-max-logger-save)
(defun shannon-max-start-logger ()
  "Begin shannonmax logging.

Adds the `shannon-max-logger-log' `post-command-hook',
and begins autosave."
  (add-hook 'post-command-hook #'shannon-max-logger-log)
  (shannon-max-logger-autosave))


(defun shannon-max-logger-enabled-toggle ()
  "Toggle whether the shannon-max logger is enabled.

This is intended as a temporary workaround,
requested by one of our users.
If you have extremely sensitive info
that you do not want to be logged,
it may be worth temporarily removing the shannon-max
package entirely."
  (interactive)
  (if shannon-max-logger-enabled
      (progn (setq shannon-max-logger-enabled nil)
	     (remove-hook 'post-command-hook #'shannon-max-logger-log))
    (progn (setq shannon-max-logger-enabled t)
	   ;; remove it again, just to be safe, so we don't have a bunch
	   ;; of duplicate hooks lying around
	   (remove-hook 'post-command-hook #'shannon-max-logger-log)
	   (add-hook 'post-command-hook #'shannon-max-logger-log))))

(defun shannon-max--keyseq-to-keylist (keyseq)
  "Convert KEYSEQ string to a list."
  (split-string
   (string-trim keyseq) " "))

(defun shannon-max--keyseqs-to-list (keyseqs-list-string)
  "Parse KEYSEQS-LIST-STRING into a list.

KEYSEQS-LIST-STRING is a list of quoted key sequences."
  (seq-filter
   (lambda (x) (not (string-blank-p x)))
   (butlast (cdr (split-string keyseqs-list-string "\"")))))

(cl-defstruct shannon-max-command-info command-name freq keyseqs probability)
(defun shannon-max-extract-to-struct (x)
  "Build shannon-max data representation from string X.

Used to read in the output of the clojure
build_analysis_file.clj (can be found on the github
https://github.com/sstraust/shannonmax)"
  (let ((split-contents (split-string x ",")))
    (make-shannon-max-command-info :command-name (car split-contents)
		       :freq (string-to-number (cadr split-contents))
		       :keyseqs (mapcar #'shannon-max--keyseq-to-keylist
					(shannon-max--keyseqs-to-list (caddr split-contents)))
		       :probability nil)))

(defun shannon-max--total-freq-count (command-freq-input)
  "Total number of keypresses in the COMMAND-FREQ-INPUT.

COMMAND-FREQ-INPUT is the analsysis data as a list of
 `shannon-max-command-info' structs."
  (apply '+ (mapcar #'shannon-max-command-info-freq command-freq-input)))



(defun shannon-max--filter-filtered-commands (command-freq-input)
  "Filter COMMAND-FREQ-INPUT to remove junky commands.

You probably don't want to see commands
like `insert-char' in your output."
  (seq-filter
   (lambda (x)
     (and (not (member (shannon-max-command-info-command-name x) shannon-max-filtered-commands))
	  (or (not shannon-max-filter-commands-fn)
	      (not (funcall shannon-max-filter-commands-fn (shannon-max-command-info-command-name x))))))
   command-freq-input))

  

(defun shannon-max--commands-from-process-output (output-buffer)
  "Read frequency data OUTPUT-BUFFER and produce data for display.

Applies all of the filtering, and
 computes command probabilities."
  (let ((current-lines '())
	(command-freqs nil)
	(total-freq-count nil))
    (with-current-buffer (get-buffer output-buffer)
      (save-excursion
	(goto-char (point-min))
	(let ((cond-met 0))
	  (while (< cond-met 1)
	    (setq current-lines (cons (buffer-substring-no-properties
				       (line-beginning-position)
				       (line-end-position))
				      current-lines))
	    (setq cond-met (forward-line))))))
    (setq current-lines (seq-filter (lambda (x)
				      (string-match "," x))
				    current-lines))
    (setq command-freqs (mapcar #'shannon-max-extract-to-struct current-lines))
    (setq command-freqs (shannon-max--filter-filtered-commands command-freqs))
    (setq total-freq-count (shannon-max--total-freq-count command-freqs))
    (mapc (lambda (x)
	  (setf (shannon-max-command-info-probability x)
		(/ (shannon-max-command-info-freq x)
		   (float total-freq-count))))
	    command-freqs)
    command-freqs))

    
(defun shannon-max--log2 (x)
  "Compute log X base 2."
  (/ (log x) (log 2)))

(defun shannon-max--command-entropy (command-freqs-input)
  "Compute the total entropy of your Emacs usage from COMMAND-FREQS-INPUT."
      (apply '+ (mapcar (lambda (x)
	  (* -1
	     (shannon-max-command-info-probability x)
	     (shannon-max--log2 (shannon-max-command-info-probability x))))
	command-freqs-input)))


(defun shannon-max--keypress-cost (keypress)
  "Compute the cost of an individual KEYPRESS.

By default each key you press on your keyboard
counts as 1 press."
  (if shannon-max-custom-keypress-cost
      (funcall shannon-max-custom-keypress-cost keypress)
    (length (split-string keypress "-"))))

(defun shannon-max--keyseq-cost (keyseq)
  "Compute the cost of a KEYSEQ.

It's the sum of all the individual keypresses"
  (apply '+ (mapcar #'shannon-max--keypress-cost keyseq)))

(defun shannon-max-command-info--actual-keylength (shannon-max-command-info-input)
  "The cost of your current keybinding for a SHANNON-MAX-COMMAND-INFO-INPUT.

In the case that multiple keybindings were used
for the same command, we take the one with the
lowest cost.

This is actually glossing over some details,
because:
- it pulls from your usage data, rather
  than the current configuration
- it is not aware of different keylengths
  for different modes
- because it uses the history, it still
  uses the minimum when keys are changed.

it is certainly inaccurate in some cases, but
I've found it be a reasonable enough estimate
to be useful."
  (apply 'min (mapcar #'shannon-max--keyseq-cost (shannon-max-command-info-keyseqs shannon-max-command-info-input))))

(defun shannon-max-command-info--theoretical-keylength (shannon-max-command-info-input)
  "Compute theoretical optimum length for SHANNON-MAX-COMMAND-INFO-INPUT.

This is the length that the key command
would be in an information-optimal config."
  (* -1 (/ (shannon-max--log2 (shannon-max-command-info-probability shannon-max-command-info-input))
	   (float (shannon-max--log2 shannon-max-alphabet-size)))))
(defun shannon-max--sort-by (input-list num-func)
  "Sort the INPUT-LIST by NUM-FUNC."
  (sort (cl-copy-list input-list)
	(lambda (x y) (< (apply num-func (list x))
			 (apply num-func (list y))))))

(defun shannon-max-command-info--shortest-keybinding (shannon-max-command-info-input)
  "Compute the shortest keybinding used for a SHANNON-MAX-COMMAND-INFO-INPUT.

Shown in the `shannon-max-analyze' display
to help users understand what the problematic
keybinding was.  See `shannon-max-command-info--actual-keylength'
for more detailed info."
  (car
   (shannon-max--sort-by
    (shannon-max-command-info-keyseqs shannon-max-command-info-input)
    #'shannon-max--keyseq-cost)))


(defun shannon-max--get-results-for-page (commands-list page-access-symbol)
  "Display string about COMMANDS-LIST for a specific PAGE-ACCESS-SYMBOL."
  (progn
    (set page-access-symbol (max 0
				 (min
				  (eval page-access-symbol)
				  (/ (length commands-list) 10))))
    (cl-subseq
     (nthcdr (* (eval page-access-symbol) 10)
	     commands-list)
     0 (min 10 (-
		(length commands-list)
		(* (eval page-access-symbol) 10))))))

(defun shannon-max-top-commands-to-shorten (command-freqs-input)
  "Get commands to shorten from COMMAND-FREQS-INPUT."
  (let* ((filtered-sorted (seq-filter (lambda (x) (> (shannon-max-command-info-freq x) 5))
		     (shannon-max--sort-by command-freqs-input (lambda (x)
				 (- (shannon-max-command-info--theoretical-keylength x)
				    (shannon-max-command-info--actual-keylength x)))))))
    (shannon-max--get-results-for-page
     filtered-sorted 'shannon-max-current-results-page-to-shorten)))


(defun shannon-max-top-commands-to-lengthen (command-freqs-input)
  "Get commands to lengthen from COMMAND-FREQS-INPUT."
  (let* ((filtered-sorted
	 (seq-filter (lambda (x) (> (shannon-max-command-info-freq x) 5))
		     (shannon-max--sort-by command-freqs-input (lambda (x)
						    (* -1 (- (shannon-max-command-info--theoretical-keylength x)
							     (shannon-max-command-info--actual-keylength x))))))))
    (shannon-max--get-results-for-page
     filtered-sorted 'shannon-max-current-results-page-to-lengthen)))
    
(defun shannon-max--table-header ()
  "Header string for shannon-max tables."
    (concat "|" "command-name"
	  "|" "freq"
	  "|" "prob"
	  "|" "theory-len"
	  "|" "actual"
	  "|" "difference"
	  "|" "\n"
	  "|<30>|<10>|<10>|<12>|<20>||\n"
	  "|-\n"))

(defun shannon-max--command-table-str (x)
  "Format keyfreq data X into a row of data."
  (concat "|" (shannon-max-command-info-command-name x)
		    "|" (number-to-string (shannon-max-command-info-freq x))
		    "|" (format  "%.3f" (shannon-max-command-info-probability x))
		    "|" (format "%.3f" (shannon-max-command-info--theoretical-keylength x))
		    "|" (format "%s" (shannon-max-command-info--shortest-keybinding x))
		    "|" (format "%.3f"
				(- (shannon-max-command-info--theoretical-keylength x)
				   (shannon-max-command-info--actual-keylength x)))
		    "|" "\n"))

(defun shannon-max--instantiate-table ()
  "Create an org table for shannon-max to use."
  (save-excursion
    (forward-line -1)
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
    (forward-line)))

(defun shannon-max-display-shannon-output (command-freqs-input)
  "Display the shannon max output from COMMAND-FREQS-INPUT."
  (with-current-buffer (get-buffer shannon-max-output-buffer-name)
    (erase-buffer)
    (insert "Total Commands Logged: " (number-to-string (shannon-max--total-freq-count command-freqs-input)) "\n")
    (insert "Entropy: " (number-to-string (shannon-max--command-entropy command-freqs-input)) "\n\n\n")
  (insert "Keybindings that are too long: \n")
  (insert (shannon-max--table-header))
  
  (mapc (lambda (x) (insert (shannon-max--command-table-str x)))
	  (shannon-max-top-commands-to-shorten command-freqs-input))
  (shannon-max--instantiate-table)
  (insert "\n\n\n\n")
  (insert "Keybindings that are too short: \n")
  (insert (shannon-max--table-header))
  (mapc (lambda (x) (insert (shannon-max--command-table-str x)))
	  (shannon-max-top-commands-to-lengthen command-freqs-input))
  (shannon-max--instantiate-table)))

(defun shannon-max-refresh-output ()
  "Refresh the shannon max output."
  (shannon-max-display-shannon-output (shannon-max--commands-from-process-output shannon-max-process-buffer)))

(defun shannon-max--page-down-helper (var-to-edit)
  "Go down page, and then reload table.

VAR-TO-EDIT represents which page-marker variable
to modfiy."
  (set var-to-edit (+ 1 (eval var-to-edit)))
  (shannon-max-refresh-output))


(defun shannon-max--page-up-helper (var-to-edit)
  "Go up page, and then reload table.

VAR-TO-EDIT represents which page-marker variable
to modify."
  (set var-to-edit (max (+ -1 (eval var-to-edit))
			0))
  (shannon-max-refresh-output))

(defun shannon-max--get-current-table-selection ()
  "Check whether cursor is in bottom half of table."
  (if (with-current-buffer (get-buffer shannon-max-output-buffer-name)
	(search-backward "Keybindings that are too short:" nil 't))
      'shannon-max-current-results-page-to-lengthen
    'shannon-max-current-results-page-to-shorten))




(defun shannon-max--edit-saving-line-num (func)
  "Edit the buffer using FUNC, saving the current line number."
  (let ((current-line-number (with-current-buffer (get-buffer shannon-max-output-buffer-name)
			       (count-lines 1 (point)))))
    (funcall func)
    (with-current-buffer (get-buffer shannon-max-output-buffer-name)
      (goto-char (point-min))
      (forward-line current-line-number)
      (let ((curr-point (point)))
	(mapcar (lambda (x)
		(when (eq (current-buffer) (window-buffer x))
		  (set-window-point x curr-point)))
	      (window-list))))))
  

(defun shannon-max-page-down ()
  "Scroll down a page of results."
  (interactive)
  (shannon-max--edit-saving-line-num
   (lambda () (shannon-max--page-down-helper (shannon-max--get-current-table-selection)))))
(defun shannon-max-page-up ()
  "Scroll up a page of results."
  (interactive)
  (shannon-max--edit-saving-line-num
   (lambda () (shannon-max--page-up-helper (shannon-max--get-current-table-selection)))))

(define-minor-mode shannon-max-mode
  "Mode for viewing ShannonMax results."
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-n") #'shannon-max-page-down)
	    (define-key map (kbd "C-c C-p") #'shannon-max-page-up)
	    (define-key map (kbd "C-c C-e") #'keymap-global-set)
	    map))

(defun shannon-max--jar-file-already-downloaded-p ()
  "Check if the shannon-max jar file was already downloaded."
  (and (file-exists-p shannon-max-jar-download-location)
			  (> (file-attribute-size
			      (file-attributes shannon-max-jar-download-location))
			     0)))

(defun shannon-max-download-jar-if-not-present ()
  "Download the shannon-max jar if not already available."
  (when (and (null shannon-max-jar-file)
	     (not (null shannon-max--jar-download-path))
	     (not (null shannon-max-jar-download-location)))
    (if (shannon-max--jar-file-already-downloaded-p)
	(setq shannon-max-jar-file shannon-max-jar-download-location)
      (if (y-or-n-p (concat "Download shanon-max-jar to " shannon-max-jar-download-location "? (necessary to parse your keyfreqs file):"))
	  (progn
	    (make-directory (file-name-directory shannon-max-jar-download-location) t)
	    (url-copy-file shannon-max--jar-download-path
			   shannon-max-jar-download-location)
	    (set-file-modes shannon-max-jar-download-location #o500)
	    (setq shannon-max-jar-file shannon-max-jar-download-location))
	(message "Shannon Max needs the jar file to run. It is used to read your keypress csv file into keyfreqs for processing.")))))
      
	
	
	     

(defun shannon-max-analyze ()
  "Analyze your keybindings with shannon-max!

This is the command that analyzes your logged keybindings
and returns information about which ones are too long or short.

It requires your log file to be populated, meaning you've already
called (shannon-max-start-logger), typically in a config file,
and have been using Emacs with the keylogger enabled long enough
to see interesting results.

This is the main command of the shannon-max package."
  (interactive)
  (progn
    (let ((shannon-max-output-buffer (get-buffer-create shannon-max-output-buffer-name)))
      (switch-to-buffer-other-window shannon-max-output-buffer)
      (with-current-buffer shannon-max-output-buffer
	(shannon-max-mode 1)
	(erase-buffer)
	(insert "Loading...."))
      (with-current-buffer (get-buffer-create shannon-max-process-buffer)
	(erase-buffer))
      (shannon-max-download-jar-if-not-present)
      (let* ((process-name "shannon-max-process1"))
	(start-process process-name shannon-max-process-buffer
						   "java" "-jar" shannon-max-jar-file shannon-max-keylog-file-name)
	(set-process-sentinel (get-process process-name)
			      (lambda (_process _event)
				(shannon-max-display-shannon-output (shannon-max--commands-from-process-output shannon-max-process-buffer))))))))


		

;; (shannon-max-run-proces-and-display-results)
(provide 'shannon-max)

;;; shannon-max.el ends here
