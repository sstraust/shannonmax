;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'subr-x)
(defgroup shannon-max nil
  "Customization group for ShannonMax."
  :package-version '(keyfreq . "1.7")
  :group 'local
  :prefix "shannon-max")

(defcustom shannon-max-output-buffer-name
  "shannon-max-results"
  "The name of the buffer to display the results"
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
  "A list of the emacs commands to ignore from the output. We also filter all commands matching lambda, or open brackets"
  :type '(repeat string)
  :group 'shannon-max)

(defcustom shannon-max-filter-commands-fn
  (lambda (command-name)
    (string-match-p "ido-*" command-name))
  "A function of the emacs commands to ignore from the output."
  :type 'function
  :group 'shannon-max)

(defvar shannon-max-jar-file nil)


(defconst shannon-max-process-buffer "shannon-max-gather-frequencies")


(defvar shannon-max-logged-events '())
(defvar shannon-max-last-command-info nil)
(defvar shannon-max-current-results-page-to-shorten 0)
(defvar shannon-max-current-results-page-to-lengthen 0)
(defvar shannon-max-logger-enabled t)

(defun shannon-max-set-last-command-info ()
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
  (when shannon-max-logger-enabled
    (when shannon-max-last-command-info
      (push (vconcat (vector
		      real-last-command)
		     shannon-max-last-command-info)
	    shannon-max-logged-events)
      (setq shannon-max-last-command-info nil))
    (shannon-max-set-last-command-info)))

;; (shannon-logger-save)
(defun shannon-max-logger-save ()
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

(defun shannon-logger-autosave ()
  (setq shannon-logger-timer
	(run-with-idle-timer 2 t 'shannon-max-logger-save)))

;; (shannon-max-logger-save)
(defun shannon-max-start-logger ()
  (add-hook 'post-command-hook 'shannon-max-logger-log)
  (shannon-logger-autosave))
(prefer-coding-system 'utf-8)

(defun shannon-max-logger-enabled-toggle ()
  (interactive)
  (if shannon-max-logger-enabled
      (progn (setq shannon-max-logger-enabled nil)
	     (remove-hook 'post-command-hook 'shannon-max-logger-log))
    (progn (setq shannon-max-logger-enabled t)
	   ;; remove it again, just to be safe, so we don't have a bunch
	   ;; of duplicate hooks lying around
	   (remove-hook 'post-command-hook 'shannon-max-logger-log)
	   (add-hook 'post-command-hook 'shannon-max-logger-log))))

(defun shannon-max-keyseq-to-keylist (keyseq)
  (split-string
   (string-trim keyseq) " "))

(defun shannon-max-keyseqs-to-list (keyseqs-list-string)
  (seq-filter
   (lambda (x) (not (string-blank-p x)))
   (butlast (cdr (split-string keyseqs-list-string "\"")))))

(cl-defstruct shannon-max-command-info command-name freq keyseqs probability)
(defun shannon-max-extract-to-struct (x)
  (let ((split-contents (split-string x ",")))
    (make-shannon-max-command-info :command-name (car split-contents)
		       :freq (string-to-number (cadr split-contents))
		       :keyseqs (mapcar #'shannon-max-keyseq-to-keylist
					(shannon-max-keyseqs-to-list (caddr split-contents)))
		       :probability nil
		       )))

(defun shannon-max-total-freq-count (command-freq-input)
  (apply '+ (mapcar #'shannon-max-command-info-freq command-freq-input)))



(defun shannon-max-filter-filtered-commands (command-freq-input)
  (seq-filter
   (lambda (x)
     (and (not (member (shannon-max-command-info-command-name x) shannon-max-filtered-commands))
	  (or (not shannon-max-filter-commands-fn)
	      (not (funcall shannon-max-filter-commands-fn (shannon-max-command-info-command-name x))))
	  ))
   command-freq-input))

  

(defun shannon-max-commands-from-process-output (output-buffer)
  (let ((current-lines '())
	(command-freqs nil)
	(total-freq-count nil))
    (save-excursion
      (set-buffer (get-buffer output-buffer))
      (goto-char (point-min))
      (let ((cond-met 0))
      (while (< cond-met 1)
	(setq current-lines (cons (buffer-substring-no-properties
				   (line-beginning-position)
				   (line-end-position))
				  current-lines))
	(setq cond-met (forward-line))
	)))
    (setq current-lines (seq-filter (lambda (x)
				      (string-match "," x))
				    current-lines))
    (setq command-freqs (mapcar #'shannon-max-extract-to-struct current-lines))
    (setq command-freqs (shannon-max-filter-filtered-commands command-freqs))
    (setq total-freq-count (shannon-max-total-freq-count command-freqs))
    (mapcar (lambda (x)
	  (setf (shannon-max-command-info-probability x)
		(/ (shannon-max-command-info-freq x)
		   (float total-freq-count))))
	    command-freqs)
    command-freqs))

    
(defun shannon-max-log2 (x)
  (/ (log x) (log 2)))

(defun shannon-max-command-entropy (command-freqs-input)
      (apply '+ (mapcar (lambda (x)
	  (* -1
	     (shannon-max-command-info-probability x)
	     (shannon-max-log2 (shannon-max-command-info-probability x))))
	command-freqs-input)))


(defun shannon-max-keypress-cost (keypress)
  (if shannon-max-custom-keypress-cost
      (funcall shannon-max-custom-keypress-cost keypress)
    (length (split-string keypress "-"))))

(defun shannon-max-keyseq-cost (keyseq)
  (apply '+ (mapcar 'shannon-max-keypress-cost keyseq)))

(defun shannon-max-command-info--actual-keylength (shannon-max-command-info-input)
  (apply 'min (mapcar 'shannon-max-keyseq-cost (shannon-max-command-info-keyseqs shannon-max-command-info-input))))

(defun shannon-max-command-info--theoretical-keylength (shannon-max-command-info-input)
  (* -1 (/ (shannon-max-log2 (shannon-max-command-info-probability shannon-max-command-info-input))
	   (float (shannon-max-log2 shannon-max-alphabet-size)))))
(defun sort-by (input-list num-func)
  (sort (cl-copy-list input-list)
	(lambda (x y) (< (apply num-func (list x))
			 (apply num-func (list y))))))

(defun shannon-max-command-info--shortest-keybinding (shannon-max-command-info-input)
  (car
   (sort-by
    (shannon-max-command-info-keyseqs shannon-max-command-info-input)
    'shannon-max-keyseq-cost)))


  

(defun shannon-max-get-results-for-page (commands-list page-access-symbol)
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
  (let* ((filtered-sorted (seq-filter (lambda (x) (> (shannon-max-command-info-freq x) 5))
		     (sort-by command-freqs-input (lambda (x)
				 (- (shannon-max-command-info--theoretical-keylength x)
				    (shannon-max-command-info--actual-keylength x)))))))
    (shannon-max-get-results-for-page
     filtered-sorted 'shannon-max-current-results-page-to-shorten)))


;; (cl-subseq '(1 2 3) 0 1)
(defun shannon-max-top-commands-to-lengthen (command-freqs-input)
  (let* ((filtered-sorted
	 (seq-filter (lambda (x) (> (shannon-max-command-info-freq x) 5))
		     (sort-by command-freqs-input (lambda (x)
						    (* -1 (- (shannon-max-command-info--theoretical-keylength x)
							     (shannon-max-command-info--actual-keylength x))))))))
    (shannon-max-get-results-for-page
     filtered-sorted 'shannon-max-current-results-page-to-lengthen)))
    
(defun shannon-max-table-header ()
    (concat "|" "command-name"
	  "|" "freq"
	  "|" "prob"
	  "|" "theory-len"
	  "|" "actual"
	  "|" "difference"
	  "|" "\n"
	  "|<30>|<10>|<10>|<12>|<20>||\n"
	  "|-\n"))

(defun shannon-max-command-table-str (x)
  (concat "|" (shannon-max-command-info-command-name x)
		    "|" (number-to-string (shannon-max-command-info-freq x))
		    "|" (format  "%.3f" (shannon-max-command-info-probability x))
		    "|" (format "%.3f" (shannon-max-command-info--theoretical-keylength x))
		    "|" (format "%s" (shannon-max-command-info--shortest-keybinding x))
		    "|" (format "%.3f"
				(- (shannon-max-command-info--theoretical-keylength x)
				   (shannon-max-command-info--actual-keylength x)))
		    "|" "\n"))

(defun shannon-max-instantiate-table ()
  (save-excursion
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
    (next-line)))

;; (length command-freqs)
(defun shannon-max-display-shannon-output (command-freqs-input)
  (with-current-buffer (get-buffer shannon-max-output-buffer-name)
    (erase-buffer)
    (insert "Total Commands Logged: " (number-to-string (shannon-max-total-freq-count command-freqs-input)) "\n")
    (insert "Entropy: " (number-to-string (shannon-max-command-entropy command-freqs-input)) "\n\n\n")
  (insert "Keybindings that are too long: \n")
  (insert (shannon-max-table-header))
  
  (mapcar (lambda (x) (insert (shannon-max-command-table-str x)))
	  (shannon-max-top-commands-to-shorten command-freqs-input))
  (shannon-max-instantiate-table)
  (insert "\n\n\n\n")
  (insert "Keybindings that are too short: \n")
  (insert (shannon-max-table-header))
  (mapcar (lambda (x) (insert (shannon-max-command-table-str x)))
	  (shannon-max-top-commands-to-lengthen command-freqs-input))
  (shannon-max-instantiate-table)))

(defun shannon-max-refresh-output ()
  (shannon-max-display-shannon-output (shannon-max-commands-from-process-output shannon-max-process-buffer)))

(defun shannon-max-page-down-helper (var-to-edit)
  (set var-to-edit (+ 1 (eval var-to-edit)))
  (shannon-max-refresh-output))


(defun shannon-max-page-up-helper (var-to-edit)
  (set var-to-edit (max (+ -1 (eval var-to-edit))
			0))
  (shannon-max-refresh-output))

(defun get-current-table-selection ()
  (if (with-current-buffer (get-buffer shannon-max-output-buffer-name)
	(search-backward "Keybindings that are too short:" nil 't))
      'shannon-max-current-results-page-to-lengthen
    'shannon-max-current-results-page-to-shorten))




(defun shannon-max-edit-saving-line-num (func)
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
  (interactive)
  (shannon-max-edit-saving-line-num
   (lambda () (shannon-max-page-down-helper (get-current-table-selection)))))
(defun shannon-max-page-up ()
  (interactive)
  (shannon-max-edit-saving-line-num
   (lambda () (shannon-max-page-up-helper (get-current-table-selection)))))

(define-minor-mode shannon-max-mode
  "Mode for viewing ShannonMax results"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-n") 'shannon-max-page-down)
	    (define-key map (kbd "C-c C-p") 'shannon-max-page-up)
	    (define-key map (kbd "C-c C-e") 'keymap-global-set)
	    map))

(defun shannon-max-analyze ()
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
      (let* ((process-name "shannon-max-process1")
	     (compute-freqs-process (start-process process-name shannon-max-process-buffer
						   "java" "-jar" shannon-max-jar-file shannon-max-keylog-file-name)))
	(set-process-sentinel (get-process process-name)
			      (lambda (process event)
				(shannon-max-display-shannon-output (shannon-max-commands-from-process-output shannon-max-process-buffer))))))))


		

;; (shannon-max-run-proces-and-display-results)
(provide 'shannon-max)
