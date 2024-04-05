;; -*- lexical-binding: t -*-

(defvar shannon-logged-events '())
(defvar shannon-last-command-info nil)

(defcustom shannon-keylog-file "~/emacs-logged-keys")

(defun set-last-command-info ()
  (when (and (key-description (this-command-keys-vector))
	     (not (= (length (key-description (this-command-keys-vector))) 0)))
    (setq shannon-last-command-info
	  (vector
	   (current-time)
	   major-mode
	   (key-description (this-command-keys-vector))
	   (buffer-file-name)
	   (cons (line-number-at-pos)
		 (current-column))))))


(defun shannon-logger-log ()
  (when shannon-last-command-info
    (push (vconcat (vector
		    real-last-command)
		   shannon-last-command-info)
	  shannon-logged-events)
    (setq shannon-last-command-info nil))
  (set-last-command-info))

;; (shannon-logger-save)
(defun shannon-logger-save ()
  (when shannon-logged-events
    (with-temp-buffer
      (dolist (k shannon-logged-events)
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
	  (append-to-file nil nil shannon-keylog-file)
	  (setq save-silently silent))
	(setq shannon-logged-events '()))))

(defun shannon-logger-autosave ()
  (setq shannon-logger-timer
	(run-with-idle-timer 2 t 'shannon-logger-save)))

;; (shannon-logger-save)
(defun shannon-start-logger ()
  (add-hook 'post-command-hook 'shannon-logger-log)
  (shannon-logger-autosave))
(prefer-coding-system 'utf-8)

(provide 'shannon-max)
