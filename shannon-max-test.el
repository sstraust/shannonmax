;;; -*- lexical-binding: t -*-

;; tests for shannon-max.el

(ert-deftest shannon-max-log2-test ()
  (should (= (shannon-max-log2 8) 3)))



;; NOTE: This filters commands with empty key sequences
;;   it may be good to investigate why some commands
;;   are reporting with keysequences that are empty
(ert-deftest shannon-max-run-jar-test ()
  (with-temp-buffer
  (let* ((test-process-name "shannon-max-proc-test")
	 (test-process-buffer (current-buffer))
	 (test-process (start-process
			test-process-name test-process-buffer
			"java" "-jar" shannon-max-jar-file
			"test/emacskeys/resources/example_keyfreqs_file1")))
    (while (process-live-p test-process)
      (accept-process-output test-process))
    (should (string= (with-current-buffer test-process-buffer
		 (buffer-string))
	       "ido-exit-minibuffer,5,[\" x b\"]
backward-char,1,[\" b\"]
god-mode-all,1,[\" ;\"]
ido-complete,1,[\" TAB\"]
self-insert-command,9,[\" g\" \" o\" \" l\" \" -\" \" s\" \" c\" \" a\" \" m\" \" e\"]
eval-last-sexp,1,[\" x e\"]

Process shannon-max-proc-test finished
")))))


(defmacro wrap-log-test (&rest test-contents)
  `(let* ((shannon-max-logged-events '())
	 (shannon-max-last-command-info nil)
	 (curr-buff (current-buffer))
	 (temp-buff (make-temp-name "scratch"))
	 (temp-file-name (make-temp-name "./test/emacskeys/resources/scratch"))
	 (shannon-max-keylog-file-name temp-file-name))
     (flet ((current-time () 1))
       (switch-to-buffer (get-buffer-create temp-buff))
       (unwind-protect
	   (progn
	     (use-local-map (make-sparse-keymap))
	     (use-local-map (make-sparse-keymap))
	     (local-set-key (kbd "C-n") 'next-line)
	     (local-set-key (kbd "C-p") 'previous-line)
	     (local-set-key (kbd "C-a C-a") 'forward-char)
	     (goto-char 1)
	     ,@test-contents)
	 (progn (switch-to-buffer curr-buff)
		(kill-buffer temp-buff)
		(delete-file temp-file-name))))))

(ert-deftest shannon-max-log-test ()
  (should
   ;; we only write to the log after the _NEXT_
   ;; event occurs, so the last command doesn't appear
   ;; in the log
   (equal (wrap-log-test
	   (insert "\n\n\n")
	   (goto-char 1)
	   (execute-kbd-macro (kbd "C-n"))
	   (execute-kbd-macro (kbd "C-p"))
	   shannon-max-logged-events)
	  '([next-line 1 fundamental-mode "C-n" nil (2 . 0)])))

  (should
   (equal
    (wrap-log-test
	   (insert "\n\n\n")
	   (goto-char 1)
	   (execute-kbd-macro (kbd "C-n"))
	   (execute-kbd-macro (kbd "C-a C-a"))
	   (execute-kbd-macro (kbd "C-p"))
	   (execute-kbd-macro (kbd "C-n"))
	   (execute-kbd-macro (kbd "C-a C-a"))
	   shannon-max-logged-events)
    ;; logged events are shown in reverse order
    '([next-line 1 fundamental-mode "C-n" nil (3 . 0)]
      [previous-line 1 fundamental-mode "C-p" nil (2 . 0)]
      [forward-char 1 fundamental-mode "C-a C-a" nil (3 . 0)]
      [next-line 1 fundamental-mode "C-n" nil (2 . 0)]))))

;; you want to set 
;; shannon-max-keylog-file-name
;; inside of wrap-log-test
;; and then verify that it holds the right value afterwards


;; then you want to call shannon-max-commands-from-process-output
;; and verify that it returns the right thing
(ert-deftest shannon-max-logger-save-test ()
  (wrap-log-test
   (insert "\n\n\n\n\n")
   (goto-char 1)
   (execute-kbd-macro (kbd "C-n"))
   (execute-kbd-macro (kbd "C-a C-a"))
   (execute-kbd-macro (kbd "C-p"))
   (execute-kbd-macro (kbd "C-n"))
   (execute-kbd-macro (kbd "C-a C-a"))
   
   (shannon-max-logger-save)

   ;; verify that the text contains the right value
   (should
    (equal
     (with-temp-buffer
	     (insert-file-contents shannon-max-keylog-file-name)
	     (buffer-string))
     "next-line, 1, fundamental-mode, C-n, nil, (3 0)
previous-line, 1, fundamental-mode, C-p, nil, (2 0)
forward-char, 1, fundamental-mode, C-a C-a, nil, (3 0)
next-line, 1, fundamental-mode, C-n, nil, (2 0)
")))

  ;; note that it does correectly log all the events, but they
  ;; are out of order, i.e. you need to sort the file by _TIMESTAMP_
  ;; and not rely on the inherent ordering of the file
  (wrap-log-test
   (insert "\n\n\n\n\n")
   (goto-char 1)
   (execute-kbd-macro (kbd "C-n"))
   (execute-kbd-macro (kbd "C-a C-a"))
   
   (shannon-max-logger-save)

   (execute-kbd-macro (kbd "C-p"))
   (execute-kbd-macro (kbd "C-p"))

   (shannon-max-logger-save)
   (should
    (equal 
     (with-temp-buffer
       (insert-file-contents shannon-max-keylog-file-name)
       (buffer-string))
    "next-line, 1, fundamental-mode, C-n, nil, (2 0)
previous-line, 1, fundamental-mode, C-p, nil, (2 0)
forward-char, 1, fundamental-mode, C-a C-a, nil, (3 0)
"))))


(defun shannon-max-test-keyfreqs-file-to-freqs (log-file)
    (with-temp-buffer
      (let* ((test-process-name "shannon-max-proc-test")
	     (test-process-buffer (current-buffer))
	     (test-process (start-process
			    test-process-name test-process-buffer
			    "java" "-jar" shannon-max-jar-file
			    log-file)))
	(while (process-live-p test-process)
	  (accept-process-output test-process))
	(shannon-max-commands-from-process-output
	 (current-buffer)
	 ))))

(ert-deftest shannon-max-parse-freqs-test ()
  (let ((freqs-result (shannon-max-test-keyfreqs-file-to-freqs "test/emacskeys/resources/example_keyfreqs_file2")))
    (should (equal freqs-result
		   '(#s(shannon-max-command-info "god-mode-all" 1 ((";")) 0.125) #s(shannon-max-command-info "backward-char" 3 (("b")) 0.375 ) #s(shannon-max-command-info "eval-last-sexp" 4 (("x" "e")) 0.5))))

    
    (should (equal (shannon-max-command-entropy freqs-result) 1.4056390622295662))))



(ert-deftest shannon-max-logger-toggle-test ()
  (let ((shannon-max-logger-enabled t)
	(post-command-hook '()))
    (shannon-max-start-logger)
    (should (equal post-command-hook '(shannon-max-logger-log)))
    (wrap-log-test
     (insert "\n\n\n\n\n")
     (goto-char 1)
     (execute-kbd-macro (kbd "C-n"))
     (execute-kbd-macro (kbd "C-a C-a"))
     (execute-kbd-macro (kbd "C-p"))
     (execute-kbd-macro (kbd "C-n"))
     (execute-kbd-macro (kbd "C-a C-a"))
     
     (shannon-max-logger-save)
     
     ;; verify that the text contains the right value
     (should
      (equal
       (with-temp-buffer
	 (insert-file-contents shannon-max-keylog-file-name)
	 (buffer-string))
       "next-line, 1, fundamental-mode, C-n, nil, (3 0)
previous-line, 1, fundamental-mode, C-p, nil, (2 0)
forward-char, 1, fundamental-mode, C-a C-a, nil, (3 0)
next-line, 1, fundamental-mode, C-n, nil, (2 0)
")))
    
    (shannon-max-logger-enabled-toggle)

    (should (equal shannon-max-logger-enabled nil))


    (wrap-log-test
     (insert "\n\n\n\n\n")
     (goto-char 1)
     (execute-kbd-macro (kbd "C-n"))
     (execute-kbd-macro (kbd "C-a C-a"))
     (execute-kbd-macro (kbd "C-p"))
     (execute-kbd-macro (kbd "C-n"))
     (execute-kbd-macro (kbd "C-a C-a"))

     (shannon-max-logger-save)

     ;; verify that the text contains the right value
     (should
      (equal
       (file-exists-p shannon-max-keylog-file-name)
       nil))
     (should (equal shannon-max-logged-events '()))
     (should (equal post-command-hook '()))

     (shannon-max-logger-enabled-toggle)
     (should (equal post-command-hook '(shannon-max-logger-log)))



     (execute-kbd-macro (kbd "C-n"))
     (execute-kbd-macro (kbd "C-a C-a"))
     (execute-kbd-macro (kbd "C-p"))
     (execute-kbd-macro (kbd "C-n"))
     (shannon-max-logger-save)

     (should
      (equal
       (with-temp-buffer
	 (insert-file-contents shannon-max-keylog-file-name)
	 (buffer-string))
       "previous-line, 1, fundamental-mode, C-p, nil, (5 0)
forward-char, 1, fundamental-mode, C-a C-a, nil, (6 0)
next-line, 1, fundamental-mode, C-n, nil, (5 0)
"))

     (shannon-max-logger-enabled-toggle)
     (execute-kbd-macro (kbd "C-p"))
     (execute-kbd-macro (kbd "C-p"))
     (execute-kbd-macro (kbd "C-p"))
     (execute-kbd-macro (kbd "C-p"))
     (shannon-max-logger-save)
     (should
      (equal
       (with-temp-buffer
	 (insert-file-contents shannon-max-keylog-file-name)
	 (buffer-string))
       "previous-line, 1, fundamental-mode, C-p, nil, (5 0)
forward-char, 1, fundamental-mode, C-a C-a, nil, (6 0)
next-line, 1, fundamental-mode, C-n, nil, (5 0)
")))))
