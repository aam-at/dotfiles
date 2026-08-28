;;; write-or-die.el --- Delete words after prolonged idle time -*- lexical-binding: t; -*-

;; Version: 0.1a

;;; Commentary:
;;
;; An emacs version of Dr. Wicked's "Write-or-die" webapp, by Duncan Mortimer <dmorti@gmail.com>
;;
;; As soon as it is write-or-die-go is invoked, the time spent writing and the number of
;; words written are tracked.
;;
;; However, if you stop writing for some period of time, the buffer
;; changes colour to give you some warning, and then words are deleted
;; from the end of the document at a rate of 1 per second.  Beginning
;; writing again stops this.
;;
;; Please let me know if this is useful to you, or if you have any suggestions!

;;; History:
;;
;; - 2009-12-09 incorporated Tom Breton's suggested change to word-count (see wiki page)

;;; Code:

(require 'timer)

(define-minor-mode write-or-die-mode
  "A mode to keep you motivated to continue writing.  Based on
  'write-or-die' by Dr. Wicked (http://lab.drwicked.com/writeordie.html)."
  :lighter write-or-die-mode-text
  :group write-or-die
  (if write-or-die-mode
      (add-hook 'post-command-hook #'write-or-die-post-command-hook nil t)
    (progn
      (write-or-die-stop)
      (remove-hook 'post-command-hook #'write-or-die-post-command-hook t))))

;; Customisable variables
(defcustom write-or-die-target-words 500
  "The target number of words to be written after write-or-die-go is invoked."
  :group 'write-or-die)
(defcustom write-or-die-target-time 1200
  "The target time (in seconds) for which you want to write after
  write-or-die-go is invoked."
  :group 'write-or-die)
(defcustom write-or-die-progress-format " [%s of %s, TIMER: %s]"
  "A format string that controls how your current progress is
displayed"
  :group 'write-or-die)
(defcustom write-or-die-warning-period 10
  "Number of seconds of idleness before warning occurs."
  :group 'write-or-die)
(defcustom write-or-die-grace-period 20
  "Number of seconds of idleness before aversive stimulus occurs."
  :group 'write-or-die)

;; Working variables:
(defvar-local write-or-die-mode-text " ☠")

;; state = 0: off
;; state = 1: on and going well
;; state = 2: warning
;; state = 3: Zap!!
(defvar-local write-or-die-state 0)

;; number of words when 'write-or-die-go' most recently called
(defvar-local write-or-die-num-words-begin 0)

;; timers:
;;  write-or-die-timer:
;;     how long since 'w-o-d-go' called, with w-o-d-state = 0
;;  write-or-die-warning-timer:
;;     sets the state to 2 when w-o-d-warning-period seconds pass while idle.
;;  write-or-die-grace-timer:
;;     sets the state to 3 when w-o-d-grace-period seconds pass while idle.
(defvar-local write-or-die-timer nil)
(defvar-local write-or-die-warning-timer nil)
(defvar-local write-or-die-grace-timer nil)
(defvar-local write-or-die-warning-face-cookie nil)

(defvar-local write-or-die-time-so-far 0)

(defun write-or-die-update ()
  "This is called every second, and updates word count etc. / calls
warning routine or stimulus routine."
  (if (> write-or-die-state 0)
      (let (
	    (num-words-written
	     (- (write-or-die-word-count) write-or-die-num-words-begin))
	    (time-to-go
	     (- write-or-die-target-time write-or-die-time-so-far)))
	(setq write-or-die-time-so-far
	      (+ 1 write-or-die-time-so-far))
	(setq write-or-die-mode-text
	      (format write-or-die-progress-format
		      num-words-written
		      write-or-die-target-words
		      time-to-go))
	)
    (setq write-or-die-mode-text " ☠"))
  ;; If we're being warned about not concentrating on our writing....
  (if (> write-or-die-state 1)
      (write-or-die-warning)
    (write-or-die-clear-warning))
  ;; If we're being punished for not writing for too long!
  (if (> write-or-die-state 2)
      (write-or-die-stimulus))

  (force-mode-line-update)
  )

(defun write-or-die-post-command-hook ()
  "Used to reset the 'stimulus/warning' after you start typing again..."
  (if (> write-or-die-state 1)
      (setq write-or-die-state 1))
  )

(defun write-or-die-word-count ()
  "Count words in buffer"
  ;;Adapted from replace.el - Tehom
  (let
      ((regexp "\\w+")
       (rend (point-max)))
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)
	    opoint)
	(while (and
		(< (point) rend)
		(progn
		  (setq opoint (point))
		  (re-search-forward regexp rend t)))
	  (if (= opoint (point))
	      (forward-char 1)
	    (setq count (1+ count))))
	count))))

(defun write-or-die-go ()
  "Start incentivised writing!"
  (interactive)
  (unless write-or-die-mode
    (write-or-die-mode 1))
  (when (= 0 write-or-die-state)
    (let ((buffer (current-buffer)))
      (setq write-or-die-state 1)
      (setq write-or-die-num-words-begin (write-or-die-word-count))
      (setq write-or-die-time-so-far 0)
      (setq write-or-die-timer
	    (run-with-timer
	     0 1  ;; i.e. update once per second, starting NOW!
	     (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (write-or-die-update))))))
      (setq write-or-die-warning-timer
	    (run-with-idle-timer write-or-die-warning-period
				 1
				 (lambda ()
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (setq write-or-die-state 2))))))
      (setq write-or-die-grace-timer
	    (run-with-idle-timer write-or-die-grace-period
				 1
				 (lambda ()
				   (when (buffer-live-p buffer)
				     (with-current-buffer buffer
				       (setq write-or-die-state 3))))))))
  )

(defun write-or-die-toggle ()
  "Start a writing session, or stop and disarm the current one."
  (interactive)
  (if (> write-or-die-state 0)
      (write-or-die-mode -1)
    (write-or-die-go)))

(defun write-or-die-stop ()
  "Stop incentivised writing!"
  (interactive)
  (setq write-or-die-state 0)
  (dolist (timer (list write-or-die-timer
                       write-or-die-warning-timer
                       write-or-die-grace-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (setq write-or-die-timer nil
        write-or-die-warning-timer nil
        write-or-die-grace-timer nil)
  (write-or-die-clear-warning)
  (write-or-die-update)
  )

(defun write-or-die-warning ()
  (unless write-or-die-warning-face-cookie
    (setq write-or-die-warning-face-cookie
          (face-remap-add-relative 'default :background "Red"))))

(defun write-or-die-clear-warning ()
  "Remove the buffer-local warning face, if active."
  (when write-or-die-warning-face-cookie
    (face-remap-remove-relative write-or-die-warning-face-cookie)
    (setq write-or-die-warning-face-cookie nil)))

(defun write-or-die-stimulus ()
  (unless (bobp)
    (backward-kill-word 1)))

(provide 'write-or-die)
;;; write-or-die.el ends here
