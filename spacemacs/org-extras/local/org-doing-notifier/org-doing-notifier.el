(require 'org-doing)

(defgroup org-doing-notifier nil
  "Customization group for org-doing-notifier functionality."
  :group 'org)

(defcustom org-doing-notifier-interval 30
  "Base time interval in minutes to run the shell command."
  :type 'integer
  :group 'org-doing-notifier)

(defcustom org-doing-notifier-interval-variation 5
  "Variation in minutes to add or subtract from the base interval."
  :type 'integer
  :group 'org-doing-notifier)

(defcustom org-doing-notifier-command "what_am_i_doing.sh"
  "Shell command to execute for getting current activity."
  :type 'string
  :group 'org-doing-notifier)

(defcustom org-doing-notifier-enabled nil
  "Whether the org-doing-notifier is currently active."
  :type 'boolean
  :group 'org-doing-notifier)

(defvar org-doing-notifier-timer nil
  "Store the timer object for org-doing-notifier.")

(defun org-doing-notifier--calculate-interval ()
  "Calculate a random interval in seconds based on the base interval and variation."
  (let* ((variation (random (* 2 org-doing-notifier-interval-variation)))
         (interval-minutes (+ org-doing-notifier-interval
                              (- variation org-doing-notifier-interval-variation))))
    (* interval-minutes 60)))

(defun org-doing-notifier--execute-command ()
  "Execute the org-doing command and handle errors."
  (condition-case err
      (let ((output (string-trim (shell-command-to-string org-doing-notifier-command))))
        (unless (string-empty-p output)
          (org-doing output)))
    (error
     (message "Error in org-doing-notifier: %s" err)
     (org-doing-notifier-stop))))

(defun org-doing-notifier--schedule-next ()
  "Schedule the next execution of the notifier."
  (when org-doing-notifier-enabled
    (let ((interval (org-doing-notifier--calculate-interval)))
      (setq org-doing-notifier-timer
            (run-at-time interval nil
                         (lambda ()
                           (org-doing-notifier--execute-command)
                           (org-doing-notifier--schedule-next)))))))

;;;###autoload
(defun org-doing-notifier-start ()
  "Start the org-doing-notifier."
  (interactive)
  (when org-doing-notifier-timer
    (cancel-timer org-doing-notifier-timer))
  (setq org-doing-notifier-enabled t)
  (org-doing-notifier--schedule-next)
  (message "Org-doing-notifier started"))

;;;###autoload
(defun org-doing-notifier-stop ()
  "Stop the org-doing-notifier."
  (interactive)
  (setq org-doing-notifier-enabled nil)
  (when org-doing-notifier-timer
    (cancel-timer org-doing-notifier-timer)
    (setq org-doing-notifier-timer nil))
  (message "Org-doing-notifier stopped"))

;;;###autoload
(defun org-doing-notifier-toggle ()
  "Toggle org-doing-notifier on/off."
  (interactive)
  (if org-doing-notifier-enabled
      (org-doing-notifier-stop)
    (org-doing-notifier-start)))

(provide 'org-doing-notifier)
