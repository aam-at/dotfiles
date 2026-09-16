;; -*- lexical-binding: t; -*-
;; This file configures email for use.

;;;###autoload
(defun aam/mail-setup ()
  (setq mu4e-mu-home "~/.cache/mu"
        mu4e-sent-folder "/sent"
        mu4e-refile-folder "/archive"
        mu4e-drafts-folder "/drafts"
        mu4e-trash-folder "/trash"
        mu4e-attachment-dir "~/Downloads"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 600
        ;; give me ISO(ish) format date-time stamps in the header list
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-compose-signature t
        mu4e-compose-signature-auto-include nil
        mu4e-compose-dont-reply-to-self t
        mu4e-view-show-images t
        mu4e-show-images t
        mu4e-view-image-max-width 800
        mu4e-view-show-addresses t
        ;; setup queue mail dir
        smtpmail-queue-dir (concat mu4e-mu-home "/queue")
        message-kill-buffer-on-exit t)

  ;; See: http://emacs.stackexchange.com/questions/3051/how-can-i-use-eww-as-a-renderer-for-mu4e
  (defun aam/mu4e-render-html-message ()
    "Replacement for standard html2text using shr."
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (erase-buffer)
      (shr-insert-document dom)
      (goto-char (point-min))))
  (setq mu4e-html2text-command 'aam/mu4e-render-html-message)
  ;; (setq mu4e-html2text-command "w3m -dump -T text/html")
  (setq mu4e-view-prefer-html t)
  (require 'mu4e-view)
  (add-to-list 'mu4e-view-actions
               '("xView with xwidget" . mu4e-action-view-with-xwidget))

  ;; org-mu4e settings
  (setq org-mu4e-convert-to-html t)

  ;; customize the reply-quote-string
  ;; M-x find-function RET message-citation-line-format for docs
  (setq message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n")
  (setq message-citation-line-function 'message-insert-formatted-citation-line)

  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
        '((:date          .  25)
          (:flags         .   6)
          (:from          .  22)
          (:subject       .  nil)))
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed and NOT maildir:\"/*Trash*\" and NOT maildir:\"/*Delete*\" and NOT maildir:\"/*Spam*\" and NOT maildir:\"/*Junk*\"" "Unread messages" ?u)
          ("flag:new and NOT flag:trashed and NOT maildir:\"/*Trash*\" and NOT maildir:\"/*Delete*\" and NOT maildir:\"/*Spam*\" and NOT maildir:\"/*Junk*\""    "New messages"    ?n)
          ("maildir:\"/*INBOX*\""                       "Inbox's messages"     ?i)
          ("date:today..now"                            "Today's messages"     ?t)
          ("date:7d..now"                               "Last 7 days"          ?w)
          ("maildir:\"/INBOX\" and flag:flagged"        "Flagged in INBOX"     ?f)
          ("mime:image/*"                               "Messages with images" ?p)
          ("maildir:\"/*Spam*\" or maildir:\"/*Junk*\"" "Spam messages"        ?s)))
  (setq mu4e-user-mail-address-list '("alexander.matyasko@gmail.com"
                                      "aliaksan001@e.ntu.edu.sg"
                                      "a103596@singaporetech.edu.sg"
                                      "amatyasko@e.ntu.edu.sg"
                                      "alex.m@nus.edu.sg"
                                      "alexander.matyasko@yandex.ru"))

  ;; mu4e-maildir
  (setq mu4e-maildir-shortcuts
        '(("/gmail/INBOX"  . ?g)
          ("/student/INBOX" . ?c)
          ("/sit/INBOX"   . ?w)
          ("/yandex/INBOX" . ?y)))

  ;; mail account list: a message matches a context by maildir or recipient
  (let ((signature "Best regards,\nAlexander Matyasko\n"))
    (setq mu4e-contexts
          (mapcar
           (pcase-lambda (`(,name ,address ,maildir ,sent ,drafts ,trash ,refile
                                  ,smtp-server ,smtp-port ,signature))
             (make-mu4e-context
              :name name
              :enter-func (lambda () (mu4e-message "Switching to %s mail" name))
              :match-func
              (lambda (msg)
                (when msg
                  (or (string-prefix-p maildir (mu4e-message-field msg :maildir))
                      (mu4e-message-contact-field-matches msg :to address))))
              :vars
              `((user-mail-address . ,address)
                (user-full-name . "Alexander Matyasko")
                (mu4e-sent-folder . ,sent)
                (mu4e-drafts-folder . ,drafts)
                (mu4e-trash-folder . ,trash)
                ,@(when refile `((mu4e-refile-folder . ,refile)))
                (mu4e-compose-signature . ,signature)
                (message-send-mail-function . smtpmail-send-it)
                (smtpmail-stream-type . starttls)
                (smtpmail-smtp-server . ,smtp-server)
                (smtpmail-smtp-service . ,smtp-port))))
           ;; SIT and NUS send through the local DavMail gateway.
           `(("Gmail" "alexander.matyasko@gmail.com" "/gmail"
              "/gmail/[Gmail]/Sent Mail" "/gmail/[Gmail]/Drafts" "/gmail/[Gmail]/Trash"
              "/gmail/[Gmail]/All Mail" "smtp.gmail.com" 587 ,signature)
             ("NTU" "aliaksan001@e.ntu.edu.sg" "/student"
              "/student/Sent Items" "/student/Drafts" "/student/Deleted Items"
              nil "smtp.office365.com" 587 ,signature)
             ("SIT" "a103596@singaporetech.edu.sg" "/sit"
              "/sit/Sent" "/sit/Drafts" "/sit/Trash" nil "localhost" 1025 ,signature)
             ("NUS" "alex.m@nus.edu.sg" "/nus"
              "/nus/Sent" "/nus/Drafts" "/nus/Trash" nil "localhost" 1025 ,signature)
             ("Yandex" "alexander.matyasko@yandex.ru" "/yandex"
              "/yandex/Sent" "/yandex/Drafts" "/yandex/Trash"
              nil "smtp.yandex.com" 587 "C уважением,\nАлександр Матяско\n")))))

  (setq mu4e-compose-context-policy 'ask-if-none
        mu4e-context-policy 'pick-first)
  ;; (mu4e/mail-account-reset)

  ;; email org-contacts
  (setq mu4e-org-contacts-file org-contacts-files)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)

  ;; message mode hooks
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  ;; confirmation
  (add-hook 'message-send-hook
            (lambda ()
              (unless (yes-or-no-p "Sure you want to send this?")
                (signal 'quit nil))))
  (with-eval-after-load 'org
    ;; TODO: replace orgstruct++
    ;; (add-hook 'message-mode-hook 'turn-on-orgstruct)
    ;; (add-hook 'message-mode-hook 'turn-on-orgstruct++)
    (add-hook 'message-mode-hook 'orgtbl-mode)
    (require 'org-mime)
    (setq org-mime-library 'mml)
    (add-hook 'message-mode-hook
              (lambda ()
                (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
    (add-hook 'org-mode-hook
              (lambda ()
                (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))
    (add-hook 'org-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c M-o") 'org-mime-subtree)))
    (add-hook 'org-mime-html-hook
              (lambda ()
                ;; (insert-file-contents "~/git/.emacs.d/personal/css/office.css")
                (org-mime-change-element-style
                 "blockquote" "border-left: 2px solid gray; padding-left: 4px;")
                (org-mime-change-element-style
                 "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                               "#E6E1DC" "#232323"))))))


(provide 'config-mail)
