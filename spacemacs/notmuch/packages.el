(setq notmuch-packages
    '(
      notmuch
      notmuch-labeler
      ))

(setq notmuch-excluded-packages '())

(defun notmuch/init-notmuch()
  (use-package notmuch
    :defer t
    :init
    (spacemacs/set-leader-keys "aN" 'notmuch)
    :config
    (progn
      (setq notmuch-address-selection-function
            (lambda (prompt collection initial-input)
              (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))
      (evilified-state-evilify-map notmuch-hello-mode-map
        :mode notmuch-hello-mode
        :bindings
        "J" 'notmuch-jump-search)
      (evilified-state-evilify-map notmuch-search-mode-map
        :mode notmuch-search-mode
        :bindings
        "j" 'notmuch-search-next-thread
        "k" 'notmuch-search-previous-thread
        "J" 'notmuch-jump-search
        "n" 'notmuch-search-next-thread
        "gg" 'notmuch-search-first-thread
        "G" 'notmuch-search-last-thread
        "gr" 'notmuch-search-update-result
        "z" 'notmuch-tree
        "*" 'notmuch-search-tag-all
        "a" 'notmuch-search-archive-thread
        "-" 'notmuch-search-remove-tag
        "+" 'notmuch-search-add-tag)
      (evilified-state-evilify-map notmuch-show-mode-map
        :mode notmuch-show-mode
        :bindings
        "J" 'notmuch-jump-search
        "n" nil
        "nc" 'notmuch-show-stack-cc
        "n|" 'notmuch-show-pipe-message
        "nw" 'notmuch-show-save-attachments
        "nv" 'notmuch-show-view-raw-message)
    )))

(defun notmuch/init-notmuch-labeler()
  :defer t)
