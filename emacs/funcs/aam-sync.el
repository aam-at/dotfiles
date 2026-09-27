;;; aam-sync.el --- Resolve Dropbox sync conflicts in ediff -*- lexical-binding: t; -*-

;;; Commentary:
;; windots' scripts/Sync-Dropbox.ps1 syncs ~/Dropbox with rclone bisync.  When
;; both sides changed a file, the newer keeps its name and the other becomes
;; NAME.conflictN (appended, so org-roam and the agenda never read it as a
;; note).  `aam/sync-resolve-conflicts' walks those one at a time in ediff:
;; A is the conflict copy, B the file that won.  Merge into B with ediff's
;; usual keys (a/b copy a difference across, n/p move) and save; quitting
;; ediff (q) offers to delete the copy and goes on to the next.

;;; Code:

(require 'ediff)

(defvar aam/sync-root "~/Dropbox/"
  "Root of the rclone-synced Dropbox tree.")

(defconst aam/sync--copy-regexp "\\.conflict[0-9]+\\'"
  "Matches the suffix rclone bisync gives the losing side of a conflict.")

(defun aam/sync--conflicts ()
  "Return (WINNER . COPY) pairs for every NAME.conflictN under `aam/sync-root'."
  (let* ((root (expand-file-name aam/sync-root))
         (copies (if (executable-find "fd")
                     (process-lines "fd" "--hidden" "--no-ignore" "--type" "f"
                                    "--exclude" "node_modules" "--absolute-path"
                                    "\\.conflict[0-9]+$" root)
                   (directory-files-recursively root aam/sync--copy-regexp nil nil t))))
    (mapcar (lambda (copy)
              (cons (replace-regexp-in-string aam/sync--copy-regexp "" copy) copy))
            copies)))

;;;###autoload
(defun aam/sync-resolve-conflicts ()
  "Walk the Dropbox sync conflicts in ediff, one at a time."
  (interactive)
  (aam/sync--next (aam/sync--conflicts)))

(defun aam/sync--next (pairs)
  "Ediff the first of PAIRS, then carry on with the rest."
  (if (null pairs)
      (message "No Dropbox sync conflicts left")
    (pcase-let ((`(,winner . ,copy) (car pairs)))
      (if (file-exists-p winner)
          (ediff-files copy winner
                       (list (lambda ()
                               ;; Appended after `t', so ediff's own cleanup runs first.
                               (add-hook 'ediff-quit-hook
                                         (lambda () (aam/sync--done copy winner (cdr pairs)))
                                         t t))))
        ;; The winner was deleted or renamed since: nothing to merge into.
        (when (yes-or-no-p (format "%s has no original; restore it as %s? "
                                   (file-name-nondirectory copy)
                                   (file-name-nondirectory winner)))
          (rename-file copy winner))
        (aam/sync--next (cdr pairs))))))

(defun aam/sync--done (copy winner rest)
  "After ediff on COPY and WINNER: offer to save and drop COPY, then do REST."
  (let ((buf (find-buffer-visiting winner)))
    (when (and buf (buffer-modified-p buf)
               (y-or-n-p (format "Save %s? " (file-name-nondirectory winner))))
      (with-current-buffer buf (save-buffer))))
  (when (y-or-n-p (format "Delete the conflict copy %s? " (file-name-nondirectory copy)))
    (let ((buf (find-buffer-visiting copy)))
      (when buf (kill-buffer buf)))
    (delete-file copy))
  ;; Start the next ediff once this one has fully quit.
  (run-at-time 0 nil #'aam/sync--next rest))

(provide 'aam-sync)
;;; aam-sync.el ends here
