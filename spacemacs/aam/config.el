;; (setq org-caldav-calendars
;;       '((:calendar-id "Projects.org" :files ("~/Dropbox/Notes/Projects.org")
;;                       :inbox "~/Dropbox/Notes/fromwork.org")))
(setq org-caldav-calendars
      '((:calendar-id "work@whatever" :files ("~/org/work.org")
                      :inbox "~/org/fromwork.org")
        (:calendar-id "stuff@mystuff"
                      :files ("~/org/sports.org" "~/org/play.org")
                      :inbox "~/org/fromstuff.org")) )

