(defvar org-journal-agenda-days nil)
(defvar org-journal-agenda-files nil)

(defvar scholarly-citations-output nil
  "Holds the output from the `scholarly-citations-process-sentinel` function.")

(defconst scholarly-methods-alist
  '(("manual" . "")
    ("scholarly_no_proxy" . "scholarly_citations.py --proxy noproxy")
    ("scholarly_freeproxy" . "scholarly_citations.py --proxy freeproxy")
    ("scholarly_scrapper" . "scholarly_citations.py --proxy scrapper")
    ("serpapi_scrapper" . "serpapi_citations.py"))
  "Association list of methods to automatically fetch citations and
  corresponding python scripts.")

(defvar org-enable-calfw nil
  "If non-nil, calfw and org-calfw (https://github.com/kiwanami/emacs-calfw/) are configured")

(defvar org-enable-gcal nil
  "If non-nil, org-gcal (https://github.com/nicoalphonse/org-gcal) is configured")
