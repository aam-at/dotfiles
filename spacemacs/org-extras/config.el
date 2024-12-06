(defvar org-journal-agenda-days nil)
(defvar org-journal-agenda-files nil)

(defvar scholarly-citations-output nil
  "Holds the output from the `scholarly-citations-process-sentinel` function.")

(defvar scholarly-default-method "manual"
  "Default method to fetch citations.")

(defconst scholarly-methods-alist
  '(("manual" . scholarly-manual-citations)
    ("jina" . scholarly-jina-citations)
    ("scholarly_no_proxy" . "scholarly_citations.py --proxy noproxy")
    ("scholarly_freeproxy" . "scholarly_citations.py --proxy freeproxy")
    ("scholarly_scrapper" . "scholarly_citations.py --proxy scrapper")
    ("serpapi_scrapper" . "serpapi_citations.py"))
  "Association list of methods to automatically fetch citations and
  corresponding python scripts.")

(defvar org-enable-gcal nil
  "If non-nil, org-gcal (https://github.com/nicoalphonse/org-gcal) is configured")

(defvar org-enable-doing-notifier nil
  "If non-nil, org-doing-notifier is enabled")

(defvar org-enable-delve nil
  "If non-nil, delve (https://github.com/publicimageltd/delve) is configured")
