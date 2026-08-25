(defvar org-journal-agenda-days nil)
(defvar org-journal-agenda-files nil)

(defvar aam-scholarly-citations-output nil
  "Holds the output from the `aam-scholarly-citations-process-sentinel` function.")

(defvar semantic-scholar-api-key nil
  "API key for Semantic Scholar citation lookups.
When nil, `aam-semantic-scholar-api-citations` uses the
SEMANTIC_SCHOLAR_API_KEY environment variable if set. If neither is
available, it sends an unauthenticated request.")

(defvar semantic-scholar-api-retry-seconds 5
  "Seconds to retry transient Semantic Scholar API failures.
Retries apply to `aam-semantic-scholar-api-citations` for rate limits,
server errors, and empty responses.")

(defvar scholarly-default-method "google scholar [manual]"
  "Default method to fetch citations.")

(defconst scholarly-methods-alist
  '(("google scholar [manual]" . aam-scholarly-manual-citations)
    ("google scholar [jina]" . aam-scholarly-jina-citations)
    ("google scholar [scholarly no proxy]" . "scholarly_citations.py --proxy noproxy")
    ("google scholar [scholarly freeproxy]" . "scholarly_citations.py --proxy freeproxy")
    ("google scholar [scholarly scrapper]" . "scholarly_citations.py --proxy scrapper")
    ("google scholar [serpapi scrapper]" . "serpapi_citations.py")
    ("semantic scholar [manual]" . aam-semantic-scholar-manual-citations)
    ("semantic scholar [api]" . aam-semantic-scholar-api-citations))
  "Association list of methods to automatically fetch citations and
  corresponding python scripts.")

(defvar org-enable-gcal nil
  "If non-nil, org-gcal (https://github.com/nicoalphonse/org-gcal) is configured")

(defvar org-enable-delve nil
  "If non-nil, delve (https://github.com/publicimageltd/delve) is configured")
