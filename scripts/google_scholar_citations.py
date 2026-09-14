#!/usr/bin/env python3
"""Print a paper's citation count from a selected scholarly index.

The default source is Google Scholar through SerpAPI. Direct Scholar scraping
(including through Jina Reader) is routinely blocked and is therefore not a
reliable API. OpenAlex is a credential-free alternative, but its citation
totals are from a different corpus.
"""

import argparse
import getpass
import json
import os
import sys
import urllib.error
import urllib.parse
import urllib.request
from typing import Any


SERPAPI_URL = "https://serpapi.com/search.json"
OPENALEX_URL = "https://api.openalex.org/works"
USER_AGENT = "google-scholar-citations/2.0 (+https://github.com/amatyasko/dotfiles)"


class CitationLookupError(RuntimeError):
    """The requested paper's citation count could not be retrieved safely."""


def normalise_title(title: str) -> str:
    """Make conservative comparisons of provider titles."""
    return " ".join(
        "".join(char.lower() if char.isalnum() else " " for char in title).split()
    )


def get_json(url: str, params: dict[str, str], timeout: float) -> dict[str, Any]:
    query = urllib.parse.urlencode(params)
    request = urllib.request.Request(
        f"{url}?{query}",
        headers={"Accept": "application/json", "User-Agent": USER_AGENT},
    )
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            return json.load(response)
    except urllib.error.HTTPError as error:
        body = error.read().decode("utf-8", errors="replace")
        detail = body[:300].replace("\n", " ")
        raise CitationLookupError(
            f"HTTP {error.code} from provider: {detail}"
        ) from error
    except (urllib.error.URLError, TimeoutError, json.JSONDecodeError) as error:
        raise CitationLookupError(f"Could not contact provider: {error}") from error


def get_serpapi_key(explicit_key: str | None) -> str:
    """Use an explicit key, environment variable, then the legacy keyring entry."""
    if explicit_key:
        return explicit_key
    if environment_key := os.environ.get("SERPAPI_API_KEY"):
        return environment_key

    try:
        import keyring

        keyring_key = keyring.get_password("serpapi", getpass.getuser())
    except ImportError:
        keyring_key = None

    if keyring_key:
        return keyring_key
    raise CitationLookupError(
        "SerpAPI needs a key. Set SERPAPI_API_KEY, pass --api-key, or install "
        "keyring and store it as service 'serpapi' for the current user."
    )


def get_serpapi_citations(title: str, api_key: str, timeout: float) -> int:
    data = get_json(
        SERPAPI_URL,
        {"engine": "google_scholar", "q": f'"{title}"', "hl": "en", "api_key": api_key},
        timeout,
    )
    if error := data.get("error"):
        raise CitationLookupError(f"SerpAPI: {error}")

    expected_title = normalise_title(title)
    for result in data.get("organic_results", []):
        if normalise_title(str(result.get("title", ""))) != expected_title:
            continue
        cited_by = result.get("inline_links", {}).get("cited_by", {}).get("total")
        if isinstance(cited_by, int):
            return cited_by
        if isinstance(cited_by, str) and cited_by.isdecimal():
            return int(cited_by)
        raise CitationLookupError(
            "Google Scholar found the paper but did not return a citation total."
        )

    raise CitationLookupError("No exact Google Scholar title match found.")


def get_openalex_citations(title: str, timeout: float) -> int:
    data = get_json(OPENALEX_URL, {"search": title, "per-page": "10"}, timeout)
    expected_title = normalise_title(title)
    for result in data.get("results", []):
        if normalise_title(str(result.get("title", ""))) == expected_title:
            cited_by = result.get("cited_by_count")
            if isinstance(cited_by, int):
                return cited_by
    raise CitationLookupError("No exact OpenAlex title match found.")


def get_citations(
    title: str,
    provider: str = "serpapi",
    api_key: str | None = None,
    timeout: float = 30.0,
) -> int:
    """Return the count from *provider*, refusing approximate title matches."""
    if provider == "serpapi":
        return get_serpapi_citations(title, get_serpapi_key(api_key), timeout)
    if provider == "openalex":
        return get_openalex_citations(title, timeout)
    raise ValueError(f"Unsupported provider: {provider}")


def main() -> int:
    parser = argparse.ArgumentParser(description="Get a paper's citation count.")
    parser.add_argument("title", help="Exact publication title")
    parser.add_argument(
        "--provider",
        choices=("serpapi", "openalex"),
        default="serpapi",
        help="Citation index (default: serpapi, which queries Google Scholar)",
    )
    parser.add_argument(
        "--api-key", help="SerpAPI key; overrides SERPAPI_API_KEY and keyring"
    )
    parser.add_argument(
        "--timeout", type=float, default=30.0, help="HTTP timeout in seconds"
    )
    args = parser.parse_args()

    if args.timeout <= 0:
        parser.error("--timeout must be positive")

    try:
        print(get_citations(args.title, args.provider, args.api_key, args.timeout))
    except CitationLookupError as error:
        print(f"citation lookup failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
