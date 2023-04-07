#!/usr/bin/env python
import argparse
import getpass
import sys

import keyring
from serpapi import GoogleSearch


def get_citations(title):
    params = {
        "api_key": keyring.get_password("serpapi", getpass.getuser()),
        "engine": "google_scholar",
        "q": f'"{title}"',
        "hl": "en"
    }

    search = GoogleSearch(params)
    results = search.get_dict()
    search_result = results['organic_results'][0]

    if 'inline_links' not in search_result or 'cited_by' not in search_result[
            'inline_links']:
        print("Citations not found.")
        return

    cited_by = search_result['inline_links']['cited_by']['total']
    return cited_by


def main():
    parser = argparse.ArgumentParser(
        description="Get the number of citations for a given publication title."
    )
    parser.add_argument("title", help="The title of the publication")

    args = parser.parse_args()

    title = args.title

    citations = get_citations(title)
    print(citations)


if __name__ == "__main__":
    main()
