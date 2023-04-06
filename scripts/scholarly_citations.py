#!/usr/bin/env python
import sys

from scholarly import ProxyGenerator, scholarly


def get_citations(title):
    # NOTE: enable this to use proxy
    # pg = ProxyGenerator()
    # success = pg.FreeProxies()
    # success = pg.ScraperAPI("API_KEY")
    # scholarly.use_proxy(pg)
    search_query = scholarly.search_pubs(title)
    try:
        pub = next(search_query)
        return pub['num_citations']
    except StopIteration:
        return 0


title = sys.argv[1]
citations = get_citations(title)
print(citations)
