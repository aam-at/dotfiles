#!/usr/bin/env python
import argparse
import getpass
from enum import Enum

import keyring
from scholarly import ProxyGenerator, scholarly


class ProxyType(Enum):
    Noproxy = "Noproxy"
    Freeproxy = "Freeproxy"
    Scrapper = "Scrapper"


def proxy_type(value):
    try:
        return ProxyType[value.strip().title()]
    except KeyError:
        raise argparse.ArgumentTypeError(
            f"Invalid proxy type: '{value}'. Allowed values are {', '.join([e.name for e in ProxyType])}"
        )


def get_citations(title, proxy_type):
    pg = ProxyGenerator()

    if proxy_type == ProxyType.Freeproxy:
        success = pg.FreeProxies()
    elif proxy_type == ProxyType.Scrapper:
        success = pg.ScraperAPI(
            keyring.get_password("scrapperapi", getpass.getuser()))

    if proxy_type != ProxyType.Noproxy:
        scholarly.use_proxy(pg)

    search_query = scholarly.search_pubs(title)
    try:
        pub = next(search_query)
        return pub['num_citations']
    except StopIteration:
        return 0


def main():
    parser = argparse.ArgumentParser(
        description="Get the number of citations for a given publication title."
    )
    parser.add_argument("--proxy",
                        type=proxy_type,
                        choices=list(ProxyType),
                        default=ProxyType.Noproxy,
                        help="The proxy type to use (default: Noproxy)")
    parser.add_argument("title", help="The title of the publication")

    args = parser.parse_args()

    title = args.title
    proxy = args.proxy

    citations = get_citations(title, proxy)
    print(citations)


if __name__ == "__main__":
    main()
