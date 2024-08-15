#!/usr/bin/env python
import argparse
import re
import subprocess
import urllib.parse


class MultipleCitationMatchesError(Exception):
    pass


def get_citations(title):
    encoded_title = urllib.parse.quote(f'"{title}"')
    url = (
        f"https://r.jina.ai/https://scholar.google.com/scholar?q={encoded_title}&hl=en"
    )
    try:
        result = subprocess.run(["curl", "-s", url], capture_output=True, text=True)
        text = result.stdout

        # Find all matches of the "Cited by" pattern
        cited_by_matches = re.findall(r"Cited by (\d+)", text)

        if len(cited_by_matches) == 1:
            return int(cited_by_matches[0])
        elif len(cited_by_matches) > 1:
            raise MultipleCitationMatchesError(
                "Multiple 'Cited by' matches found. This may indicate an error in the response or multiple papers."
            )
        else:
            print("Citations not found.")
            return None
    except subprocess.CalledProcessError as e:
        print(f"Error executing curl: {e}")
        return None
    except MultipleCitationMatchesError as e:
        print(f"Error: {e}")
        return None
    except Exception as e:
        print(f"An error occurred: {e}")
        return None


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
