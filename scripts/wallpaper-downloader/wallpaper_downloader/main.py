import sys
import argparse
import hashlib
import logging
import os
import itertools
from urllib.parse import urlencode

import requests

BASE_URL = "https://wallhaven.cc/api/v1/search"

CONTENT_FILTER = {
    "GENERAL": "100",
    "ANIME": "010",
    "PEOPLE": "001",
    "GENERAL+ANIME": "110",
    "ALL": "111"
}


def get_extension(mimetype):
    return "." + mimetype.split("/")[1]


def get_wallpaper_hashes(root_path):
    dirs = os.scandir(root_path)
    dirs = (entry for entry in dirs if entry.is_file())

    result = set()

    for file in dirs:
        with open(file) as fh:
            content = fh.readall()
            hashed = hashlib.md5(content)
            result.add(hashed)

    return result


def build_search_url(category, query=None, color=None,
                     sorting="date_added", top_range=None, page=1):
    params = {"categories": CONTENT_FILTER[category],
              "purity": "100",
              "atleast": "3840x2160",
              "ratios": "16x9",
              "sorting": sorting}
    if query:
        params["q"] = query
    if color:
        params["colors"] = color
    if sorting == "toplist" and top_range:
        params["topRange"] = top_range
    if page > 1:
        params["page"] = page
    return BASE_URL + "?" + urlencode(params)


def search_wallpapers(logger, category, queries, colors,
                      sorting="date_added", top_range=None, pages=1):
    # la API solo acepta un color y una consulta por peticion: una peticion por combinacion y pagina
    found = {}
    for query, color, page in itertools.product(queries or [None], colors or [None],
                                                range(1, pages + 1)):
        url = build_search_url(category, query, color, sorting, top_range, page)
        response = requests.get(url, timeout=30)
        for row in response.json()["data"]:
            found[row["id"]] = {"url": row["path"],
                                "file_name": row["id"] + get_extension(row["file_type"])}

    return list(found.values())


def download_wallpaper(url, output_path, logger):
    # con toplist los resultados casi no cambian entre ejecuciones: no volver a bajar lo que ya esta
    if os.path.exists(output_path):
        return

    wallpaper = requests.get(url, timeout=60).content

    with open(output_path, "wb") as file_handle:
        file_handle.write(wallpaper)

    print({ "action": "DOWNLOAD_WALLPAPER", "url": url, "output_path": output_path})


def main():
    logger = logging.getLogger('spam_application')
    logger.setLevel(logging.DEBUG)

    parser = argparse.ArgumentParser()

    parser.add_argument("-o", "--output-path")
    parser.add_argument("-c", "--category", choices=sorted(CONTENT_FILTER))
    parser.add_argument("-q", "--query", action="append", default=[])
    parser.add_argument("--color", action="append", default=[])
    parser.add_argument("--sorting", default="date_added",
                        choices=["date_added", "relevance", "random", "views",
                                 "favorites", "toplist", "hot"])
    parser.add_argument("--top-range", choices=["1d", "3d", "1w", "1M", "3M", "6M", "1y"])
    parser.add_argument("--pages", type=int, default=1)
    args = parser.parse_args()

    if not args.output_path:
        print({ "action": "MISSING_COMMAND_LINE_ARG", "arg": "--output-path"}, file=sys.stderr)
        return 1

    if not args.category:
        print({ "action": "MISSING_COMMAND_LINE_ARG", "arg": "--category"}, file=sys.stderr)
        return 1

    if args.pages < 1:
        print({ "action": "INVALID_COMMAND_LINE_ARG", "arg": "--pages"}, file=sys.stderr)
        return 1

    wallpapers = search_wallpapers(logger, args.category, args.query, args.color,
                                   args.sorting, args.top_range, args.pages)

    for wallpaper in wallpapers:
        download_wallpaper(wallpaper["url"],  args.output_path + wallpaper["file_name"], logger)


if __name__ == "__main__":
    sys.exit(main())
