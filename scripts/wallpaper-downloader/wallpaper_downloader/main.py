import sys
import argparse
import hashlib
import logging
import os

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


def search_wallpapers(logger):
    response = requests.get(BASE_URL + "?categories=111&purity=100&atleast=3840x2160&ratios=16x9&sorting=date_added")
    response_json = response.json()
    rows = response_json["data"]

    return [{"url": row["path"],
             "file_name": row["id"] + get_extension(row["file_type"]) }
            for row in rows]


def download_wallpaper(url, output_path, logger):
    wallpaper = requests.get(url).content

    with open(output_path, "wb") as file_handle:
        file_handle.write(wallpaper)

    print({ "action": "DOWNLOAD_WALLPAPER", "url": url, "output_path": output_path})


def main():
    logger = logging.getLogger('spam_application')
    logger.setLevel(logging.DEBUG)

    parser = argparse.ArgumentParser()

    parser.add_argument("-o", "--output-path")
    args = parser.parse_args()

    if not args.output_path:
        print({ "action": "MISSING_COMMAND_LINE_ARG", "arg": "--output-path"}, file=sys.stderr)
        return 1

    wallpapers = search_wallpapers(logger)

    for wallpaper in wallpapers:
        download_wallpaper(wallpaper["url"],  args.output_path + wallpaper["file_name"], logger)


if __name__ == "__main__":
    sys.exit(main())
