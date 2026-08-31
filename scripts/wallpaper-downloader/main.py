import logging

import argparse
import json

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

    wallpapers = search_wallpapers(logger)

    for wallpaper in wallpapers:
        download_wallpaper(wallpaper["url"], "./test_output/" + wallpaper["file_name"], logger)


if __name__ == "__main__":
    main()
