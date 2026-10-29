from wallpaper_downloader.main import get_extension


def test_extension_comes_from_the_mimetype():
    assert get_extension("image/jpeg") == ".jpeg"
    assert get_extension("image/png") == ".png"


def test_search_url_filters_by_the_chosen_category():
    from wallpaper_downloader.main import build_search_url

    assert "categories=100&" in build_search_url("GENERAL")
    assert "categories=010&" in build_search_url("ANIME")


def test_search_url_adds_query_and_color_only_when_given():
    from urllib.parse import urlparse, parse_qs
    from wallpaper_downloader.main import build_search_url

    plain = parse_qs(urlparse(build_search_url("GENERAL")).query)
    assert "q" not in plain and "colors" not in plain

    filtered = parse_qs(urlparse(build_search_url("GENERAL", "space", "000000")).query)
    assert filtered["q"] == ["space"]
    assert filtered["colors"] == ["000000"]


def test_search_makes_one_request_per_query_and_color_without_duplicates(monkeypatch):
    import logging
    from wallpaper_downloader import main

    urls = []

    class FakeResponse:
        def json(self):
            return {"data": [{"id": "abc123", "path": "http://x/abc123.png", "file_type": "image/png"}]}

    monkeypatch.setattr(main.requests, "get",
                        lambda url, timeout: urls.append(url) or FakeResponse())

    result = main.search_wallpapers(logging.getLogger(), "GENERAL",
                                    ["nature", "space"], ["000000", "424153"])

    assert len(urls) == 4
    assert [w["file_name"] for w in result] == ["abc123.png"]


def test_search_url_defaults_to_newest_first_without_top_range():
    from urllib.parse import urlparse, parse_qs
    from wallpaper_downloader.main import build_search_url

    params = parse_qs(urlparse(build_search_url("GENERAL")).query)
    assert params["sorting"] == ["date_added"]
    assert "topRange" not in params and "page" not in params


def test_search_url_toplist_carries_range_and_page():
    from urllib.parse import urlparse, parse_qs
    from wallpaper_downloader.main import build_search_url

    params = parse_qs(urlparse(
        build_search_url("GENERAL", sorting="toplist", top_range="1y", page=2)).query)
    assert params["sorting"] == ["toplist"]
    assert params["topRange"] == ["1y"]
    assert params["page"] == ["2"]


def test_search_requests_every_page_of_every_query_and_color(monkeypatch):
    import logging
    from urllib.parse import urlparse, parse_qs
    from wallpaper_downloader import main

    urls = []

    class FakeResponse:
        def json(self):
            n = len(urls)
            return {"data": [{"id": f"id{n}", "path": "http://x/a.png", "file_type": "image/png"}]}

    monkeypatch.setattr(main.requests, "get",
                        lambda url, timeout: urls.append(url) or FakeResponse())

    result = main.search_wallpapers(logging.getLogger(), "GENERAL", ["a", "b"], ["000000"],
                                    sorting="toplist", top_range="1y", pages=3)

    assert len(urls) == 6
    pages = sorted(parse_qs(urlparse(u).query).get("page", ["1"])[0] for u in urls)
    assert pages == ["1", "1", "2", "2", "3", "3"]
    assert len(result) == 6


def test_download_skips_files_that_already_exist(tmp_path, monkeypatch):
    import logging
    from wallpaper_downloader import main

    target = tmp_path / "abc.png"
    target.write_bytes(b"old")
    monkeypatch.setattr(main.requests, "get",
                        lambda *a, **k: (_ for _ in ()).throw(AssertionError("no debe descargar")))

    main.download_wallpaper("http://x/abc.png", str(target), logging.getLogger())

    assert target.read_bytes() == b"old"
