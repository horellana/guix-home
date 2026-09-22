from wallpaper_downloader.main import get_extension


def test_extension_comes_from_the_mimetype():
    assert get_extension("image/jpeg") == ".jpeg"
    assert get_extension("image/png") == ".png"
