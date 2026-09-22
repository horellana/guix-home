# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Personal **GNU Guix Home** configuration for a Wayland/Sway desktop. It is declarative: a single Scheme file (`home-config.scm`) defines packages, Shepherd services, mcron jobs, environment variables, shell aliases, and the mapping of every dotfile into `$HOME`.

## Applying / testing changes

There is no build or test suite. The only way to apply and validate changes is to reconfigure the home environment:

```bash
guix home reconfigure home-config.scm
# alias defined in this config (also runs `guix pull` first):
guix-home-update
```

A reconfigure either succeeds (new generation activated) or fails with a Guile/Guix error — that error *is* the test feedback. To preview without activating, `guix home build home-config.scm` evaluates and builds the config without touching the live profile.

Roll back a bad generation with `guix home roll-back`; list generations with `guix home list-generations`.

## Critical architecture fact: dotfiles are store symlinks

The `dotfiles` and `guix-emacs-config` services (in `home-config.scm`) map files like `sway/sway.conf` → `~/.config/sway/config` using `local-file`. After activation these targets are **read-only symlinks into `/gnu/store`**.

Consequences when editing:
- Edit the source file in this repo (e.g. `sway/sway.conf`, `waybar/config`, `emacs/init.el`), **then `guix home reconfigure`**. Editing the `~/.config/...` symlink target directly is impossible (read-only store) and would be overwritten anyway.
- Adding a new dotfile is two steps: create the file here **and** add a `(".path/in/home" ,(local-file "dir/file"))` entry to the `dotfiles` simple-service list.
- The directory names here (`sway/`, `waybar/`, …) are arbitrary repo organization; the actual destination path is whatever the `dotfiles` entry says, not the directory name.

`emacs/init.el` is the exception in spirit: Emacs *packages* are installed via Guix (`my-emacs-packages`), but configuration is plain `use-package` in `init.el`. A generated `guix-config.el` (built by `guix-emacs-config`) injects store paths (e.g. the PlantUML jar) so `init.el` can reference store artifacts without hardcoding paths.

## External dependencies (config will not evaluate without them)

- None outside this repo: `home-config.scm` adds its own `guix-packages/` directory to the load path (derived from `(current-filename)`, so it works from any cwd). That directory holds `my-packages/` (`claude-code`, `governor`, `kotlin-lsp`, `kotlin-ts-mode`, `opencode`) and `my-scripts/` (`set-wallpaper`, exporting `random-wallpaper` / `random-wallpaper-script`). The old `~/guix-packages` copy is no longer read.
- **nonguix channel** is declared via `home-channels-service-type` and is required for `google-chrome-stable`, `(nongnu packages mozilla)` and the `claude-code` package (it uses `(nonguix licenses)`). Channel changes only take effect after a `guix pull`, not just a home reconfigure.

Package definitions live in `guix-packages/my-packages/` and Guix reads them from there directly. Each file's header comment documents how to bump the version/hash; for `claude-code` use `scripts/update-claude-code.sh` instead of editing by hand.

`wallpaper-downloader` is the other shape: a `package` defined inline in `home-config.scm` whose `source` is `(local-file "scripts/wallpaper-downloader" #:recursive? #t)`. Prefer that one for code that only this config uses; `guix-packages/my-packages/` is for definitions that wrap something external (a npm tarball, a JetBrains archive, an upstream git repo).

## Package organization

Packages are grouped into `define`d lists (`my-dev-packages`, `my-wm-packages`, `my-utils-packages`, `my-emacs-packages`, etc.) and `append`ed in the final `home-environment`. Add a package to the relevant list rather than the top-level append.

## Services worth knowing

Defined inline in `home-config.scm`:
- **swayidle** (`my-swayidle-service`): a hand-rolled `home-shepherd-service` with `auto-start? #f`. It discovers `SWAYSOCK` dynamically and runs lock/dpms/suspend timeouts. The lock action is `random-wallpaper-lock`, a `program-file` Guile script that picks a random image per output from `~/images/wallpapers/4k`.
- **pipewire / wireplumber / pipewire-pulse**: three Shepherd services started manually (this config does not use a desktop service that bundles them).
- **mcron jobs**: rotate the wallpaper every 15 min (`random-wallpaper-job`) and download fresh 4K wallpapers from the Wallhaven API every hour (`wallpaper-downloader-job`, all CLI flags passed from `home-config.scm`).
- **gpg-agent** with SSH support and `pinentry-gnome3`; **openssh** host config; **bash** service holds all aliases (`warp-on/off/status`, `steam`, the `*-update` aliases) plus `bashrc` snippets for direnv, gpg-agent and Claude Code.
- **governor-claude-plugin**: a `home-activation-service-type` service that runs `scripts/governor-setup.py` on every reconfigure to register the Governor plugin in `~/.claude/settings.json`, pointing `extraKnownMarketplaces` at the store path of the `governor` package. It merges the JSON idempotently, so a user-edited `settings.json` survives. This is the pattern to copy when something needs *mutable* state in `$HOME` that a read-only store symlink cannot provide.

When editing a Shepherd service, remember the gexp/staging split: `#$(file-append pkg "/bin/x")` resolves a store path at build time, `#~(...)` is runtime code. Mixing them up is the most common failure mode in this file.

## Conventions

- This file is read on a system where `git` lives in the user profile; it may not be on `PATH` in every non-login shell — invoke via full path or a login shell if a bare `git` fails.
- `.gitignore` excludes `*.db` (zathura's SQLite progress DB), Emacs lock/autosave junk (`*#*#`) and Rust build output (`target/`, `src/rust_out`).
- `scripts/` holds helper code that is *not* a dotfile: `governor-setup.py` (run by the activation service above) and `wallpaper-downloader/`, a Python package with its own `manifest.scm` + `.envrc` (direnv drops you into `guix shell`; `python3 -m pytest` for tests).
