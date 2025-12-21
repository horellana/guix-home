# Guix Home Configuration

This repository contains my personal declarative system configuration managed by [Guix Home](https://guix.gnu.org/manual/en/html_node/Home-Configuration.html). It provides a reproducible, complete Wayland-based desktop environment centered around Sway.

## Overview

- **Window Manager:** Sway (Wayland)
- **Status Bar:** Waybar
- **Terminal:** Foot
- **Launcher:** Wofi
- **Editor:** Emacs (Evil mode, Org Roam, Treesitter)
- **Service Manager:** GNU Shepherd (managed by Guix Home)

## Prerequisites

1.  **GNU Guix**: Ensure Guix is installed on your system.
2.  **Custom Channels/Packages**: This configuration assumes the existence of a local package collection at `~/guix-packages`. It specifically looks for:
    - `(my-packages reddit-image-downloader)`
    - `(my-scripts set-wallpaper)`

    You may need to adjust the `add-to-load-path` in `home-config.scm` if your custom packages are located elsewhere.

## Directory Structure

- `home-config.scm`: The core Guix Home configuration file defining packages, services, and file mappings.
- `sway/`: Sway configuration (`sway.conf`).
- `waybar/`: Waybar config, styling, and scripts.
- `emacs/`: Emacs initialization (`init.el`).
- `foot/`, `tmux/`, `wofi/`, `gtk/`: Configs for other core tools.
- `gnupg/`: GPG configuration.

## Installation

To apply this configuration to your user profile:

```bash
guix home reconfigure home-config.scm
```

This will:
- Install all defined packages (Sway, Emacs, fonts, utilities).
- Configure services (GPG agent, Mcron jobs, Bash).
- Symlink dotfiles from the Guix store to your home directory (e.g., `~/.config/sway/config`).

## Features

- **Automated Wallpapers**: An `mcron` job fetches wallpapers from Reddit and rotates them periodically.
- **Emacs Integration**: Packages are installed via Guix, but configuration is handled via `use-package` in `init.el`. Includes a generated `guix-config.el` for store paths.
- **Wayland Ready**: Sets up necessary environment variables like `MOZ_ENABLE_WAYLAND` and `GDK_BACKEND`.
- **Secrets**: GPG agent is configured with `pinentry-gnome3` and SSH support.
