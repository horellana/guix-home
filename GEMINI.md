# Guix Home Configuration

This directory contains the declarative system configuration for a user environment managed by [Guix Home](https://guix.gnu.org/manual/en/html_node/Home-Configuration.html). It sets up a complete Wayland-based desktop environment centered around the Sway window manager, along with development tools, Emacs configuration, and various utilities.

## Project Overview

*   **Type:** System Configuration (Dotfiles)
*   **Manager:** GNU Guix (Guix Home)
*   **Desktop Environment:** Sway (Wayland)
*   **Key Components:** Sway, Waybar, Wofi, Foot, Emacs, GPG, SSH.

The configuration uses Scheme `(home-config.scm)` to define packages and services, and deploys standard dotfiles (e.g., `.config/sway/config`) from the local directory to the user's home directory.

## Prerequisites

*   **GNU Guix:** Must be installed on the host system.
*   **External Modules:** The configuration expects a directory at `~/guix-packages` containing custom modules:
    *   `(my-packages reddit-image-downloader)`
    *   `(my-scripts set-wallpaper)`
    
    Ensure this directory exists and is populated, or adjust the `add-to-load-path` in `home-config.scm`.

## Directory Structure

*   `home-config.scm`: The main entry point. Defines the `home-environment`, including all packages, services (mcron, bash, gpg, ssh), and file mappings.
*   `sway/`: Configuration for the Sway window manager (`sway.conf`).
*   `waybar/`: Configuration (`config`) and styling (`style.css`) for the Waybar status bar, plus scripts.
*   `emacs/`: Emacs initialization file (`init.el`).
*   `foot/`, `tmux/`, `wofi/`, `gtk/`: Configurations for the Foot terminal, Tmux, Wofi launcher, and GTK theming.
*   `gnupg/`: GnuPG configuration files.
*   `.gitignore`: Git ignore patterns.

## Usage

To apply this configuration to your user profile, run the following command from this directory:

```bash
guix home reconfigure home-config.scm
```

This command will:
1.  Download and build specified packages.
2.  Start defined services (e.g., `gpg-agent`, `mcron`).
3.  Symlink configuration files from the store to your home directory (e.g., `~/.config/sway/config`).

## Key Configuration Details

### Window Manager (Sway)
*   **Mod Key:** `Mod4` (Super/Windows key).
*   **Terminal:** `foot`.
*   **Launcher:** `wofi`.
*   **Keybindings:** Standard vim-like navigation (`h`, `j`, `k`, `l`).
*   **Wallpapers:** Managed via `random-wallpaper` script and an `mcron` job that fetches images from Reddit.

### Emacs
*   Configured via `emacs/init.el` and a generated `guix-config.el`.
*   Uses `use-package` for package management (though packages are installed via Guix).
*   Includes configuration for Evil mode, Org Roam, Magit, and Treesitter.

### Services
*   **Mcron:** Scheduled jobs for wallpaper updates.
*   **GPG/SSH:** Configured with `pinentry-gnome3` and SSH support.
*   **Environment:** Sets necessary Wayland environment variables (e.g., `MOZ_ENABLE_WAYLAND`, `XDG_SESSION_TYPE`).
