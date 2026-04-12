# Guix Home Configuration Project Context

## Project Overview
This directory contains a declarative personal system configuration managed by Guix Home. It provides a reproducible, complete Wayland-based desktop environment.
- **Core Technology:** GNU Guix (Guix Home), Guile Scheme.
- **Desktop Environment:** Sway (Wayland window manager), Waybar (status bar), Foot (terminal), Wofi (launcher).
- **Editor:** Emacs (configured via `use-package` with Evil mode, Org Roam, and Treesitter).
- **Service Management:** GNU Shepherd (managed by Guix Home).
- **Custom Integrations:** Includes custom services like `swayidle` setup and an `mcron` job for automatically downloading and rotating Reddit wallpapers.

## Building and Applying Configuration
To apply this configuration to the user profile:
```bash
guix home reconfigure home-config.scm
```
There is also a bash alias configured (`guix-home-update`) that handles updating Guix and applying the configuration over a VPN.

## Architecture & Conventions
- **`home-config.scm`:** The central configuration file. It defines the `home-environment`, specifying packages to install, services to run, and file mappings for dotfiles.
- **Dotfiles Organization:** Application-specific configurations (e.g., Sway, Waybar, Emacs) are organized into their respective subdirectories (e.g., `sway/`, `waybar/`, `emacs/`). These are mapped into the user's home directory using Guix's `local-file` mechanism within `home-config.scm`.
- **Custom Packages:** The configuration relies on a local custom package collection located at `~/guix-packages`. The load path is explicitly updated in `home-config.scm` to include this directory.
- **Code Style:** Configuration is written in Guile Scheme. When modifying `home-config.scm`, ensure idiomatic Scheme formatting and structural consistency.
