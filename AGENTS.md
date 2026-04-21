# AI Agent Instructions for Guix Home Configuration

This document provides specialized instructions for AI agents working on this Guix Home configuration repository.

## 1. Project Context

- **Core Technology:** GNU Guix (Guix Home), Guile Scheme.
- **Workflow:** This is a declarative configuration. Changes to any dotfile must be reflected or mapped correctly in `home-config.scm`.
- **System Type:** Wayland-based desktop environment centered around Sway.

## 2. Mandatory Workflow

- **Atomic Changes:** Follow the project's atomic commit style. Group related changes (e.g., package additions and their corresponding service configurations) into single, logical commits.
- **Verification:** After modifying `home-config.scm`, the configuration should be validated. Although full `guix home reconfigure` might not always be possible in the agent environment, you should check for Scheme syntax errors and consistent package declarations.
- **Dotfile Mapping:** When adding new configuration files (e.g., in a new subdirectory), ensure they are correctly symlinked using `local-file` within the `home-files-service-type` in `home-config.scm`.

## 3. Configuration Conventions

- **Guile Scheme:** Adhere to idiomatic Guile Scheme formatting and structural consistency.
- **Package Grouping:** Packages are categorized into lists (e.g., `my-emacs-packages`, `my-xdg-packages`). Add new packages to the most relevant list.
- **Service Management:** Use GNU Shepherd for managing system-level and user-level services.
- **Variable Usage:** Prefer defined variables (e.g., `my-emacs-packages`) over inline lists for clarity and maintainability.

## 4. Specific Tooling Instructions

- **Emacs:** Configuration is handled via `use-package`. Ensure any new Emacs packages are also added to `my-emacs-packages` in `home-config.scm`.
- **Sway:** Volume/mic controls should use `wpctl` (WirePlumber). Ensure new keybindings are consistent with the existing layout.
- **Zathura:** Configuration for position restoration and SQLite database is mandatory for tracking reading progress.
- **Graphics:** Default to Vulkan (`WLR_RENDERER=vulkan`) where applicable for performance.

## 5. Security & Safety

- **No Secrets:** Never hardcode API keys, passwords, or private information. Use placeholders or external secret management tools (e.g., `password-store`).
- **Ignore Sensitive Files:** Ensure `.gitignore` is updated to exclude any potential local-only or sensitive configuration artifacts.
