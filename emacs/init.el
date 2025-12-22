;;; -*- lexical-binding: t; -*-

;; --- 1. Load Guix Configuration ---
;; This loads the file where Guix injects the store paths
(load (expand-file-name "guix-config.el" user-emacs-directory) t)

;; --- 2. Startup Performance ---
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 10 1024 1024))
(setq native-comp-async-jobs-number 12)

;; ... (Keep your UI Tweaks here) ...

;; --- 3. Package Management (SIMPLIFIED) ---
;; We REMOVED all the 'package-archives' and 'package-install' code.
;; We just require use-package. Guix has already installed everything.
(require 'use-package)

(use-package envrc
  :config
  (envrc-global-mode))

;; ... (Keep your existing use-package blocks) ...
(use-package gcmh :init (gcmh-mode 1))
(use-package doom-themes :config (load-theme 'doom-ir-black t))
(use-package doom-modeline :defer t :init (doom-modeline-mode 1))
(use-package all-the-icons :after (doom-modeline) :if (display-graphic-p))

;; ... (Keep Vertico, Consult, etc.) ...
(use-package vertico :init (vertico-mode))
(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))
(use-package marginalia :init (marginalia-mode))
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<")) ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

(use-package embark
  :bind (("C-." . embark-act))
  :init (setq prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult)

(use-package corfu
  :bind
  (:map global-map
	("C-M-i" . completion-at-point))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; ... (Keep Org Roam) ...
(use-package org-roam
  :custom (org-roam-directory (file-truename "~/.emacs.d/org-roam"))
  :config (org-roam-db-autosync-enable))
(use-package consult-org-roam :after (org-roam) :config (consult-org-roam-mode 1))

;; ... (Keep Programming Tools) ...
(use-package flymake-flycheck :hook ((flymake-mode . flymake-flycheck-auto)))
(use-package eglot :defer t)
(use-package eldoc-box :after eldoc :bind (("C-c K" . eldoc-box-help-at-point)))
(use-package smartparens-config :ensure nil :init (smartparens-global-mode))
(use-package yasnippet :config (yas-global-mode 1))
(use-package yasnippet-snippets :after (yasnippet))
(use-package which-key :init (which-key-mode))

;; ... (Keep Evil Mode) ...
(use-package evil :init (evil-mode 1))
(use-package evil-leader
  :after evil
  :config
  (evil-leader/set-key "w" 'save-buffer)
  (global-evil-leader-mode))
(use-package evil-commentary :after evil :hook (prog-mode . evil-commentary-mode))
(use-package evil-org :hook ((org-mode . evil-org-mode))
  :config (require 'evil-org-agenda) (evil-org-agenda-set-keys))

;; ... (Keep Project Tools) ...
(use-package project :defer t)
(use-package magit :commands (magit-status))
(use-package plantuml-mode)
(use-package jsdoc :commands (jsdoc))

;; --- Guile & Guix Development Configuration ---

(use-package geiser
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-default-implementation 'guile)
  ;; Stop Geiser from asking which implementation to use every time
  (setq geiser-active-implementations '(guile))
  ;; Use a slightly longer timeout for Guix operations which can be slow
  (setq geiser-connection-timeout 30))

(use-package geiser-guile
  :after geiser
  :config
  ;; CRITICAL: Add the Guix Pull profile to Geiser's load path.
  ;; This allows Geiser to find modules like (gnu packages ...) and (gnu services ...).
  (add-to-list 'geiser-guile-load-path 
	       "~/.config/guix/current/share/guile/site/3.0")
  
  ;; Add the compiled bytecode path (speeds up operations significantly)
  (add-to-list 'geiser-guile-load-path 
	       "~/.config/guix/current/lib/guile/3.0/site-ccache"))

(use-package scheme
  :ensure nil ;; Built-in
  :hook
  ;; Enable indentation and Paredit/Smartparens
  (scheme-mode . smartparens-mode) 
  ;; Enable Semantic highlighting (optional, helps with variables)
  (scheme-mode . font-lock-mode))

;; --- Emacs-Guix Configuration ---
;; You already have the package installed in home-config.scm.
;; This provides specific Guix commands and "pretty" modes.
(use-package emacs-guix
  :hook 
  ;; Augment Scheme mode with Guix-specific highlighting (e.g., packages in bold)
  (scheme-mode . guix-prettify-mode)
  :config
  (setq guix-directory "~/.config/guix/current"))

(use-package flycheck-guile
  :after (flycheck geiser-guile)
  :config
  ;; Sync Flycheck paths with Geiser paths so it can find Guix modules
  (setq flycheck-guile-library-path 
        (append geiser-guile-load-path
                '("~/.config/guix/current/share/guile/site/3.0"
                  "~/.config/guix/current/lib/guile/3.0/site-ccache"))))

;; --- 4. PlantUML (UPDATED) ---
(eval-after-load "org"
  '(progn
     (setq org-log-done 'time
	   org-latex-compiler "xelatex")
     (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
     
     ;; CHANGED: Use the variable provided by guix-config.el
     (setq org-plantuml-jar-path guix-plantuml-jar-path)))

(eval-after-load "go-ts-mode"
  '(progn
     (setq go-ts-mode-indent-offset 2)
     (add-hook 'before-save-hook #'eglot-format-buffer-on-save)))

(setq use-short-answers t)
(setq js-indent-level 2)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(setq inhibit-startup-message t)
(set-fringe-mode 0)
(set-face-attribute 'fringe nil :foreground (face-foreground 'default) :background (face-background 'default))
(setq frame-resize-pixelwise t)
(add-hook 'window-setup-hook #'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(undecorated . t))
(set-frame-font "DejaVu Sans Mono 14" nil t)

;; ... (Keep your File Behavior here) ...
(setq make-backup-files nil
      create-lockfiles nil
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 1
      kept-old-versions 2
      version-control t
      backup-directory-alist '((".*" . "~/.emacs.d/saves")))

(defun horellana/toggle-frame-transparency ()
  "Toggle transparency of the background (PGTK specific)."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha-background)))
    ;; Toggle between 100 and 90. If it's nil, assume it's 100.
    (set-frame-parameter
     nil 'alpha-background
     (if (or (not alpha) (= alpha 100))
	 80
       100))))

(horellana/toggle-frame-transparency)

(provide 'init)
