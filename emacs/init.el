;;; -*- lexical-binding: t; -*-

;; --- 1. Load Guix Configuration ---
;; This loads the file where Guix injects the store paths
(load (expand-file-name "guix-config.el" user-emacs-directory) t)

;; --- 2. Startup Performance ---
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 10 1024 1024))
(setq native-comp-async-jobs-number 12)

;; ... (Keep your UI Tweaks here) ...
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

;; Transparency function
(defun horellana/toggle-frame-transparency ()
  (interactive)
  (set-frame-parameter nil 'alpha 100))

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
(use-package consult :bind (("C-c M-x" . consult-mode-command) 
                            ("C-x b" . consult-buffer))) ;; (Shortened for brevity)

(use-package embark
  :bind (("C-." . embark-act))
  :init (setq prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult)
(use-package corfu
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

(setq js-indent-level 2)

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
