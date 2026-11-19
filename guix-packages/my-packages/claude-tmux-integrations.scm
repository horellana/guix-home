;;; Paquete Guix para el marketplace de plugins claude-tmux-integrations.
;;;
;;; Son scripts POSIX sh (+ jq, tmux, git y `claude` del perfil); se copian tal
;;; cual al store bajo share/claude-tmux-integrations, que es la raíz del
;;; marketplace (.claude-plugin/marketplace.json).  El registro en
;;; ~/.claude/settings.json lo hace el servicio de activación
;;; claude-tmux-plugin de home-config.scm.
;;;
;;; El repo no tiene tags: se fija un commit.  Para actualizar:
;;;   1. cambia `commit` al nuevo hash y sube `revision`.
;;;   2. obtén el nuevo hash del checkout:
;;;        guix hash -rx $(guix build -S claude-tmux-integrations -L ~/.config/guix-home/guix-packages)
;;;      o deja fallar el build y pega el hash que reporta en (base32 ...).

(define-module (my-packages claude-tmux-integrations)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public claude-tmux-integrations
  (let ((commit "846f6fdf2fb981cda46b7d8f8fc934cd7effe16e")
        (revision "0"))
    (package
      (name "claude-tmux-integrations")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/horellana/claude-tmux-integrations")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0s5j3w47jsdsq0pmkc8kpmlkrrjb99d50ys368wrapyy0zh020fp"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("." "share/claude-tmux-integrations"))))
      (home-page "https://github.com/horellana/claude-tmux-integrations")
      (synopsis "Integraciones entre Claude Code y tmux (marketplace de plugins)")
      (description
       "Marketplace de plugins de Claude Code con integraciones para tmux.
Incluye @code{tmux-window-title}, que nombra la ventana de tmux de cada sesión
según su tema.  Este paquete solo entrega los archivos en
share/claude-tmux-integrations; el registro en @file{~/.claude/settings.json} lo
realiza el servicio de activación de Guix Home.")
      (license license:expat))))
