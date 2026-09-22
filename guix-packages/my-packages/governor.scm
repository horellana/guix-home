;;; Paquete Guix para el plugin Governor de Claude Code.
;;;
;;; Vive en guix-packages/my-packages/ de este mismo repo, que es el directorio
;;; que `home-config.scm` añade con add-to-load-path.  No hay copia en $HOME.
;;;
;;; Governor son solo scripts (Python de stdlib + shell); se copian tal cual al
;;; store bajo share/governor.  El registro en ~/.claude/settings.json lo hace
;;; un servicio de activación en home-config.scm (governor-claude-plugin), que
;;; fusiona el JSON de forma idempotente en cada `guix home reconfigure`.
;;;
;;; Para actualizar de versión:
;;;   1. cambia (version ...) al nuevo tag (vX.Y.Z).
;;;   2. obtén el nuevo hash del checkout:
;;;        guix hash -rx $(guix build -S governor -L ~/.config/guix-home/guix-packages)
;;;      o deja fallar el build y pega el hash que reporta en (base32 ...).

(define-module (my-packages governor)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public governor
  (package
    (name "governor")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/0xhimanshu/governor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rw091ijxwa7s3hii1h9wrfp8r7h5pg2aqyydmaby0gdlavazcpm"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "share/governor"))))
    (home-page "https://github.com/0xhimanshu/governor")
    (synopsis "Optimizador de contexto/uso para Claude Code (plugin)")
    (description
     "Governor es un plugin de Claude Code que recorta el consumo de contexto en
sesiones largas: filtra la salida ruidosa de herramientas, lleva un ledger local
de tokens y puede comprimir archivos de memoria.  Este paquete solo entrega los
scripts del plugin en share/governor; el registro en @file{~/.claude/settings.json}
lo realiza el servicio de activación de Guix Home.")
    (license license:expat)))
