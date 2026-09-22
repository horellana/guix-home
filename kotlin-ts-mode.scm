;;; Paquete Guix para kotlin-ts-mode (major mode de Kotlin basado en treesit).
;;;
;;; DESTINO: copiar/mover este archivo a
;;;   /home/hector/guix-packages/my-packages/kotlin-ts-mode.scm
;;; (el directorio que `home-config.scm` añade con add-to-load-path).
;;;
;;; El upstream vive en GitLab, no en GitHub: el repo bricka/emacs-kotlin-ts-mode
;;; de GitHub da 404 y el único espejo (emacsmirror/kotlin-ts-mode) es de solo
;;; lectura.  La receta de MELPA también apunta a GitLab.
;;;
;;; No tiene tags, así que va como snapshot (git-version).  Para actualizar:
;;;   1. mira el commit nuevo:
;;;        git ls-remote https://gitlab.com/bricka/emacs-kotlin-ts-mode.git HEAD
;;;   2. cambia `commit', sube `revision' en 1.
;;;   3. recalcula el hash:
;;;        git clone https://gitlab.com/bricka/emacs-kotlin-ts-mode.git pkg
;;;        cd pkg && git checkout <commit> && guix hash -rx .
;;;
;;; La gramática (fwcd/tree-sitter-kotlin) también va aquí, y no es cosmético:
;;; el modo escribe sus queries contra el *master* de la gramática, no contra el
;;; último tag.  Con el `tree-sitter-kotlin' 0.3.8 de Guix, Emacs arranca pero
;;; avisa de "treesit-font-lock-rules-mismatch" y deja fuera las features
;;; `constant' y `string' (comprobado: las cadenas se quedan sin resaltar).  Por
;;; eso `tree-sitter-kotlin-next' fija un commit de master.
;;;
;;; Instala libtree-sitter-kotlin.so con el mismo nombre que el paquete de Guix,
;;; así que en home-config.scm va uno **o** el otro, nunca los dos (colisión en
;;; el profile).  A `treesit-extra-load-path' ya lo apunta init.el.

(define-module (my-packages kotlin-ts-mode)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages tree-sitter))

(define-public emacs-kotlin-ts-mode
  (let ((commit "292c9a05ff2243c39a48201b277840b1d25e1ea0")
        (revision "0"))
    (package
      (name "emacs-kotlin-ts-mode")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/bricka/emacs-kotlin-ts-mode.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0jypfbkc30xw5cij9mwzqmpb6r62kxlc8qchyyfc6kb1s7nih3hk"))))
      (build-system emacs-build-system)
      (home-page "https://gitlab.com/bricka/emacs-kotlin-ts-mode")
      (synopsis "Major mode de Emacs para Kotlin basado en tree-sitter")
      (description
       "@code{kotlin-ts-mode} edita ficheros Kotlin usando el soporte nativo de
tree-sitter de Emacs 29+ (requiere Emacs 30.1 o superior): resaltado, indentación,
imenu y navegación.  Necesita la gramática @code{tree-sitter-kotlin} de fwcd.")
      (license license:gpl3+))))

(define-public tree-sitter-kotlin-next
  ;; Sustituto del tree-sitter-kotlin de Guix (0.3.8, tag de 2024): master.
  ;; Para actualizar: nuevo commit de
  ;;   git ls-remote https://github.com/fwcd/tree-sitter-kotlin HEAD
  ;; y `guix hash -rx' sobre el checkout.
  (let ((commit "1852ea17b7f60fb3f9d84e0b1555d56b46b39fb1")
        (revision "0"))
    (package
      (inherit tree-sitter-kotlin)
      (version (git-version "0.3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fwcd/tree-sitter-kotlin")
               (commit commit)))
         (file-name (git-file-name "tree-sitter-kotlin" version))
         (sha256
          (base32 "088kh5cy57jnysscpfb9lg6h0vnmj0pcwdy85x9x5nkjwk5514gf")))))))
