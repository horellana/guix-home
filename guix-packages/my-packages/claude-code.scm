;;; Paquete Guix para Claude Code (binario nativo Bun distribuido en npm).
;;;
;;; Vive en guix-packages/my-packages/ de este mismo repo, que es el directorio
;;; que `home-config.scm` añade con add-to-load-path.  No hay copia en $HOME.
;;;
;;; NOTA: el binario lo compila Bun y `patchelf` lo rompe (segfault), porque al
;;; reescribir el ELF se invalidan offsets que Bun lleva grabados.  Por eso NO
;;; se parchea: se deja intacto en libexec/ y bin/claude es un wrapper que lo
;;; lanza con el ld.so de glibc del store (--library-path).
;;;
;;; Para actualizar de versión:
;;;   1. cambia (version ...).
;;;   2. obtén el nuevo hash:
;;;        guix download "https://registry.npmjs.org/@anthropic-ai/claude-code-linux-x64/-/claude-code-linux-x64-<VERSION>.tgz"
;;;   3. pega el base32 que imprime en (sha256 (base32 ...)).

(define-module (my-packages claude-code)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (gnu packages base)     ;glibc
  #:use-module (gnu packages bash)     ;bash-minimal (wrapper)
  #:use-module (gnu packages gnome)    ;libsecret (dlopen: keyring)
  #:use-module (gnu packages linux)    ;alsa-lib (dlopen: libasound)
  #:use-module (gnu packages python))  ;python3 (hooks de plugins, p.ej. governor)

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.278")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code-linux-x64/-/"
             "claude-code-linux-x64-" version ".tgz"))
       (sha256
        (base32 "1izicf0assr5qggz7nid5d0w495invbfk3j1gyyx2d021amm3yyi"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("claude" "libexec/claude"))
       ;; El binario se deja intacto (ver nota arriba): nada de strip ni de
       ;; validar/parchear RUNPATH; lo resuelve el wrapper vía el loader.
       #:phases
       (modify-phases %standard-phases
         (delete 'strip)
         (delete 'validate-runpath)
         (delete 'make-dynamic-linker-cache)
         (add-after 'install 'wrap-loader
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (real    (string-append out "/libexec/claude"))
                    (bin     (string-append out "/bin/claude"))
                    (bash    (assoc-ref inputs "bash-minimal"))
                    (python  (assoc-ref inputs "python"))
                    (glibc   (assoc-ref inputs "glibc"))
                    (ld      (string-append glibc "/lib/ld-linux-x86-64.so.2"))
                    (libpath (string-append
                              glibc                            "/lib:"
                              (assoc-ref inputs "libsecret")   "/lib:"
                              (assoc-ref inputs "alsa-lib")    "/lib")))
               (mkdir-p (string-append out "/bin"))
               (call-with-output-file bin
                 (lambda (port)
                   (format port "#!~a/bin/bash
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"
export PATH=\"~a/bin${PATH:+:$PATH}\"
exec ~a --library-path \"~a\" ~a \"$@\"~%"
                           bash libpath python ld libpath real)))
               (chmod bin #o555)
               #t))))))
    (inputs
     `(("bash-minimal" ,bash-minimal)
       ("glibc"        ,glibc)
       ("libsecret"    ,libsecret)
       ("alsa-lib"     ,alsa-lib)
       ("python"       ,python)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis "Anthropic's agentic coding tool (binario nativo)")
    (description
     "Claude Code es la CLI oficial de Anthropic para programación agéntica.
Este paquete instala el binario nativo precompilado (Bun) para Linux x86_64 que
se distribuye en npm.  El binario se deja sin modificar y se ejecuta a través de
un wrapper que invoca el enlazador dinámico de glibc, ya que parchearlo lo
corrompe.")
    (license (license:nonfree
              "https://www.anthropic.com/legal/commercial-terms"))))
