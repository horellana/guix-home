;;; Paquete Guix para opencode (binario nativo Bun distribuido en npm).
;;;
;;; DESTINO: mover/copiar este archivo a
;;;   /home/hector/guix-packages/my-packages/opencode.scm
;;; (el directorio que `home-config.scm` añade con add-to-load-path).
;;;
;;; NOTA: igual que claude-code, el binario lo compila Bun y `patchelf` lo
;;; rompe, porque al reescribir el ELF se invalidan offsets que Bun lleva
;;; grabados.  Por eso NO se parchea: se deja intacto en libexec/ y
;;; bin/opencode es un wrapper que lo lanza con el ld.so de glibc del store
;;; (--library-path).  Solo necesita glibc (libc, libm, libdl, libpthread).
;;;
;;; Para actualizar de versión:
;;;   1. cambia (version ...).
;;;   2. obtén el nuevo hash:
;;;        guix download "https://registry.npmjs.org/opencode-linux-x64/-/opencode-linux-x64-<VERSION>.tgz"
;;;   3. pega el base32 que imprime en (sha256 (base32 ...)).

(define-module (my-packages opencode)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)     ;glibc
  #:use-module (gnu packages bash))    ;bash-minimal (wrapper)

(define-public opencode
  (package
    (name "opencode")
    (version "1.18.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/opencode-linux-x64/-/"
             "opencode-linux-x64-" version ".tgz"))
       (sha256
        (base32 "0q4ybz802b47w4sc4lkb9y9xxc6xk65wnag7cf40rfnibp1qzj24"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("bin/opencode" "libexec/opencode"))
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
                    (real    (string-append out "/libexec/opencode"))
                    (bin     (string-append out "/bin/opencode"))
                    (bash    (assoc-ref inputs "bash-minimal"))
                    (glibc   (assoc-ref inputs "glibc"))
                    (ld      (string-append glibc "/lib/ld-linux-x86-64.so.2"))
                    (libpath (string-append glibc "/lib")))
               (mkdir-p (string-append out "/bin"))
               (call-with-output-file bin
                 (lambda (port)
                   (format port "#!~a/bin/bash
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"
exec ~a --library-path \"~a\" ~a \"$@\"~%"
                           bash libpath ld libpath real)))
               (chmod bin #o555)
               #t))))))
    (inputs
     `(("bash-minimal" ,bash-minimal)
       ("glibc"        ,glibc)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://opencode.ai")
    (synopsis "Agente de programación para la terminal (binario nativo)")
    (description
     "opencode es un agente de programación para la terminal, agnóstico al
proveedor de modelos.  Este paquete instala el binario nativo precompilado
(Bun) para Linux x86_64 que se distribuye en npm.  El binario se deja sin
modificar y se ejecuta a través de un wrapper que invoca el enlazador dinámico
de glibc, ya que parchearlo lo corrompe.")
    (license license:expat)))
