;;; Paquete Guix para el Kotlin LSP oficial de JetBrains (distribución binaria).
;;;
;;; DESTINO: copiar/mover este archivo a
;;;   /home/hector/guix-packages/my-packages/kotlin-lsp.scm
;;; (el directorio que `home-config.scm` añade con add-to-load-path).
;;;
;;; NOTAS DE EMPAQUETADO (verificadas ejecutando el servidor, no supuestas):
;;;
;;;   * El archivo "Standalone Kotlin LSP Archive" trae un JetBrains Runtime
;;;     completo en jbr/ (~250 MB) enlazado contra el FHS.  Se borra y se deja
;;;     jbr como symlink al openjdk de Guix: el lanzador nativo busca
;;;     `jbr/bin/java' relativo a su propio directorio (javaExecutablePath en
;;;     product-info.json), así que el symlink basta.  product-info.json pide
;;;     minRequiredJavaVersion 25 y openjdk de Guix es 25.0.2.
;;;
;;;   * bin/intellij-server es un ELF nativo que SÍ se puede parchear con
;;;     patchelf (a diferencia del binario Bun de claude-code).  Hay que
;;;     parchearlo y no lanzarlo vía `ld.so --library-path': el lanzador localiza
;;;     product-info.json a partir de su propia ruta ejecutable, y con el truco
;;;     del loader esa ruta pasa a ser la de ld.so y aborta con
;;;     "Cannot find a directory with a product descriptor".
;;;
;;;   * El wrapper exporta LD_LIBRARY_PATH con gcc:lib porque las libs JNI que
;;;     carga la JVM (rocksdbjni, pty4j, filewatcher, jna) necesitan libstdc++.
;;;     Sin eso el servidor arranca, responde a `initialize' con result null y
;;;     sólo el log delata el UnsatisfiedLinkError de librocksdbjni.
;;;
;;;   * kotlin-lsp.sh está deprecado upstream y sólo hace exec de
;;;     bin/intellij-server; no se instala.
;;;
;;; Para actualizar de versión:
;;;   1. mira la release nueva:
;;;        curl -sS https://api.github.com/repos/Kotlin/kotlin-lsp/releases?per_page=1
;;;   2. cambia (version ...) al buildNumber (p. ej. 263.4702.0).
;;;   3. obtén el hash:
;;;        guix download "https://download.jetbrains.com/language-server/kotlin-server/<VER>/kotlin-server-<VER>.tar.gz"
;;;      (JetBrains publica el sha256 al lado, con .sha256 al final de la URL;
;;;       compáralo antes de pegar el base32).
;;;   4. revisa minRequiredJavaVersion en product-info.json: si sube por encima
;;;      del openjdk de Guix, este paquete deja de arrancar.

(define-module (my-packages kotlin-lsp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)    ;glibc
  #:use-module (gnu packages bash)    ;bash-minimal (wrapper)
  #:use-module (gnu packages elf)     ;patchelf
  #:use-module (gnu packages gcc)     ;gcc:lib -> libstdc++ para las libs JNI
  #:use-module (gnu packages java))   ;openjdk:jdk

(define-public kotlin-lsp
  (package
    (name "kotlin-lsp")
    (version "263.4702.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.jetbrains.com/language-server/kotlin-server/"
             version "/kotlin-server-" version ".tar.gz"))
       (sha256
        (base32 "0184b550hlhscrxcps46s1y8j8i2bzlnppfqb8hymygvzvjx448y"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "share/kotlin-lsp"))
       #:phases
       (modify-phases %standard-phases
         ;; Son jars y binarios de terceros: ni strip, ni validar RUNPATH (las
         ;; libs JNI no tienen ninguno: las resuelve LD_LIBRARY_PATH del
         ;; wrapper), ni construir la caché del enlazador sobre ~900 MB.
         (delete 'strip)
         (delete 'validate-runpath)
         (delete 'make-dynamic-linker-cache)
         (add-before 'install 'drop-bundled-jbr
           (lambda _
             (delete-file-recursively "jbr")
             (delete-file "kotlin-lsp.sh")))
         (add-after 'install 'link-jdk
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (symlink (assoc-ref inputs "openjdk")
                      (string-append (assoc-ref outputs "out")
                                     "/share/kotlin-lsp/jbr"))))
         (add-after 'install 'patch-launcher
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((glibc  (assoc-ref inputs "glibc"))
                   (gcclib (assoc-ref inputs "gcc:lib")))
               (invoke "patchelf"
                       "--set-interpreter"
                       (string-append glibc "/lib/ld-linux-x86-64.so.2")
                       "--set-rpath"
                       (string-append glibc "/lib:" gcclib "/lib")
                       (string-append (assoc-ref outputs "out")
                                      "/share/kotlin-lsp/bin/intellij-server")))))
         (add-after 'patch-launcher 'make-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (bash   (assoc-ref inputs "bash-minimal"))
                    (gcclib (assoc-ref inputs "gcc:lib"))
                    (bin    (string-append out "/bin/kotlin-lsp")))
               (mkdir-p (string-append out "/bin"))
               (call-with-output-file bin
                 (lambda (port)
                   (format port "#!~a/bin/bash
export LD_LIBRARY_PATH=\"~a/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"
exec ~a/share/kotlin-lsp/bin/intellij-server \"$@\"~%"
                           bash gcclib out)))
               (chmod bin #o555)))))))
    (inputs
     `(("bash-minimal" ,bash-minimal)
       ("glibc"        ,glibc)
       ("gcc:lib"      ,gcc "lib")
       ("openjdk"      ,openjdk "jdk")))
    (native-inputs
     `(("patchelf" ,patchelf)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/Kotlin/kotlin-lsp")
    (synopsis "Servidor LSP oficial de JetBrains para Kotlin")
    (description
     "Servidor de lenguaje para Kotlin construido por JetBrains sobre la
plataforma IntelliJ: completado, hover, ir a definición/implementación,
referencias, jerarquías de llamadas y tipos, símbolos, refactor, rename,
formateo, inlay hints, semantic tokens y diagnósticos (pull).  Resuelve el
classpath importando el proyecto de Gradle o Maven, así que necesita un proyecto
con build y acceso a la red la primera vez.

Este paquete instala la distribución binaria @emph{standalone} para Linux
x86_64; se ejecuta con el @code{openjdk} de Guix en lugar del JetBrains Runtime
que trae el archivo.  Invócalo como @code{kotlin-lsp --stdio}.")
    (license license:asl2.0)))
