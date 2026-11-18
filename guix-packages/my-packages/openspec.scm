;;; Paquete Guix para OpenSpec (CLI de spec-driven development, npm).
;;;
;;; Vive en guix-packages/my-packages/ de este mismo repo, que es el directorio
;;; que `home-config.scm` añade con add-to-load-path.
;;;
;;; OpenSpec es JS puro con ~12 dependencias npm (y sus transitivas), así que no
;;; basta copiar un binario.  En vez de empaquetar cada dependencia en Guix, el
;;; `source` es una derivación de salida fija (`npm-install-fetch`) que ejecuta
;;; `npm install` con red y cuyo resultado queda fijado por el hash de abajo.
;;; El paquete solo instala ese árbol y un wrapper `bin/openspec` que lo lanza
;;; con el node del store.
;;;
;;; Para actualizar de versión:
;;;   1. cambia (version ...).
;;;   2. pon un hash falso (p.ej. (base32 "1p5f64igbd5qg0j1jjbkmp6j4a6452pclb7gpb3jvdg3cyhcgcxp"))
;;;      y ejecuta `guix build -f guix-packages/my-packages/openspec.scm`; el
;;;      error de "hash mismatch" imprime el base32 real.
;;;   3. pega ese base32 en (sha256 (base32 ...)).
;;; El hash cubre TODO node_modules: cambia también si lo hace alguna
;;; dependencia transitiva dentro del rango semver del paquete.

(define-module (my-packages openspec)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)     ;bash-minimal (wrapper)
  #:use-module (gnu packages nss)      ;nss-certs (TLS de npm)
  #:use-module (gnu packages node))    ;node

(define* (npm-install-fetch spec hash-algo hash name
                            #:key system #:allow-other-keys)
  "Derivación de salida fija: instala el paquete npm SPEC (\"nombre@versión\")
con sus dependencias de producción y deja el árbol node_modules como salida."
  (gexp->derivation
   name
   (with-imported-modules '((guix build utils))
     #~(begin
       (use-modules (guix build utils))
       (let ((npm (string-append #$node "/bin/npm")))
       (setenv "PATH" (string-append #$node "/bin"))
       (setenv "HOME" (getcwd))
       (setenv "SSL_CERT_FILE" #$(file-append nss-certs "/etc/ssl/certs/ca-certificates.crt"))
       (mkdir #$output)
       ;; --ignore-scripts: ningún script de instalación de terceros.
       ;; Cache dentro del directorio de build; nada se escribe fuera.
       (invoke npm "install" "--prefix" #$output
               "--omit=dev" "--ignore-scripts" "--no-audit" "--no-fund"
               "--no-update-notifier" "--cache" (string-append (getcwd) "/npm-cache")
               #$spec)
       ;; Los lockfiles de npm graban la ruta de salida (que depende del
       ;; propio hash fijo), así que harían imposible converger el hash.
       (for-each (lambda (f)
                   (when (file-exists? f) (delete-file f)))
                 (list (string-append #$output "/package-lock.json")
                       (string-append #$output "/node_modules/.package-lock.json"))))))
   #:system system
   #:hash-algo hash-algo
   #:hash hash
   #:recursive? #t
   #:local-build? #t))

(define-public openspec
  (package
    (name "openspec")
    (version "2.1.0")
    (source
     (origin
       (method npm-install-fetch)
       (uri (string-append "@fission-ai/openspec@" version))
       (file-name (string-append name "-" version "-node-modules"))
       (sha256
        (base32 "1p5f64igbd5qg0j1jjbkmp6j4a6452pclb7gpb3jvdg3cyhcgcxp"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("node_modules" "lib/openspec/node_modules"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (bin  (string-append out "/bin/openspec"))
                     (js   (string-append out "/lib/openspec/node_modules"
                                          "/@fission-ai/openspec/bin/openspec.js")))
                (mkdir-p (dirname bin))
                (call-with-output-file bin
                  (lambda (port)
                    (format port "#!~a/bin/bash\nexec ~a/bin/node ~a \"$@\"~%"
                            (assoc-ref inputs "bash-minimal")
                            (assoc-ref inputs "node") js)))
                (chmod bin #o555)))))))
    (inputs (list bash-minimal node))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/Fission-AI/OpenSpec")
    (synopsis "Spec-driven development para asistentes de código con IA")
    (description
     "OpenSpec añade una capa ligera de especificaciones a los proyectos para
que el humano y el asistente de IA acuerden qué construir antes de escribir
código.  Este paquete instala el CLI @command{openspec} desde npm.")
    (license license:expat)))
