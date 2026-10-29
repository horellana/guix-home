;;; Paquete Guix para Docker Compose v2 (binario estático oficial de GitHub).
;;;
;;; Guix solo trae docker-compose 1.29 (v1, Python), que no entiende
;;; `docker compose ... up --wait`.  Se fija la rama v2 a propósito: desde v5
;;; Compose quitó su builder interno y delega en buildx bake, que no funciona
;;; contra el socket de Podman rootless.  Con v2 y DOCKER_BUILDKIT=0 el build
;;; va por la API clásica /build, que Podman (buildah) atiende incluyendo
;;; `RUN --mount=type=cache` y `COPY --chmod`.
;;;
;;; El binario es Go estático: no necesita parchear el loader.  Se instala como
;;; bin/docker-compose y como plugin del CLI (libexec/docker/cli-plugins/);
;;; home-config.scm enlaza este último en ~/.docker/cli-plugins/.
;;;
;;; Para actualizar de versión (dentro de v2.x):
;;;   1. cambia (version ...).
;;;   2. guix download "https://github.com/docker/compose/releases/download/v<VERSION>/docker-compose-linux-x86_64"
;;;   3. pega el base32 en (sha256 (base32 ...)); el sha256 hex debe coincidir
;;;      con el digest del asset en la release de GitHub.

(define-module (my-packages docker-compose)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public docker-compose-v2
  (package
    (name "docker-compose-v2")
    (version "2.40.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/docker/compose/releases/download/v"
             version "/docker-compose-linux-x86_64"))
       (file-name (string-append "docker-compose-" version))
       (sha256
        (base32 "0wnk5zja6qadlgp6izps615658645z9rp6y9i0fy3gx53f7dkafv"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("docker-compose" "libexec/docker/cli-plugins/docker-compose"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (copy-file source "docker-compose")
              (chmod "docker-compose" #o755)))
          (delete 'strip)
          (delete 'validate-runpath)
          (add-after 'install 'link-bin
            (lambda _
              (mkdir-p (string-append #$output "/bin"))
              (symlink (string-append
                        #$output "/libexec/docker/cli-plugins/docker-compose")
                       (string-append #$output "/bin/docker-compose")))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/docker/compose")
    (synopsis "Define and run multi-container applications (Compose v2)")
    (description
     "Docker Compose v2, the @code{docker compose} CLI plugin, as the static
binary published upstream.")
    (license license:asl2.0)))
