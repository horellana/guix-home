;; Entorno de desarrollo Rust para wallpapers-downloader.
;; Uso:  guix shell -m manifest.scm
;;   (o) guix shell -m manifest.scm -- cargo build
(specifications->manifest
 (list "rust"            ; rustc + std
       "rust:cargo"      ; cargo (output separado)
       "rust:rust-src"   ; fuentes de std, para rust-analyzer
       "rust-analyzer"   ; LSP
       "gcc-toolchain"   ; linker (cc) que necesita rustc
       "pkg-config"      ; descubrir libs del sistema
       "openssl"))       ; TLS para descargas HTTP (reqwest/openssl)
