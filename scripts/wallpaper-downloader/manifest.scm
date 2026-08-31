;; Entorno de desarrollo Guix para este proyecto Python.
;; Uso: guix shell -m manifest.scm   (o) guix shell -m manifest.scm -- <comando>
(specifications->manifest
 (list "python"              ; 3.12.12
       "python-lsp-server")) ; pylsp, el LSP que arranca eglot en python-mode
