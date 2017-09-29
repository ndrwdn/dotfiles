(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)

(defvar desired-packages
  '(;; general
    counsel
    counsel-projectile
    company
    projectile
    adoc-mode
    markdown-mode
    magit
    restclient
    cql-mode

    ;; for Haskell
    haskell-mode

    ;; for JSON
    json-mode

    ;; Golang
    go-mode
    go-guru
    go-autocomplete
    exec-path-from-shell

    ;; for Elixir
    elixir-mode

    ;; for Clojure
    paredit
    aggressive-indent
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    rainbow-delimiters))

(dolist (p desired-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))
