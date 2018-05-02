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
    perspective
    adoc-mode
    markdown-mode
    magit
    restclient
    cql-mode
    yaml-mode
    ob-http
    xclip
    flycheck
    neotree

    ;; Because vim
    evil
    undo-tree
    goto-chg

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
    rainbow-delimiters

    ;; for Rust
    company-racer
    racer
    flycheck-rust
    rust-mode
    ))

(dolist (p desired-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))
