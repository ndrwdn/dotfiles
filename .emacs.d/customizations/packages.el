(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)

(defvar desired-packages
  '(;; general
    company
    helm
    helm-ag
    projectile
    helm-projectile
    adoc-mode
    markdown-mode
    magit

    ;; for Haskell
    haskell-mode

    ;; for JSON
    json-mode

    ;; Golang
    go-mode
    go-guru
    go-autocomplete
    exec-path-from-shell

    ;; for Clojure
    paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    rainbow-delimiters))

(dolist (p desired-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))
