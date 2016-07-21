(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(defvar desired-packages
  '(;; general
    company
    markdown-mode

    ;; for Haskell
    haskell-mode

    ;; for JSON
    json-mode

    ;; for Clojure
    paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    rainbow-delimiters

    ;; for Pro*C
    mmm-mode))

(dolist (p desired-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))
