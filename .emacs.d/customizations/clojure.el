(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(require 'clojure-mode-extra-font-locking)

(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer nil)

(setq cider-repl-history-file (concat user-emacs-directory
                                      "cider-history"))

(setq cider-repl-wrap-history t)
(setq cider-repl-prompt-function 'cider-repl-prompt-abbreviated)
(setq cider-repl-use-pretty-printing t)
(setq cider-repl-history-display-duplicates t)
(setq cider-repl-history-display-duplicate-highest t)

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljc.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(setq cider-eldoc-display-context-dependent-info t)
(setq cider-overlays-use-font-lock t)
(setq cider-pprint-fn 'puget)


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)))
