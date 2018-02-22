(package-initialize)

(add-to-list 'load-path "~/.emacs.d/customizations")

(load "packages.el")
(load "general.el")
(load "local-org-mode-config.el")
(load "my-ivy-config.el")
(load "projectile-config.el")
(load "md-mode-config.el")
(load "json-config.el")
(load "company-config.el")
(load "clojure.el")
(load "magit-config.el")
(load "golang-config.el")
(load "rust-config.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xclip rainbow-delimiters cider clojure-mode-extra-font-locking clojure-mode paredit json-mode haskell-mode magit markdown-mode helm-projectile projectile helm-ag helm company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
