;; Set path to rust src directory
(setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
(setq rust-rustfmt-bin (concat (getenv "HOME") "/.cargo/bin/rustfmt"))

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Setting up configurations when you load rust-mode
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(add-hook 'evil-mode-hook
          (lambda ()
            (add-hook 'racer-mode-hook
                      (lambda ()
                        (define-key evil-normal-state-local-map (kbd "M-.") #'racer-find-definition)
                        (define-key evil-normal-state-local-map (kbd "C-x 4 .") #'racer-find-definition-other-window)
                        (define-key evil-normal-state-local-map (kbd "C-x 5 .") #'racer-find-definition-other-frame)
                        (define-key evil-normal-state-local-map (kbd "M-,") #'pop-tag-mark)

                        (define-key evil-insert-state-local-map (kbd "M-.") #'racer-find-definition)
                        (define-key evil-insert-state-local-map (kbd "C-x 4 .") #'racer-find-definition-other-window)
                        (define-key evil-insert-state-local-map (kbd "C-x 5 .") #'racer-find-definition-other-frame)
                        (define-key evil-insert-state-local-map (kbd "M-,") #'pop-tag-mark)))))
