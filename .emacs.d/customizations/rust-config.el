;; Set path to rust src directory
(setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
(setq rust-rustfmt-bin (concat (getenv "HOME") "/.rustup/toolchains/nightly-x86_64-apple-darwin/bin/rustfmt"))

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Setting up configurations when you load rust-mode
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
