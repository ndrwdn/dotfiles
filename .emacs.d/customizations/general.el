;; Disable the menubar
(menu-bar-mode -1)

;; Don't autosave or backup
(setq auto-save-default nil)
(setq backup-inhibited t)

;; Show line numbers
(global-linum-mode)
(setq linum-format "%4d ")

;; Go straight to scratch buffer on startup, without a message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Set theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(add-to-list 'load-path (concat user-emacs-directory "themes"))
(load-theme 'ujelly t)

;; Turn off tabs, make tab-stop's be in increments of 2
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))

;; Show trailing whitespace and tabs
(require 'whitespace)
(setq whitespace-style '(trailing tabs tab-mark))
(global-whitespace-mode 1)

;; Handle tmux's xterm-keys
(if (getenv "TMUX")
    (progn
      (define-key key-translation-map (kbd "M-[ A") (kbd "C-<up>"))
      (define-key key-translation-map (kbd "M-[ B") (kbd "C-<down>"))
      (define-key key-translation-map (kbd "M-[ C") (kbd "C-<right>"))
      (define-key key-translation-map (kbd "M-[ D") (kbd "C-<left>"))))

;; Change yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)
