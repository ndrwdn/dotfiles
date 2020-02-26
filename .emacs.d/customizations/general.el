;; Disable the menubar or toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Don't autosave or backup
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq create-lockfiles nil)

;; When autosaving is manually enabled, do it in the same file, every 5 seconds
(setq auto-save-visited-file-name t)
(setq auto-save-timeout 5)

;; Show line numbers
(global-linum-mode 1)
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)

;; Go straight to scratch buffer on startup, without a message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Set theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(add-to-list 'load-path (concat user-emacs-directory "themes"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "emacs-color-theme-solarized"))
(add-to-list 'load-path (concat user-emacs-directory "emacs-color-theme-solarized"))
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

;; Turn off tabs, make tab-stop's be in increments of 2
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))
(setq-default tab-width 2)

;; Show trailing whitespace and tabs
(require 'whitespace)
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode 1)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Handle tmux's xterm-keys
(if (getenv "TMUX")
    (progn
      (define-key key-translation-map (kbd "M-[ A") (kbd "C-<up>"))
      (define-key key-translation-map (kbd "M-[ B") (kbd "C-<down>"))
      (define-key key-translation-map (kbd "M-[ C") (kbd "C-<right>"))
      (define-key key-translation-map (kbd "M-[ D") (kbd "C-<left>"))))

;; Change yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Load adoc-mode
(autoload 'adoc-mode "adoc-mode" nil t)

;; Paredit for elisp
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;(persp-mode)

(xclip-mode 1)

;; timestamp for, i.e., filenames
(defun insert-file-time-stamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S" (current-time))))


;; key-shortcut for neotree
(global-set-key (kbd "C-c M-t") 'neotree)

;; better parn stuff
(electric-pair-mode 1)
(show-paren-mode 1)
