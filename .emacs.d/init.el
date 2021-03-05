;; Disable the menubar and toolbar
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

;; Turn off tabs, make tab-stop's be in increments of 2
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 120 2))
(setq-default tab-width 2)

;; Show trailing whitespace and tabs
(require 'whitespace)
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode 1)

;; Change where Custom puts its stuff
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Enable mouse support
(xterm-mouse-mode 1)
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)
(electric-pair-mode 1)

;; Set theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "emacs-color-theme-solarized"))
(add-to-list 'load-path (concat user-emacs-directory "emacs-color-theme-solarized"))
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

;; Setup/install for use-package
(eval-and-compile
  (require 'package)
  (setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setf use-package-always-ensure t))

;; Packages
(use-package xclip
  :config
  (xclip-mode 1))

(use-package evil
  :config
  (evil-mode 1))

(use-package paredit
  :hook (emacs-lisp-mode . enable-paredit-mode))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :hook (json-mode .
                   (lambda ()
                     (make-local-variable 'js-indent-level)
                     (setq js-indent-level 2))))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook (yaml-mode .
                   (lambda ()
                     (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-height 10)
  (setq enable-recursive-minibuffers t)
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1))

(use-package counsel-projectile
  :defer t)

(use-package projectile
  :defer t
  :config
  (projectile-mode 1)
  (counsel-projectile-mode)
  :bind-keymap (("C-c p" . projectile-command-map)))

(use-package neotree
  :bind (("C-c t" . neotree-toggle)))
;; (use-package perspective
;;   :disabled
;;   :config
;;   (persp-mode))

(use-package auto-package-update)

;; Maybe enable this?
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Helpful while working on Emacs config
(defun display-startup-echo-area-message ()
  (message "Started in: %s" (emacs-init-time)))
