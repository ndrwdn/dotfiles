(require 'helm-config)

;; Use helm for buffer list
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; Use helm for executing commands
(global-set-key (kbd "M-x") 'helm-M-x)

(helm-mode 1)
