(defun custom-go-mode-hook ()
  ;; Use goimports
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(defun auto-complete-for-go ()
  (auto-complete-mode 1))

(add-hook 'go-mode-hook 'custom-go-mode-hook)
(add-hook 'go-mode-hook 'auto-complete-for-go)
(add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))
