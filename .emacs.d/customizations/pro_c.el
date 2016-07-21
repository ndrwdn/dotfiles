(require 'mmm-mode)
(set-face-background 'mmm-default-submode-face nil)
(mmm-add-classes
 '((embedded-sql
    :submode sql-mode
    :front "EXEC SQL"
    :back ";")))
(setq-default mmm-global-mode t)
(mmm-add-mode-ext-class 'c-mode "\\.pc\\'" 'embedded-sql)
(mmm-add-mode-ext-class 'c-mode "\\.sqc\\'" 'embedded-sql)
(add-to-list 'auto-mode-alist '("\\.pc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.sqc\\'" . c-mode))
(setq-default mmm-never-modes
              (append '(ediff-mode) '(text-mode) mmm-never-modes))
