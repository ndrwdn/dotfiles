(require 'mmm-mode)
(require 'plsql)

(set-face-background 'mmm-default-submode-face nil)
(setq-default mmm-global-mode t)
(setq-default mmm-never-modes
              (append '(ediff-mode) '(text-mode) mmm-never-modes))

(mmm-add-classes
 '((embedded-sql
    :submode plsql-mode
    :front "EXEC SQL"
    :back ";")))

(mmm-add-mode-ext-class 'c-mode "\\.pc\\'" 'embedded-sql)
(mmm-add-mode-ext-class 'c-mode "\\.sqc\\'" 'embedded-sql)
(add-to-list 'auto-mode-alist '("\\.pc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.sqc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . plsql-mode))
