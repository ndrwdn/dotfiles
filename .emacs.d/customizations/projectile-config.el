(require 'helm-projectile)

(projectile-global-mode)

;; Use helm-projectile
(setq projectile-completion-system 'helm)
(helm-projectile-on)
