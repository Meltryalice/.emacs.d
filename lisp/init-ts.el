;; init-ts.el --- Initialize tree-sitter configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (delete 'yaml treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (setq treesit-font-lock-level 4)
  :defer t
  :hook (after-init . global-treesit-auto-mode))

(use-package treesit-fold-indicators
  :ensure treesit-fold
  :hook (after-init . global-treesit-fold-indicators-mode)
  :init (setq treesit-fold-indicators-priority -1))

(provide 'init-ts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ts.el ends here
