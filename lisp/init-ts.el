;; init-ts.el --- Initialize tree-sitter configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (setq treesit-font-lock-level 4)
  :defer t
  :hook (after-init . global-treesit-auto-mode))

(provide 'init-ts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ts.el ends here
