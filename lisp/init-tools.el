;; init-tools.el --- Initialize snippet configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rainbow-delimiters
 :ensure t
 :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
 :ensure t
 :defer 2 
 :bind (("C-c p" . projectile-command-map))
 :config
 (setq projectile-mode-line "Projectile")
 (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
 :ensure t
 :after (projectile)
 :init (counsel-projectile-mode))

(use-package magit 
 :ensure t
 :defer t)

(use-package ace-window
  :ensure t
  :defer 2
  :bind (("C-x o" . 'ace-window)))

(use-package undo-tree
 :ensure t
 :hook (after-init . global-undo-tree-mode)
 :custom
 (undo-tree-auto-save-history nil))

(use-package good-scroll
 :ensure t
 :if window-system
 :hook (after-init . good-scroll-mode))

(use-package which-key
 :ensure t
 :hook (after-init . which-key-mode))

(use-package highlight-symbol
 :ensure t
 :hook (after-init . highlight-symbol-mode)
 :bind ("<f3>" . highlight-symbol))

(use-package dirvish
 :ensure t
 :hook (after-init . dirvish-override-dired-mode))

(provide 'init-tools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
