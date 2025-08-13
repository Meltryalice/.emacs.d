;; init-tools.el --- Initialize snippet configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :defer 2
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package magit
  :ensure t
  :defer t)

(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . 'ace-window)))

(use-package undo-tree
  :ensure t
  :defer t
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

(use-package good-scroll
  :ensure t
  :defer t
  :if window-system
  :hook (after-init . good-scroll-mode))

(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode))

(use-package highlight-symbol
  :ensure t
  :defer t
  :hook (after-init . highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol))

(use-package dirvish
  :ensure t
  :defer t
  :hook (after-init . dirvish-override-dired-mode))

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
          ("j" . scroll-up-line)
          ("k" . scroll-down-line))
  )

(use-package calibredb
  :ensure t
  :commands calibredb
  :bind ("\e\e b" . calibredb)
  :config
  (setq calibredb-root-dir "~/book")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/book")
                       ))
  )

(use-package olivetti
  :ensure t
  :defer t
  :hook
  (nov-mode . olivetti-mode))

(use-package emacs
  :ensure nil
  :config
  (when (olivetti-mode)
    (display-line-numbers-mode nil)))

(use-package rime
  :ensure t
  :defer t
  :init
  (setq rime-user-data-dir "~/.config/fcitx/rime/")
  (setq rime-posframe-properties
	(list :background-color "#333333"
              :foreground-color "#dcdccc"
              :font "LXGW Wenkai-20"
              :internal-border-width 15))

  (setq default-input-method "rime"
	rime-show-candidate 'posframe)
  :custom
  (default-input-method "rime"))

(use-package telega
  :ensure t
  :defer t
  :init
  (setq telega-proxies
	(list '(:server "127.0.0.1" :port 7897 :enable t
			:type (:@type "proxyTypeSocks5")))))
  
(provide 'init-tools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
