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
  :config
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Maple Mono NF CN"
                             :height 5.0
			     :size 40))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  :init
  (setq nov-text-width 1000)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode))

(use-package calibredb
  :ensure t
  :commands calibredb
  :bind ("\e\e b" . calibredb)
  :config
  (setq calibredb-root-dir "~/book")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/book")))
  )

(use-package olivetti
  :ensure t
  :defer t
  :hook
  (nov-mode . olivetti-mode))

(use-package rime
  :ensure t
  :defer t
  :bind ("C-z" . rime-inline-ascii)
  :init
  (setq rime-user-data-dir "~/.config/fcitx/rime/")
  (setq rime-posframe-properties
	(list :background-color "#333333"
              :foreground-color "#dcdccc"
              :font "LXGW Wenkai-20"
              :internal-border-width 15))
  (setq default-input-method "rime"
	rime-show-candidate 'posframe)
  (setq rime-disable-predicates
	'(rime-predicate-prog-in-code-p
	  rime-predicate-space-after-cc-p
	  rime-predicate-punctuation-after-space-cc-p))
  :custom
  (default-input-method "rime"))

(use-package telega
  :ensure t
  :defer t
  :custom-face
  (telega-entity-type-code ((t (:family "Maple Mono"))))
  (telega-msg-heading ((t (:inherit hl-line :background unspecified))))
  (telega-msg-inline-reply ((t (:inherit (hl-line font-lock-function-name-face)))))
  (telega-msg-inline-forward ((t (:inherit (hl-line font-lock-type-face)))))
  (telega-msg-user-title ((t (:bold t))))
  :config
  (setq telega-symbol-folder ""
        telega-root-fill-column 70
        telega-root-auto-fill-mode nil)
  (setq telega-symbols-emojify nil)
  (setq telega-emoji-use-images nil)
  (setq telega-sticker--use-thumbnail t)
  (setq telega-symbol-reply (nerd-icons-faicon "nf-fa-reply"))
  (setq telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all"))
  (setq telega-symbol-forward (nerd-icons-mdicon "nf-md-comment_arrow_right_outline"))
  (setq telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all"))
  (setq telega-symbol-right-arrow (nerd-icons-codicon "nf-cod-arrow_right"))
  (setq telega-symbol-reaction (nerd-icons-mdicon "nf-md-heart_circle"))
  (setq telega-avatar-factors-alist
	'((1 . (1.5 . 0.1))
          (2 . (0.9 . 0.1))) )
  (setq telega-avatar-workaround-gaps-for '(return t))
  (setq telega-proxies
	(list '(:server "127.0.0.1" :port 7897 :enable t
			:type (:@type "proxyTypeSocks5")))))

(provide 'init-tools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
