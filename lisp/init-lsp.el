;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
 :ensure t
 :init
 ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
 (setq lsp-keymap-prefix "C-c l"
	lsp-file-watch-threshold 500)
 :hook ((js-ts-mode . lsp-deferred)
	(typescript-ts-mode . lsp-deferred)
        (lsp-mode . lsp-enable-which-key-integration)) ; which-key integration
 :commands (lsp-deferred)
 :config
 (setq lsp-completion-provider :none)
 (setq lsp-headerline-breadcrumb-enable t)
 :bind
 ("C-c l s" . lsp-ivy-workspace-symbol))

(use-package lsp-ui
 :ensure t
 :defer 2 
 :config
 (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
 (setq lsp-ui-doc-position 'top))

(use-package consult-lsp
  :ensure t
  :after (lsp-mode))

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
