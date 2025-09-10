;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
	lsp-file-watch-threshold 500)
  :hook
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  (js-ts-mode . lsp-deferred)
  (typescript-ts-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  :commands (lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable t)
  :bind
  ("C-c l s" . lsp-ivy-workspace-symbol)) ;; 可快速搜索工作区内的符号（类名、函数名、变量名等）

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode)
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package consult-lsp
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-mode-map
              ("C-M-." . consult-lsp-symbols)))

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
