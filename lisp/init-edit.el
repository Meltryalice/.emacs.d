;;; init-edit.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

;; 不自动备份 自动保存
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 删除不常用的快捷键
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "s-q") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key [mouse-2] nil)

;; Directly modify when selecting text
(use-package delsel
  :ensure nil
  :defer t
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :defer t
  :hook (after-init . global-auto-revert-mode)
  :bind ("s-u" . revert-buffer)
  :custom
  (auto-revert-interval 10)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

(use-package indent-bars
  :defer t
  :ensure t
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.225))
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
				                       if_statement with_statement while_statement)))
  :hook ((prog-mode yaml-mode) . indent-bars-mode)
  :config (require 'indent-bars-ts))

(use-package aggressive-indent
  :ensure t
  :diminish
  :autoload aggressive-indent-mode
  :functions too-long-file-p
  :defer t
  :hook ((after-init . global-aggressive-indent-mode)
       ;; NOTE: Disable in large files due to the performance issues
       ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
       (find-file . (lambda ()
		  (when (too-long-file-p)
		  (aggressive-indent-mode -1)))))
  :config
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)))

(electric-pair-mode t)
(column-number-mode t)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
