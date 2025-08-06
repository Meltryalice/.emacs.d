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

(electric-pair-mode t)
(column-number-mode t)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
