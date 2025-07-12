;; init-check.el --- Initialize check configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
 :ensure t
 :config
 (setq truncate-lines nil)
 :hook
 (prog-mode . flycheck-mode))

(provide 'init-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-check.el ends here
