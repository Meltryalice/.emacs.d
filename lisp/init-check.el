;; init-check.el --- Initialize check configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :defer t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
          ("C-c ! n" . flymake-goto-next-error)
          ("C-c ! p" . flymake-goto-prev-error)
          ("C-c ! l" . flymake-show-buffer-diagnostics)))

(use-package flymake-popon
  :ensure t
  :defer t
  :diminish
  :hook (flymake-mode . flymake-popon-mode)
  :init (setq flymake-popon-width 80))

(provide 'init-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-check.el ends here
