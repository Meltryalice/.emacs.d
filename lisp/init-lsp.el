;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :defer t
  :hook ((prog-mode . (lambda ()
                 (unless (derived-mode-p
                       'emacs-lisp-mode 'lisp-mode
                       'makefile-mode 'snippet-mode
                       'ron-mode)
                   (eglot-ensure))))
       ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
      eglot-events-buffer-size 0
      eglot-send-changes-idle-time 0.5))

(use-package consult-eglot
  :defer t
  :after consult eglot
  :bind (:map eglot-mode-map
          ("C-M-." . consult-eglot-symbols)))


(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
