;;; init.el --- A Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;;一些有关启动加速的设置
(setq-default mode-line-format nil)
(setq auto-mode-case-fold nil)
(unless (or (daemonp) noninteractive init-file-debug)
  ;; Temporarily suppress file-handler processing to speed up startup
  (let ((default-handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    ;; Recover handlers after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist default-handlers))))
              101)))

;;指定源码加载路径
(defun update-load-path (&rest _)
  "Update the `load-path` to prioritize personal configurations."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirectories in `site-lisp` to `load-path`.

Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(update-load-path)

;; custom-file
(setq custom-file "~/.emacs.d/custom.el")

;;模块化配置
(require 'init-package)
(require 'init-ui)
(require 'init-edit)
(require 'init-hydra)
(require 'init-completion)
(require 'init-tools)
(require 'init-treemacs)
(require 'init-dashboard)
(require 'init-lsp)
(require 'init-dap)
(require 'init-ts)
(require 'init-check)
(require 'init-eaf)
(require 'init-web)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

