;;; early-init.el --- Early initialization. -*- lexical-binding: t; -*-

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

(let ((normal-gc-cons-threshold (* 20 1024 1024))
   (init-gc-cons-threshold (* 128 1024 1024)))
 (setq gc-cons-threshold init-gc-cons-threshold)
 (add-hook 'emacs-startup-hook
      (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq native-comp-deferred-compilation nil
   native-comp-jit-compilation nil)

(setq native-comp-speed -1) ;; disable native comp

(setq package-enable-at-startup nil)

(setq use-package-enable-imenu-support t)

(setq load-prefer-newer noninteractive)

(prefer-coding-system 'utf-8)

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
