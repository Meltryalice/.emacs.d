;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil)

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
