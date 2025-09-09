;;; early-init.el --- Early initialization. -*- lexical-binding: t; -*-

;;; Code:

(setq load-suffixes '(".elc" ".el"))
(setq load-file-rep-suffixes '(""))

(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil)

(setq native-comp-speed -1) ;; disable native comp

(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
