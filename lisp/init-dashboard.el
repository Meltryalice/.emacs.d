;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; use-package with package.el:
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "To Alice")
  (setq dashboard-projects-backend 'projectile) 
  (setq dashboard-startup-banner "~/.emacs.d/lisp/pic/logo.png")
  (setq dashboard-icon-type 'all-the-icons)  ; use `all-the-icons' package
  (setq dashboard-items '((recents . 5)  
    (bookmarks . 5) 
    (projects . 10)))
  (dashboard-modify-heading-icons '((recents   . "file-text")
                                    (bookmarks . "book")
				    (projects  . "briefcase")))
  :hook (after-init . dashboard-open)
  )

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
