;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; use-package with package.el:
(use-package dashboard
  :ensure t
  :defer t
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "To Alice")
  (setq dashboard-projects-backend 'projectile) 
  (setq dashboard-startup-banner "~/.emacs.d/lisp/pic/logo.png")
  (setq dashboard-display-icons-p t) 
  (setq dashboard-icon-type 'nerd-icons) 
  (setq dashboard-items '((recents . 5)  
		  (bookmarks . 5) 
		  (projects . 10)))
  (dashboard-modify-heading-icons '((recents   . "nf-oct-history")
                         (bookmarks . "nf-oct-bookmark")
			 (projects  . "nf-oct-briefcase")))
  :hook (after-init . dashboard-open)
  )

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
