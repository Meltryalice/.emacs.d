;; init-check.el --- Initialize web-coding configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package js2-mode 
  :ensure t 
  :mode "\.js\'" 
  :interpreter "node" 
  :config 
  (setq js2-basic-offset 2) 
  (setq js2-bounce-indent-p t))

(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
