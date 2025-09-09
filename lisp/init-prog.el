;;; init-prog.el --- prog settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eldoc
  :ensure nil
  :diminish
  :config
  (use-package eldoc-box
     :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
     :custom
     (eldoc-box-lighter nil)
     (eldoc-box-only-multi-line t)
     (eldoc-box-clear-with-C-g t)
     :custom-face
     (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
     (eldoc-box-body ((t (:inherit tooltip))))
     :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
     :config
     ;; Prettify `eldoc-box' frame
     (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
         (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

(use-package xref
  :ensure t
  :defer t
  :autoload xref-show-definitions-completing-read
  :bind (("M-g ." . xref-find-definitions)
       ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
   (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
      xref-show-xrefs-function #'xref-show-definitions-completing-read))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package indent-bars
  :defer t
  :ensure t
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.225))
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
			        if_statement with_statement while_statement)))
  :hook ((prog-mode yaml-mode) . indent-bars-mode)
  :config (require 'indent-bars-ts))

(use-package aggressive-indent
  :ensure t
  :diminish
  :autoload aggressive-indent-mode
  :defer t
  :bind ("C-x C-a" . aggressive-indent-mode)
  :hook (after-init . global-aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)))

(use-package prettier-js
  :ensure t
  :defer t
  :config
  (setq prettier-js-command "prettierd")
  :hook ((js-ts-mode . prettier-js-mode)))


(use-package yaml-mode
  :ensure t
  :mode "\.yml\'")

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
