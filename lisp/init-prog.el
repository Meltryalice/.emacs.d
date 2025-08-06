;;; init-prog.el --- prog settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

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
  :hook (after-init . global-aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)))

(use-package apheleia
  :ensure t
  :defer t
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'prettier apheleia-formatters)
      '("prettier" "--stdin-filepath" filepath)))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
