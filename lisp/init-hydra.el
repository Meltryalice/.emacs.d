;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package hydra
  :ensure t)

(use-package pretty-hydra
  :ensure t
  :after hydra
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                          &key face height v-adjust)
   "Add an icon in the hydra title."
   (let ((face (or face 'mode-line-emphasis))
       (height (or height 1.2))
       (v-adjust (or v-adjust 0.0)))
     (concat
      (when (and icon-type icon-name)
       (let ((f (intern (format "nerd-icons-%s" icon-type))))
         (when (fboundp f)
           (concat
            (apply f (list icon-name :face face :height height :v-adjust v-adjust))
            " "))))
      (propertize title 'face face))))
  )

(provide 'init-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
