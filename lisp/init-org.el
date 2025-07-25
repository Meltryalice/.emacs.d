;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t)
  )

(use-package org-modern
  :ensure t
  :hook (after-init . (lambda ()
                        (setq org-modern-hide-stars 'leading)
                        (global-org-modern-mode t)))
  :config
  (setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
  (setq-default line-spacing 0.1)
  (setq org-modern-label-border 1)
  (setq org-modern-table-vertical 2)
  (setq org-modern-table-horizontal 0)
  (setq org-modern-checkbox
        '((?X . #("▢✓" 0 2 (composition ((2)))))
     (?- . #("▢–" 0 2 (composition ((2)))))
        (?\s . #("▢" 0 1 (composition ((1)))))))
  (setq org-modern-list
        '((?- . "•")
        (?+ . "◦")
        (?* . "▹")))
  (setq org-modern-block-fringe t)
  (setq org-modern-block-name nil)
  (setq org-modern-keyword nil)
  )


(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
