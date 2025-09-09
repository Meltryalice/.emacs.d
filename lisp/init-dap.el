;; init-dap.el --- Initialize DAP configurations.	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dape
  :ensure t
  :defer t
  :bind (("<f5>" . dape)
	 ("M-<f5>" . dape-hydra/body))
  :custom (dape-buffer-window-arrangment 'right)
  :config
  (add-to-list 'dape-configs
           `(vscode-js-node
             modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
             host "localhost"
             port 8123
             command "node"
             command-cwd ,(file-name-concat user-emacs-directory "debug-adapters/vscode-js-debug/dist")
             command-args ("src/dapDebugServer.js" "8123")
             :type "pwa-node"
             :request "launch"
             :cwd dape-cwd-fn
             :program dape-buffer-default
             :outputCapture "console"
             :sourceMapRenames t
             :pauseForSourceMap nil
             :enableContentValidation t
             :autoAttachChildProcesses t
             :console "internalConsole"
             :killBehavior "forceful"))
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks
         (defun dape--save-on-start ()
           (save-some-buffers t t)))
  ;; Display hydra on startup
  (add-hook 'dape-on-start-hooks #'dape-hydra/body))

(provide 'init-dap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dap.el ends here
