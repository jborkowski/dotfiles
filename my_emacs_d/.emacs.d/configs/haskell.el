(packages-conditional-install '(haskell-mode lsp-ui lsp-haskell flycheck company-lsp lsp-treemacs helm-lsp dap-mode))

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; LSP
(use-package company-lsp
  :commands company-lsp)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
(use-package dap-mode)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)
(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "hie-wrapper")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
 )

(add-to-list 'exec-path "/usr/local/bin/")
