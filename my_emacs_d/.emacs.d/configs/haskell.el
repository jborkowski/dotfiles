(packages-conditional-install '(haskell-mode lsp-ui eglot hasky-stack))
(require 'hasky-stack)

;; LSP
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
 (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
 )

;; EGLOT
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp"))))

(add-to-list 'exec-path "/usr/local/bin/")
