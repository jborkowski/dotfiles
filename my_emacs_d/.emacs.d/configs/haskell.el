
;; LSP
(use-package yasnippet
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package lsp-mode
  :commands (lsp lsp-deffer)

  :config
  (setq read-process-output-max (* (* 1024 1024) 4))

  :hook
  ((haskell-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  ;; enable when diagnostics needed
  ;; (lsp-mode . lsp-diagnostics-modeline-mode))
  :after yasnippet

  :bind
  ; (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol
  :custom
  (lsp-print-performance t)
  (lsp-log-io t)
  (lsp-diagnostics-modeline-scope :project)
  (lsp-file-watch-threshold 5000)
  )

(use-package lsp-ui
  :custom (lsp-ui-doc-delay 0.75)
  :after lsp-mode)

(use-package helm-lsp
  :after (helm lsp-mode)
  :commands helm-lsp-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package company-lsp
  :custom (company-lsp-enable-snippet t)
  :after (company lsp-mode))

(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
 )

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dante
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  :config
  (setq dante-methods '(new-build bare-ghci))
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  ;; OR for flymake support:
  ;; (add-hook 'haskell-mode-hook 'flymake-mode)
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  (add-hook 'haskell-mode-hook 'dante-mode)
  )
