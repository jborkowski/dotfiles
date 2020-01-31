
;;(require 'haskell-mode)
(require 'lsp-ui)
(require 'lsp-haskell)
(require 'lsp-treemacs)
(require 'helm-lsp)
(require 'dap-mode)
(require 'nix-update)
(require 'nix-mode)
(require 'nix-buffer)
(require 'nix-sandbox)
(require 'direnv)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; NIX

(use-package nix-update)

(use-package nix-mode
  :hook
  (nix-mode . (lambda () (format-all-mode 1)))
  )

(use-package nix-sandbox)

(use-package nix-buffer)

(use-package direnv
  :init (use-package projectile-direnv)
  :hook
  (after-init . direnv-mode)
  )

;; LSP
(use-package lsp-mode
  :ensure t
  :commands lsp)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  )
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :after lsp
  :custom
  (lsp-haskell-process-path-hie "hie-wrapper")
  (default-nix-wrapper (lambda (args)
                         (let ((sandbox (nix-current-sandbox))
                               (nix-shell "nix-shell"))
                           (if (and (executable-find nix-shell)
                                    (file-exists-p sandbox)
                                    (not (file-directory-p sandbox)))
                               (append
                                (list nix-shell "-I" "." "--command")
                                (list (mapconcat 'identity args " "))
                                (list sandbox)
                                )
                             args
                             )
                           )
                         ))

  (lsp-haskell-process-wrapper-function default-nix-wrapper)
  ;; (haskell-enable-hindent t)
  (haskell-enable-hlint t)
  (haskell-completion-backend 'lsp)
  (haskell-process-type 'cabal-new-repl)
  )

;; (use-package nix-haskell-mode
;;   :hook (haskell-mode . nix-haskell-mode)
;;   )

