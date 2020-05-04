;;(require 'haskell-mode)
(require 'lsp-ui)
(require 'lsp-haskell)
(require 'lsp-treemacs)
(require 'helm-lsp)
(require 'dap-mode)

(add-to-list 'exec-path "/usr/local/bin/")
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; LSP
(use-package lsp-mode
  :ensure t
  :commands lsp)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  )
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init (setq lsp-ui-doc-enable t
                 lsp-ui-doc-use-webkit nil
                 lsp-ui-doc-delay 0.2
                 lsp-ui-doc-include-signature t
                 lsp-ui-doc-position 'at-point
                 lsp-ui-doc-border (face-foreground 'default)
                 lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer
                 lsp-ui-sideline-enable t
                 lsp-ui-sideline-show-hover nil
                 lsp-ui-sideline-show-diagnostics nil
                 lsp-ui-sideline-ignore-duplicate t
                 lsp-ui-imenu-enable t)
  :config
     (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
     ;; `C-g'to close doc
     (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
     )
(use-package lsp-haskell
  :after lsp
  :custom
  ;; (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '())
  (setq lsp-haskell-process-path-hie "/Users/jborkowski/.local/bin/hie-wrapper")
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
  ;; (lsp-haskell-process-wrapper-function default-nix-wrapper)
  ;; (haskell-enable-hindent t)
  (haskell-enable-hlint t)
  (haskell-completion-backend 'lsp)
  (haskell-process-type 'cabal-new-repl)
 )
