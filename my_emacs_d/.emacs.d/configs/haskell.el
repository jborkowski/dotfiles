(packages-conditional-install '(haskell-mode lsp-ui lsp-haskell flycheck company-lsp lsp-treemacs helm-lsp dap-mode nix-update nix-mode nix-buffer nix-sandbox nix-haskell-mode direnv ormolu cl))


;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "$SHELL -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; (when window-system (set-exec-path-from-shell-PATH))

(require 'cl)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
;;  :config
  ;; (nmap 'haskell-mode-map
  ;;       ("C-c r" 'ormolu-format-buffer))
;;   (nmap haskell-mode-map
;;       ("C-c r" . ormolu-format-buffer))
)


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
  ;; avoid to run hie with ghcie in the same time
;;  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package company-lsp
  :commands company-lsp
  )
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  )
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  )
(use-package dap-mode)
(use-package yasnippet
  :ensure t
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

(use-package nix-haskell-mode
  :hook (haskell-mode . nix-haskell-mode)
  )

;; (setq flycheck-command-wrapper-function
;;         (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
;;       flycheck-executable-find
;;         (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))


(add-to-list 'exec-path "/usr/local/bin/")
