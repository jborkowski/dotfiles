(packages-conditional-install '(sbt-mode scala-mode))
;; bug in sbt-mode fixed 
(setq sbt:prefer-nested-projects t)

(add-hook 'sbt-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  `((,(expand-file-name (directory-file-name default-directory)) . ?⌂)
                    (,(expand-file-name "~") . ?~)))
            (prettify-symbols-mode t)))


(packages-conditional-install '(flycheck lsp-mode lsp-ui company-lsp scala-mode yasnippet sbt-mode))

;; Uncomment this after tests...
;; ;; Enable scala-mode and sbt-mode
;; (use-package scala-mode
;;   :mode "\\.s\\(cala\\|bt\\)$")

;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map))

;; ;; Enable nice rendering of diagnostics like compile errors.
;; (use-package flycheck
;;   :init (global-flycheck-mode))

;; (use-package lsp-mode
;;   ;; Optional - enable lsp-mode automatically in scala files
;;   :hook (scala-mode . lsp)
;;   :config (setq lsp-prefer-flymake nil))

;; (use-package lsp-ui)

;; ;; Add company-lsp backend for metals
;; (use-package company-lsp)

(require 'sbt-mode)
(setq compilation-auto-jump-to-first-error t)
(global-set-key (kbd "C-c s") 'sbt-hydra)