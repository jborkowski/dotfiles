;; add package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(global-auto-revert-mode t)

;; order matter
(load "~/.emacs.d/configs/install_first")
(load "~/.emacs.d/configs/hydras")
(load "~/.emacs.d/configs/yasnippet")
(load "~/.emacs.d/configs/haskell")
(load "~/.emacs.d/configs/scala")
(load "~/.emacs.d/configs/misc")
(load "~/.emacs.d/configs/ui")
(load "~/.emacs.d/configs/editing")
(load "~/.emacs.d/configs/project")
(load "~/.emacs.d/configs/git")
(load "~/.emacs.d/configs/windows")
(load "~/.emacs.d/configs/other")
(load "~/.emacs.d/configs/org")
(load "~/.emacs.d/configs/greek")
(load "~/.emacs.d/configs/fira")

;Polish characters
(setq mac-right-option-modifier nil)

;DISABLE BELL
(setq visible-bell 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-kindle org-blog lsp-haskell flycheck-haskell smartparens ace-window avy bash-completion csv-mode eglot emojify flymake ghub git-commit graphql-mode hl-todo htmlize hydra jsonrpc lsp-ui lv magit sbt-mode scala-mode transient treepy which-key with-editor yasnippet-snippets helm-ag helm-ag-r helm-etags-plus helm-projectile zoom-window zoom yasnippet-classic-snippets yaml-mode wttrin use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords terraform-mode terminal-here string-edit stack-mode react-snippets purescript-mode org-bullets nyan-mode neotree multiple-cursors monokai-theme moe-theme keyfreq json-mode idris-mode highlight-symbol hasky-stack goto-chg exec-path-from-shell etags-select eno encourage-mode elmacro ebdb ctags company-lsp auto-package-update auto-highlight-symbol annoying-arrows-mode angular-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
