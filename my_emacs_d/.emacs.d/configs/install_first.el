
;; util functions to instal packages if they don't exists
(defun package-conditional-install (package-name)
  "Install a package if it is not present"
  (unless (package-installed-p package-name)
    (package-refresh-contents) (package-install package-name)))

(defun packages-conditional-install (packages)
  "Install list of packages if not present"
  (when packages
    (package-conditional-install (car packages))
    (packages-conditional-install (cdr packages))))

(packages-conditional-install '(key-chord hydra auto-package-update etags-select helm-etags-plus ctags eglot emojify json-mode purescript-mode terraform-mode yaml-mode wttrin which-key use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords terminal-here string-edit stack-mode smartparens react-snippets org-bullets nyan-mode neotree multiple-cursors monokai-theme moe-theme magit lsp-ui keyfreq intero idris-mode hydra htmlize hl-todo highlight-symbol helm-projectile helm-ag hasky-stack graphql-mode goto-chg ghub exec-path-from-shell eno encourage-mode elmacro ebdb csv-mode bash-completion auto-package-update auto-highlight-symbol annoying-arrows-mode angular-mode))


;; updated all packages on startup
(auto-package-update-maybe)

;; auto-refresh all buffers when files have changed on disk
(global-auto-revert-mode t)

;; add ability to bind actions to series of key strokes
(key-chord-mode 1)

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

(require 'hydra)