(packages-conditional-install '(smartparens projectile projectile-direnv recentf ag helm-ag helm helm-projectile))

(require 'recentf)
(require 'helm-projectile)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

(global-set-key (kbd "C-c F") 'helm-do-ag-project-root)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x r l") 'helm-bookmarks)
(key-chord-define-global "lm" 'helm-do-ag-this-file)
(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "gm" 'helm-do-ag-project-root)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

(use-package projectile
  :diminish
  :bind (("C-c k" . #'projectile-kill-buffers)
	 ("C-c M" . #'projectile-compile-project))

  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(show-paren-mode 1)
(column-number-mode 1)
(smartparens-global-mode 1)

(global-linum-mode 1)
