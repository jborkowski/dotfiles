;;; init.el --- Init File -*- lexical-binding: t; -*-

(setq gc-cons-threshold #x40000000)

;; Helper function to get CPU architecture
;; Move it to separate file?
(defun +get-arch()
  "Return current CPU architecture"
  (let ((arch (car (split-string system-configuration "-"))))
    (cond ((string-match-p "^x86_64\\|^[3-6]86" arch) "x86")
          ((string-match-p "^aarch64" arch) "aarch64")
          ((string-match-p "^arm" arch) "arm")
          (t "unknown"))))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

(defvar cache-dir
  (expand-file-name (format "var/%s/cache/" (+get-arch))
                    user-emacs-directory))

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name (format "var/%s/elpaca/" (+get-arch)) user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;; Auth source
;; TODO: Integrate with Bitwarden
(use-package auth-source
  :ensure nil
  :custom
  (auth-sources'("~/.authinfo.gpg"))
  (auth-source-cache-exipry nil)
  (password-cache-expiry nil))

;; Prohibit littering
(elpaca no-littering
        (use-package no-littering
          :demand
          :custom
          (custom-file (no-littering-expand-var-file-name "custom.el"))
          (auto-save-file-name-transforms
           `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
          (backup-directory-alist
           `((".*" . ,(no-littering-expand-var-file-name "backup/"))
             ("^/dev/shm/" . nil) ;; do not backup files in RAM
             ("^/tmp/" . nil))))  ;; do not backup files in /tmp/

        (when (file-exists-p custom-file)
          (load custom-file)))

;; Personal information
(setq user-full-name "Jonatan Borkowski"
      user-mail-address "jonatan@thebo.me")


(when *is-a-mac* (setq x-alt-keysym 'meta))


;; Theme
(setopt custom-safe-themes t)
(add-to-list 'default-frame-alist '(undecorated . t))

(use-package modus-themes
  :ensure t
  :demand t
  :bind ("<leader> t t" . modus-themes-toggle)
  :custom
  (modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))

  (modus-vivendi-deuteranopia-palette-overrides
   '((bg-main "#101212")
     (bg-dim "#0d1117")
     (bg-alt "#161b22")
     (fg-main "#c9d1d9")
     (fg-alt "#8b949e")
     (border "#30363d")
     (fringe "#101212")
     (cursor "#58a6ff")
     (constant "#79c0ff")
     (comment "#8b949e")
     (keyword "#ff7b72")
     (string "#a5d6ff")
     (function "#d2a8ff")
     (region "#264f78")
     (link "#58a6ff")
     (error "#ff6e6e")
     (warning "#e3b341")
     (success "#56d364")))
  :config
  (defun +os/theme ()
    "Retrieve the OS theme (dark or light)."
    (downcase
     (if *is-a-linux*
         (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")
       (shell-command-to-string "defaults read -g AppleInterfaceStyle"))))

  ;; Automatically load the appropriate theme based on the OS setting
  (if (string-match-p "dark" (+os/theme))
      (load-theme 'modus-vivendi-deuteranopia t)
    (load-theme 'modus-operandi-deuteranopia t)))

;; Evil!
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)

  :config
  (setq evil-leader/in-all-states t)
  (setq evil-want-fine-undo t)
  (evil-set-undo-system 'undo-fu)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))






  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader> p f") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp)
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers)
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired)

  ;; Enable evil mode
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :defer t
  :custom
  (evil-collection-want-find-usages-bindings t))

(add-hook 'evil-mode-hook #'evil-collection-init)

(use-package evil-surround
  :ensure t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

;; Recent files
(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-save-file (concat cache-dir "recentf"))
  (recentf-max-menu-items 1000)
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup nil)
  :config
  ;; Emacs (unless this is a long-running daemon session).
  (setq recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;; Autosave and backups
(use-package files
  :ensure nil
  :custom
  (auto-save-default t)
  (auto-save-list-file-prefix (concat cache-dir "autosave/"))
  (make-backup-files t)
  (require-final-newline t)
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 6)
  (create-lockfiles nil))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :after evil
  :bind
  ("C-x C-r" . consult-recent-file)
  ("C-x b"   . consult-buffer)
  ("C-x r b" . consult-bookmark)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("C-c c x" . consult-flymake)

  ("M-y"     . consult-yank-pop)
  ("M-g e"   . consult-compile-error)
  ("M-g f"   . consult-flycheck)
  ("M-g g"   . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g o"   . consult-outline)
  ("M-g m"   . consult-mark)
  ("M-g k"   . consult-global-mark)
  ("M-g i"   . consult-imenu)

  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)

  ("<leader> s f" . consult-find)              ; Search for file
  ("<leader> s g" . consult-grep)              ; Search with grep
  ("<leader> s r" . consult-ripgrep)           ; Search with ripgrep
  ("<leader> s l" . consult-line)              ; Search in lines
  ("<leader> s b" . consult-buffer)            ; Switch buffer
  ("<leader> s m" . consult-mark)              ; Search global marks
  ("<leader> s o" . consult-outline)           ; Search outline (imenu)

  ("<leader> d x" . consult-xref)              ; Consult xref
  ("<leader> d i" . consult-imenu)             ; Consult imenu

  ("<leader> h r" . consult-recent-file)       ; Recent files
  ("<leader> h b" . consult-buffer)            ; Recent buffers
  ("<leader> h k" . consult-global-mark)       ; Global marks


  ("M-s e" . consult-isearch-history)
  (:map isearch-mode-map
        ("M-e" . consult-isearch-history)          ; o
        ("M-s e" . consult-isearch-history)
        ("M-s l" . consult-line))

  :custom

  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (completion-in-region-function  #'consult-completion-in-region)
  (consult-preview-key "M-.")
  (consult-narrow-key "<"))

;; Consult Dir
(use-package consult-dir
  :ensure t
  :custom (consult-dir-shadow-filenames nil)
  :bind
  ("C-x C-d" . consult-dir)
  (:map vertico-map
	("C-x C-d" . consult-dir)
	("C-x C-j" . consult-dir-jump-file)))

;; Dired
(use-package dired
  :ensure nil
  :hook (dired-mode . hl-line-mode)
  :custom
  (delete-by-moving-to-trash t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-movement-style 'bounded)
  (dired-dwim-target t))

;; Xref
(use-package xref
  :ensure nil
  :custom
  (xref-auto-jump-to-first-definition 'show)
  (xref-search-program 'ripgrep))


;; Project
(use-package project
  :ensure nil
  :bind (:map project-prefix-map ("g" . consult-ripgrep))
  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-dired "Find directory" ?d)
     (consult-ripgrep "Find regexp" ?g)
     (project-switch-to-buffer "Find buffer" ?b)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e)))
  (project-vc-extra-root-markers '("hie.yaml" "package.json" "spago.dhall"))
  (project-vc-ignores '("node_modules" "output" "dist" "tmp")))

;; History
(setq undo-limit 80000000
      history-length 5000
      kill-do-not-save-duplicates t
      create-lockfiles nil
      history-delete-duplicates t)

;; Savehist
(use-package savehist
  :ensure nil
  :defer 1
  :hook (after-init . savehist-mode)
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring
     register-alist
     mark-ring global-mark-ring
     search-ring regexp-search-ring)))


;; Minibuffer
(use-package minibuffer
  :ensure nil
  :defines (crm-separator)
  :functions (crm-indicator)
  :hook (minibuffer-setup . cursor-intangible-mode)
  :custom
  (completion-ignore-case t)
  (completion-auto-select t)
  (completion-auto-help 'visible)
  (completion-show-help nil)
  (completions-detailed t)
  (completions-header-format nil)
  (completions-format 'one-column)

  ;; Tweak minibuffer behaviour
  (resize-mini-windows t)
  (enable-recursive-minibuffers t)
  ;; (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)
  (minibuffer-eldef-shorten-default t)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  :config
  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple'.
 We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; Vertico
(use-package vertico
  :ensure t
  :defines (vertico-map)
  :functions (vertico-mode)
  :init (vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("M-j" . vertico-quick-exit)
        ("M-RET" . vertico-exit-input)
        ("C-j"   . vertico-next)
        ("C-M-j" . vertico-next-group)
        ("C-k"   . vertico-previous)
        ("C-M-k" . vertico-previous-group))

  :custom
  (vertico-mouse-mode t)
  (vertico-scroll-margin 0)
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-count 17)
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  )

;; Orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

;; Marginalia
(use-package marginalia
  :ensure t
  :functions (marginalia-mode)
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil)))
;; Undo-fu
(use-package undo-fu
  :ensure t
  :init
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960)) ; 960mb.

;; Ediff
(use-package ediff
  :ensure nil
  :custom
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Diff-hl
(use-package diff-hl
  :defer t
  :ensure t
  :hook
  (after-init . global-diff-hl-mode)
  (after-init . (lambda () (unless (window-system) (diff-hl-margin-mode))))
  (dired-mode . diff-hl-dired-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)

  :custom
  (diff-hl-side 'left)
  (diff-hl-margin-symbols-alist '((insert . "│")
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i"))))
;; Magit
(use-package transient :ensure t)
(use-package magit
  :ensure t
  :defer t
  :bind ("<leader> g g" . magit-status)
  :after transient
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-revision-insert-related-refs nil)
  (magit-section-visibility-indicator nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package all-the-icons :ensure t)

;; Modeline

(use-package doom-modeline
  :ensure t
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :init
  (setq projectile-dynamic-mode-line nil
        doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-encoding nil
        doom-modeline-project-detection 'project
        doom-modeline-lsp t
        doom-modeline-default-eol-type 2)
  (doom-modeline-mode 1)
  :config
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes check input-method buffer-encoding process vcs "  "))

  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)

  (add-hook 'magit-mode-hook
            (defun +modeline-hide-in-non-status-buffer-h ()
              "Show minimal modeline in magit-status buffer, no modeline elsewhere."
              (if (eq major-mode 'magit-status-mode)
                  (doom-modeline-set-modeline 'magit)
                (hide-mode-line-mode))))
  (set-face-attribute 'mode-line nil :font (font-spec :family "CaskaydiaMono Nerd Font" :size 13 :weight 'regular))
  (set-face-attribute 'mode-line-inactive nil :font (font-spec :family "CaskaydiaMono Nerd Font" :size 13)))

(defun +setup-font-faces-h ()
  "Setup all Emacs font faces."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font (font-spec :family "CaskaydiaMono Nerd Font" :size 14 :weight 'semi-light))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "CaskaydiaMono Nerd Font" :size 14 :weight 'regular))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "CaskaydiaMono Nerd Font" :size 14 :weight 'regular))))

(add-hook 'after-init-hook '+setup-font-faces-h)
(add-hook 'server-after-make-frame-hook '+setup-font-faces-h)

;; Treesiter
(use-package treesit-auto
  :ensure t
  :defer t
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Flymake
(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
     (note "»" compilation-info))))

;; LSP
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (
         (bash-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (js-mode . lsp)
         (js-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)) ;; Integrate with Which Key
  :commands lsp
  :custom
  ;; General Settings
  (lsp-keymap-prefix "<leader> l")
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-idle-delay 0)
  (lsp-auto-configure t)
  (lsp-enable-xref t)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-signature-doc-lines 1)

  ;; Performance Settings
  (lsp-keep-workspace-alive nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-indentation nil)
  (lsp-enable-folding nil)

  ;; Features and UI
  (lsp-enable-imenu t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-links nil)
  (lsp-enable-text-document-color nil)
  (lsp-inlay-hint-enable t)
  (lsp-ui-doc-enable t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-workspace-status-enable t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-lens-enable nil)

  ;; Completion Settings
  (lsp-completion-enable t)
  (lsp-completion-show-kind t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  :config
  ;; TODO Cleanup this!
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'lsp-mode-map
      (kbd "gr") 'lsp-find-references
      (kbd "<leader> c a") 'lsp-execute-code-action
      (kbd "<leader> l r") 'lsp-rename
      (kbd "<leader> l d") 'lsp-find-definition
      (kbd "<leader> l t") 'lsp-find-type-definition
      (kbd "<leader> l i") 'lsp-find-implementation
      (kbd "<leader> l e") 'lsp-execute-code-action
      (kbd "<leader> l f") 'lsp-format-buffer
      (kbd "<leader> l a") 'lsp-find-references
      (kbd "<leader> l h") 'lsp-describe-session
      (kbd "<leader> l s") 'lsp-signature-help))
  )


;; Corfu
(use-package corfu
  :ensure t
  :after savehist
  :init (global-corfu-mode)
  :bind
  (:map corfu-map ("M-j" . corfu-quick-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-history-mode t)
  (corfu-popupinfo-mode t)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-terminal
  :ensure t
  :unless (window-system)
  :hook (corfu-mode . corfu-terminal-mode))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;;
  ;; TODO Bindings!
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . turn-on-visual-line-mode)
  :custom
  (markdown-fontify-code-blocks-natively t))

;; WhichKey
(use-package which-key
  :ensure t
  :defer t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))


;; Rust
(use-package rust-mode
  :ensure t
  :mode
  ("\\.rs\\'"      . rust-mode)
  ("\\.lalrpop\\'" . rust-mode)
  :defines (rust-mode-map)
  :config
  (use-package evil
    :ensure t
    :config
    ;; Bindings for Rust mode in Evil Normal state
    (evil-define-key 'normal rust-mode-map
      (kbd "M-j") 'lsp-ui-imenu
      (kbd "M-?") 'lsp-find-references
      (kbd "<leader> c l") 'flycheck-list-errors
      (kbd "<leader> c x") 'lsp-execute-code-action
      (kbd "<leader> c b") 'rustic-cargo-build
      (kbd "<leader> c r") 'lsp-rename
      (kbd "<leader> c s") 'lsp-rust-analyzer-status
      (kbd "<leader> c A") 'rustic-cargo-add-missing-dependencies)))


;;; init.el ends here
