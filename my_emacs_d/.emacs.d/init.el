;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add package repositories
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade.ferrier.me.uk/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)


(when (memq window-system '(mac ns))
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add use-package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global UI configs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-linum-mode t)     ;; show line numbers
(tool-bar-mode 0)         ;; no tool bar
(menu-bar-mode 0)         ;; no menu bar
(scroll-bar-mode 0)       ;; turn off scroll bar
(show-paren-mode 1)       ;; highlight matchin paranthesis

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)

(use-package diminish
  :config (diminish 'eldoc-mode))

(fset `yes-or-no-p `y-or-n-p)

(global-auto-revert-mode t)

(use-package all-the-icons)
(use-package spacemacs-common
    :ensure spacemacs-theme
    :config (load-theme 'spacemacs-dark t))
;;(load-theme 'spacemacs-dark)

;; quick-switch-themes allows to quickly toggle between defined themes
(defvar quick-switch-themes
  (let ((themes-list (list 'spacemacs-dark
                           'spacemacs-light )))
    (nconc themes-list themes-list)))
(defun quick-switch-themes* ()
  (interactive)
  (if-let* ((next-theme (cadr quick-switch-themes)))
      (progn (when-let* ((current-theme (car quick-switch-themes)))
               (disable-theme (car quick-switch-themes)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq quick-switch-themes (cdr quick-switch-themes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Zoom window
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/configs/frame-fns.el")
(load "~/.emacs.d/configs/frame-cmds.el")
(load "~/.emacs.d/configs/zoom-frm.el")
(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eyebrowse)
(eyebrowse-mode t)

(use-package dumb-jump)

(use-package duplicate-thing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Evil-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil)
(use-package evil-leader)
(use-package evil-mc)

(require 'evil)
(require 'evil-leader)
(global-evil-mc-mode  1)

(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)

(use-package evil-magit)
(use-package evil-surround)
(global-evil-surround-mode 1)
(use-package evil-nerd-commenter)

(setq evil-normal-state-cursor '(box "yellow"))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "y"  'helm-show-kill-ring
  "u"  'undo-tree-visualize
;; "r"  'undo-tree-visualize-redo
  "bb" 'helm-mini
  "bp" 'helm-projectile-find-file
  "br" 'helm-projectile-recentf
  "ww" 'ace-window
  "w1" 'eyebrowse-switch-to-window-config-1
  "w2" 'eyebrowse-switch-to-window-config-2
  "w3" 'eyebrowse-switch-to-window-config-3
  "w4" 'eyebrowse-switch-to-window-config-4
  "wv" 'split-window-horizontally
  "wh" 'split-window-vertically
  "wx" 'ace-delete-window
  "k"  'kill-buffer
  "g"  'hydra-git/body
  "m"  'major-mode-hydra
  "p"  'helm-projectile-switch-project
  "nn" 'eno-word-goto
  "n]" 'sp-backward-sexp
  "n[" 'sp-forward-sexp
  "nl" 'goto-line
  "nc" 'goto-last-change
  "nw" 'evil-avy-goto-char-timer
  "jj" 'dumb-jump-go
  "jb" 'dumb-jump-back
  "jw" 'dumb-jump-go-prompt
  "0"  'treemacs-select-window
  "do" 'treemacs-delete-other-windows
  "tt" 'treemacs
  "tb" 'treemacs-bookmark
  "tf" 'treemacs-find-file
  "ff" 'treemacs-find-tag
  "st" 'vterm-toggle)

(key-chord-define-global "fj" 'evil-normal-state)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(evil-mode 1)
(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :diminish
  :bind (("C-." . #'company-complete))
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0 "Faster!")
  (company-echo-delay 0 "Zero waste")
  (company-minimum-prefix-length 2)
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  (company-transformers '(company-sort-by-o))
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))


  :config
  (add-to-list 'company-backends 'company-etags)

  (global-company-mode t)
  ;; numberic helper to select company completition candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
			`(lambda () (interactive) (company-complete-number ,x))))
	  (number-sequence 0 9))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Direnv
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package direnv
  :config
  (direnv-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit is a powerful interface to git
(use-package pretty-hydra)

(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-x G" . #'magit-status))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))


(use-package libgit)

(use-package magit-libgit
  :after (magit libgit))

(use-package git-timemachine)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package git-gutter-fringe+)
(global-git-gutter+-mode t)
;; refresh gutter when staged by magit
(defun my-refresh-visible-git-gutter-buffers ()
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (and git-gutter+-mode (get-buffer-window buff))
        (git-gutter+-mode t)))))

(add-hook 'magit-post-refresh-hook
          #'my-refresh-visible-git-gutter-buffers)

(pretty-hydra-define hydra-git (:foreign-keys warn :title "Git" :quit-key "q" :exit t)
  ("Magit"
   (("o" magit "open")
    ("b" magit-blame "blame")
   )

   "Hunks"
   (("w" git-gutter+-show-hunk "show hunk" :exit nil)
    ("k" git-gutter+-previous-hunk "previous hunk" :exit nil)
    ("j" git-gutter+-next-hunk "previous hunk" :exit nil)
    ("x" git-gutter+-revert-hunk "kill hunk" :exit nil)
    ("s" git-gutter+-stage-hunks "stage hunk" :exit nil)
   )
   "Other"
   (("t" git-timemachine "time machine")
   )
   ))
(global-set-key (kbd "C-c g") 'hydra-git/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hydras
;;
;; Majore Mode Hydra is needed to be installed first so that we can later on
;; define hydras per major mode (e.g for haskell, orm-mode)
;;
;; Key bind to C-space which under this configuration is control-space.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package major-mode-hydra
  :bind
  ("M-C-SPC" . major-mode-hydra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell-checking
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell)
(use-package flyspell-correct-helm
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Haskell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package haskell-mode)
(load "~/.emacs.d/configs/haskell")
(use-package haskell-snippets
  :after (haskell-mode yasnippet)
  :defer)

(setq haskell-import-mapping
      '(("Data.Text" . "import qualified Data.Text as T
import Data.Text (Text)
")
        ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT
")
        ("Data.ByteString" . "import qualified Data.ByteString as S
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as L
")
        ("Data.Map" . "import qualified Data.Map as M
import Data.Map (Map)
")
        ("Data.HashMap" . "import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
")
        ("Data.Set" . "import qualified Data.Set as Set
import Data.Set (Set)
")
        ("Data.Vector" . "import qualified Data.Vector as V
import Data.Vector (Vector)
")
        ("Data.List.NonEmpty" . "import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
")
        ("Data.Conduit.List" . "import qualified Data.Conduit.List as CL
")
        ("Data.Conduit.Binary" . "import qualified Data.Conduit.Binary as CB
")
        ("Data.Sequence" . "import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
")))

(setq haskell-imports-helm-source
      `((name . "*helm* Insert Haskell import")
        (candidates . ,haskell-import-mapping)
        (action . (lambda (candidate)
                    (helm-marked-candidates)))))

(defun haskell-imports-helm ()
  (interactive)
  (insert
   (mapconcat 'identity
              (helm :sources '(haskell-imports-helm-source))
              ",")))

(major-mode-hydra-define haskell-mode nil
  ("Navigation"
   (("o" haskell-navigate-imports  "imports"))
   "Editing"
   (("i" haskell-imports-helm  "imports")
    ("y" yas-describe-tables "snippets")
    ("l" lsp "hie")
    )
   "Documentation"
   (("h" hoogle "hoogle"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GC TuneUp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar file-name-handler-alist-backup
        file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)
(add-hook 'after-init-hook
  (lambda ()
    (garbage-collect)
    (setq gc-cons-threshold
            (car (get 'gc-cons-threshold 'standard-value))
      file-name-handler-alist
        (append
          file-name-handler-alist-backup
          file-name-handler-alist))))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq exec-path-from-shell-check-startup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macOS specific configutation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; needed for PATH recognition
(use-package exec-path-from-shell)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-initialize)

;; bind meta and super to cmd and option
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  )

;Polish characters
(setq mac-right-option-modifier nil)

; Disable notifier
(setq visible-bell 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load depdended packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/configs/install_first")
(load "~/.emacs.d/configs/hydras")
(load "~/.emacs.d/configs/yasnippet")
(load "~/.emacs.d/configs/scala")
(load "~/.emacs.d/configs/misc")
(load "~/.emacs.d/configs/ui")
(load "~/.emacs.d/configs/editing")
(load "~/.emacs.d/configs/project")
(load "~/.emacs.d/configs/windows")
(load "~/.emacs.d/configs/other")
(load "~/.emacs.d/configs/org")
(load "~/.emacs.d/configs/greek")
(load "~/.emacs.d/configs/fira")
(load "~/.emacs.d/configs/javascript")
(load "~/.emacs.d/configs/java")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-async-timeout 20 t)
 '(company-dabbrev-downcase nil)
 '(company-echo-delay 0 t)
 '(company-idle-delay 0)
 '(company-lsp-enable-snippet t)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(company-tooltip-limit 20)
 '(company-transformers
   '(company-sort-by-occurrence company-sort-by-backend-importance))
 '(inhibit-startup-screen t)
 '(lsp-diagnostics-modeline-scope :project t)
 '(lsp-file-watch-threshold 5000)
 '(lsp-log-io t)
 '(lsp-modeline-diagnostics-scope :project t)
 '(lsp-print-performance t)
 '(lsp-ui-doc-delay 0.75)
 '(org-agenda-files
   '("/Users/jobo/org/weekly/23July2019.org" "/Users/jobo/org/.#inbox.org" "/Users/jobo/org/.#orgy.org" "/Users/jobo/org/.#todo.org" "/Users/jobo/org/MAM.org" "/Users/jobo/org/adform_interview.org" "/Users/jobo/org/advent-of-code-in-haskell.org" "/Users/jobo/org/alfa.org" "/Users/jobo/org/bidder-stats.org" "/Users/jobo/org/books-notes.org" "/Users/jobo/org/check_n_times_bash.org" "/Users/jobo/org/citi.org" "/Users/jobo/org/configure-emacs.org" "/Users/jobo/org/diving.org" "/Users/jobo/org/driver-lic-a.org" "/Users/jobo/org/english.org" "/Users/jobo/org/ex.org" "/Users/jobo/org/gtd.org" "/Users/jobo/org/haskell-thoughts.org" "/Users/jobo/org/haskell.org" "/Users/jobo/org/inbox.org" "/Users/jobo/org/index.org" "/Users/jobo/org/itm-panel.org" "/Users/jobo/org/japanese.org" "/Users/jobo/org/journal.org" "/Users/jobo/org/mobileorg.org" "/Users/jobo/org/norway.org" "/Users/jobo/org/notes.org" "/Users/jobo/org/orgy.org" "/Users/jobo/org/personal.org" "/Users/jobo/org/risk_analysis.org" "/Users/jobo/org/todo.org" "/Users/jobo/org/ubs-interver.org" "/Users/jobo/org/uczenie-indukcyjne.org" "/Users/jobo/org/work.org" "/Users/jobo/org/zadanie-forum.org" "/Users/jobo/org/zadanie-parser.org" "/Users/jobo/org/zio-introduction.org"))
 '(org-default-notes-file "~/org/notes.org")
 '(org-directory "~/org")
 '(org-footnote-section "")
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)))
 '(org-return-follows-link t)
 '(org-src-ask-before-returning-to-edit-buffer nil)
 '(org-src-window-setup 'split-window-below)
 '(package-selected-packages
   '(lsp-metals meghanada erc-crypt dante dap-java toml-mode dockerfile-mode vterm-toggle vterm ace-mc diminish
		(tide)
		treemacs-projectile treemacs-evil spacemacs-theme ox-epub ox-pandoc python-mode ## all-the-icons-dired all-the-icons git-gutter-fringe+ git-timemachine evil-org evil-nerd-commenter evil-surround evil-magit evil-ledger evil-mc evil-leader evil duplicate-thing dumb-jump eyebrowse nix-haskell-mode nix-sandbox hie-nix direnv dap-mode helm-lsp lsp-treemacs ob-sql-mode ob-rust ob-go ob-http ob-ammonite org-kindle org-blog flycheck-haskell smartparens ace-window avy bash-completion csv-mode eglot emojify flymake ghub git-commit graphql-mode hl-todo htmlize hydra jsonrpc lsp-ui lv magit sbt-mode scala-mode transient treepy which-key with-editor yasnippet-snippets helm-ag helm-ag-r helm-etags-plus helm-projectile zoom-window zoom yasnippet-classic-snippets yaml-mode wttrin use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords terraform-mode terminal-here string-edit stack-mode react-snippets purescript-mode org-bullets nyan-mode neotree multiple-cursors moe-theme keyfreq json-mode idris-mode highlight-symbol goto-chg exec-path-from-shell etags-select eno encourage-mode elmacro ebdb ctags company-lsp auto-package-update auto-highlight-symbol annoying-arrows-mode angular-mode ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
