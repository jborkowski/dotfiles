;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add package repositories
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
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
(toggle-frame-fullscreen) ;; start with fullscreen
(scroll-bar-mode 0)       ;; turn off scroll bar
(show-paren-mode 1)       ;; highlight matchin paranthesis

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
  "r"  'redo-tree-visualize
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
  "ff" 'treemacs-find-tag)

(key-chord-define-global "fj" 'evil-normal-state)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(evil-mode 1)
(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit is a powerful interface to git
(use-package pretty-hydra)
(use-package magit
  :bind ("C-x G" . magit-status))

;; time machine allows inspecting changes on a single file
;; we can move back and forth to see the progress on a given file
(use-package git-timemachine)

;; By default ediff pops out a separate frame for navigation during the difff.
;; This change below keeps the ediff in the same frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; this will refresh buffer if file changed on a disk e.g loaded new branch
(global-auto-revert-mode t)

;; show which lines were added/modfied/removed
;; git-gutter-fringe+ words perfectly with linum-mode but only in
;; graphical environment (this will not work in terminal)
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
(use-package haskell-snippets)
(require 'haskell-snippets)

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    ((tide)
     treemacs-projectile treemacs-evil spacemacs-theme ox-epub ox-pandoc python-mode ## all-the-icons-dired all-the-icons git-gutter-fringe+ git-timemachine evil-org evil-nerd-commenter evil-surround evil-magit evil-ledger evil-mc evil-leader evil duplicate-thing dumb-jump eyebrowse nix-haskell-mode nix-sandbox hie-nix direnv dap-mode helm-lsp lsp-treemacs ob-sql-mode ob-rust ob-go ob-http ob-ammonite org-kindle org-blog flycheck-haskell smartparens ace-window avy bash-completion csv-mode eglot emojify flymake ghub git-commit graphql-mode hl-todo htmlize hydra jsonrpc lsp-ui lv magit sbt-mode scala-mode transient treepy which-key with-editor yasnippet-snippets helm-ag helm-ag-r helm-etags-plus helm-projectile zoom-window zoom yasnippet-classic-snippets yaml-mode wttrin use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords terraform-mode terminal-here string-edit stack-mode react-snippets purescript-mode org-bullets nyan-mode neotree multiple-cursors moe-theme keyfreq json-mode idris-mode highlight-symbol goto-chg exec-path-from-shell etags-select eno encourage-mode elmacro ebdb ctags company-lsp auto-package-update auto-highlight-symbol annoying-arrows-mode angular-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
