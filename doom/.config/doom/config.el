;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Jonatan Borkowski"
      user-mail-address "jonatan@thebo.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "CaskaydiaMono Nerd Font" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "CaskaydiaMono Nerd Font" :size 17))

(setq doom-theme 'modus-vivendi-tinted)

(defvar my/theme-toggle-dark 'modus-vivendi-tinted
  "Dark theme")
(defvar my/theme-toggle-light 'modus-operandi-deuteranopia
  "Light theme")
(defvar my/current-theme my/theme-toggle-dark
  "Tracks the current theme")

(defun my/toggle-theme ()
  "Toggle between dark and light themes."
  (interactive)
  (setq my/current-theme
        (if (eq my/current-theme my/theme-toggle-dark)
            my/theme-toggle-light
          my/theme-toggle-dark))
  (load-theme my/current-theme t))

(map! :leader
      :desc "Toggle Theme Variant"
      "t T" #'my/toggle-theme)

(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! gptel
  :config
  (setq! gptel-backend (gptel-make-anthropic "Claude"
                         :stream t :key  (getenv "ANTHROPIC_API_KEY"))))
