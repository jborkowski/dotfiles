;;; init.el --- Init File -*- lexical-binding: t; -*-


(defvar elpaca-installer-version 0.8)

;; Helper function to get CPU architecture 
(defun +get-arch() 
  "Return current CPU architecture"
  (let ((arch (car (split-string system-configuration "-")))) 
    (cond ((string-match-p "^x86_64\\|^[3-6]86" arch) "x86") 
          ((string-match-p "^aarch64" arch) "aarch64")
          ((string-match-p "^arm" arch) "arm")
          (t "unknown"))))

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


;; Theme
(setopt custom-safe-themes t)
(add-to-list 'default-frame-alist '(undecorated . t))
(use-package modus-themes
  :ensure t
  :demand t
  :bind
  ("C-c t t" . modus-themes-toggle)
  :custom
  (modus-themes-to-toggle
   '(modus-operandi-tinted modus-vivendi-tinted))
  (modus-themes-common-palette-overrides
	 '((prose-done green-intense)
	   (prose-todo red-intense)))
  
  ;; Tone down almost all colors.
  (modus-themes-common-palette-overrides)
	(modus-themes-preset-overrides-faint))

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
  (evil-set-leader 'normal (kbd "SPC")) 
  (evil-set-leader 'visual (kbd "SPC"))



  ;; Enable evil mode
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :defer t
  :custom
  (evil-collection-want-find-usages-bindings t)
  :hook
  (evil-mode . evil-collection-init))

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


;;; init.el ends here
