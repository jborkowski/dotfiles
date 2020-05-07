;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :hook ((org-mode . visual-line-mode))
  :bind (:map org-mode-map
              ("C-c c" . #'org-mode-insert-code)
              ("C-c a f" . #'org-shifttab))
  :custom

  (org-directory "~/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-return-follows-link t)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-src-ask-before-returning-to-edit-buffer nil "org-src is kinda needy out of the box")
  (org-src-window-setup 'split-window-below)
  (org-footnote-section "" "Footnotes don't get their own section.")
  (org-agenda-files (directory-files-recursively "~/org/" "\.org$"))

  :config

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)
  (unbind-key "C-," org-mode-map)
  (unbind-key "C-c ;" org-mode-map)
  (unbind-key "C-c ," org-mode-map)

  (defun org-mode-insert-code ()
    (interactive)
   (org-emphasize ?~))
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))


(org-babel-do-load-languages 'org-babel-load-languages
 '(
   (ammonite . t)
   (awk . t)
   (calc .t)
   (C . t)
   (emacs-lisp . t)
   (haskell . t)
   (gnuplot . t)
   (go . t)
   (js . t)
   (rust . t)
   (haskell . t)
   (http . t)
   (python . t)
   (shell . t)
   (sql . t)
   ))

(defun ck/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "amm")
           (string= lang "shell")
           (string= lang "ledger")
           (string= lang "python")
           (string= lang "emacs-lisp")
           (string= lang "http")
           (string= lang "calc")
           (string= lang "js")
           (string= lang "go"))))
(setq-default org-confirm-babel-evaluate 'ck/org-confirm-babel-evaluate)
