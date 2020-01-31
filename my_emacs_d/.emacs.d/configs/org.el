;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(packages-conditional-install '(org org-bullets org-autolist ob-ammonite ob-http ob-go ob-rust ob-sql-mode))

(require 'org)
(require 'org-bullets)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package org-autolist
  :ensure)
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-agenda-files (directory-files-recursively "~/org/" "\.org$"))

(setq org-return-follows-link t)

(setq org-refile-targets '(
   (nil :maxlevel . 2)             ; refile to headings in the current buffer
   (org-agenda-files :maxlevel . 2) ; refile to any of these files
   ))

;; (setq org-capture-templates
;;       '(("n"  "Notes" entry (file+headline "~/org/notes.org" "Notes")
;;          "* %^{Title} %U \n %i")
;;         ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
;;          "* TODO %?\n  %i\n  %a")
;;         ))
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
    "* TODO %?\n  %i")
   ("n" "Notes" entry (file+datetree "~/org/notes.org")
    "* %? %U")))

(defun markdown-convert-buffer-to-org ()
    "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
    (interactive)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f markdown -t org -o %s"
                                     (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

 
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
