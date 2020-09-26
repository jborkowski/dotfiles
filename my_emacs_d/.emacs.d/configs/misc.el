;; All other packages
(packages-conditional-install '(keyfreq nyan-mode company eno elmacro auto-highlight-symbol hl-todo wttrin))

;; collects stats of keyusage
(require 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        previous-line
        next-line))

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; nyancut flying around :)
(nyan-mode 1)

;; copy buffer's path to clipboard
(defun put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; jumping betweetn TODOs
(defhydra hydra-todo (:pre
                  (hl-todo-mode 1)
              :post
             (hl-todo-mode -1))
  "Todo"
  ("n" hl-todo-next "Next")
  ("p" hl-todo-previous "Previous")
  ("o" hl-todo-occur "Occur")
  ("q" nil "Quit" :color blue :exit t))

(setq wttrin-default-cities '("Warsaw"))
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))

(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Terminal
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :hook (vterm-mode . turn-off-chrome)

  :config
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))

  (evil-define-key* 'insert vterm-mode-map
    (kbd "C-a") #'vterm--self-insert
    (kbd "C-b") #'vterm--self-insert ; Should not be necessary.
    (kbd "C-d") #'vterm--self-insert
    (kbd "C-e") #'vterm--self-insert
    (kbd "C-f") #'vterm--self-insert ; Should not be necessary.
    (kbd "C-k") #'vterm--self-insert
    (kbd "C-l") #'vterm--self-insert ; Should not be necessary.
    (kbd "C-n") #'vterm--self-insert
    (kbd "C-o") #'vterm--self-insert
    (kbd "C-p") #'vterm--self-insert
    (kbd "C-q") #'vterm--self-insert ; Should not be necessary.
    (kbd "C-r") #'vterm--self-insert
    (kbd "C-s") #'vterm--self-insert ; Should not be necessary.
    (kbd "C-t") #'vterm--self-insert
    (kbd "C-u") #'vterm--self-insert ; Should not be necessary.
    (kbd "C-v") #'vterm--self-insert ; Should not be necessary.
    (kbd "C-w") #'vterm--self-insert
    (kbd "C-y") #'vterm--self-insert
    (kbd "C-z") #'vterm--self-insert)

  )

(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                ;;(direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Yaml, Toml, Dockerfile, Markdown
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package toml-mode)
(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode))
