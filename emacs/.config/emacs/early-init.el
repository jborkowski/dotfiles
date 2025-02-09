;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:

;; Since Emacs 27, an early configuration file early-init.el can be provided to
;; handle initialization to be done before init.el is loaded. I use it to disable
;; GUI elements that I do not need and to inhibit package.el.

;;; Code:

;; Adjust garbage collection thresholds during startup, and thereafter
(setopt gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

(defun +gc-after-focus-change-fn ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values-h ()
  "Restore defalut values after init."
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setopt gc-cons-threshold (* 20 1024 1024)
             gc-cons-percentage 0.1)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change-fn)))))

(add-hook 'emacs-startup-hook '+reset-init-values-h)

;; Set the file-name-handler to nil (because regexing is cpu intensive and we do
;; not need Emacs extremely flexible mechanism for loading files)
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setopt file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setopt file-name-handler-alist
                        (delete-dups (append file-name-handler-alist
                                             old-file-name-handler-alist)))))))

;; Helper function to get CPU architecture 
(defun +get-arch() 
  "Return current CPU architecture"
  (let ((arch (car (split-string system-configuration "-")))) 
    (cond ((string-match-p "^x86_64\\|^[3-6]86" arch) "x86") 
          ((string-match-p "^aarch64" arch) "aarch64")
          ((string-match-p "^arm" arch) "arm")
          (t "unknown"))))


;; Change location of the native compilation cache with arch-specific subdirectory
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name (format "var/%s/eln-cache/" (+get-arch))
                      user-emacs-directory))))


;; (when (featurep 'native-compile)
;;   ;; Set the right directory to store the native compilation cache
;;   (let ((path (expand-file-name (concat "eln-cache-" (system-name) "/") user-emacs-directory)))
;;     (setq-default native-comp-eln-load-path       (list path)
;;                   native-compile-target-directory path)
;;     (startup-redirect-eln-cache path)
;;   (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings as they can be pretty disruptive
;;                 native-comp-jit-compilation              t    ;; Make native compilation happens asynchronously
;;                 package-native-compile                   t)))  ;; Compile installed packages

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq-default load-prefer-newer noninteractive)

;; Packages will be initialized later
(setopt package-enable-at-startup nil)

;; Silence compiler warnings
(setopt native-comp-async-report-warnings-errors 'silent)

;; Remove old versions of native-compiled files
(setopt native-compile-prune-cache t)

;; Process performance tuning
(setopt process-adaptive-read-buffering nil
        read-process-output-max (* 4 1024 1024))

;; Prevent printing instructions on how to close an emacsclient frame.
(setopt server-client-instructions nil)

;; In the beginning there was no fancy
(setopt fancy-startup-text nil
        fancy-about-text nil)

;; Set frame settings early
(setopt frame-inhibit-implied-resize t  ; do not resize the frame at this early stage.
        frame-resize-pixelwise t        ; fully cover the screen when maximized
        frame-title-format '("%b"))     ; a bit more meaningful frame title

;; Disable some things right away
(setopt inhibit-x-resources t
        inhibit-splash-screen t
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-buffer-menu t
        initial-scratch-message nil
        ring-bell-function #'ignore    ; disable sound notification on error/keyboard quit
        visible-bell nil               ; disable visual notification on error/keyboard quit
        tooltip-mode nil               ; I have no need for tooltips
        use-dialog-box nil             ; prompts should go in the minibuffer, not in a GUI.
        use-short-answers t            ; never use `yes-or-no-p', prefer `y-or-n-p'
        warning-minimum-level :error)  ; I don't care about your warnings, just give me errors!

;; Disable additional graphical elements
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(menu-bar-mode -1)

;; Set initial frame parameters
(modify-all-frames-parameters
 `((horizontal-scroll-bars . nil)
   (vertical-scroll-bars . nil)
   (width . (text-pixels . 1200))
   (height . (text-pixels . 900))))

(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here

