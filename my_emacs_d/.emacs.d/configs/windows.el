(packages-conditional-install '(ace-window zoom-window ))

(global-set-key (kbd "M-`") 'ace-window)
(key-chord-define-global "cw" 'ace-window)
(key-chord-define-global "sw" 'ace-swap-window)
(key-chord-define-global "ms" 'ace-maximize-window)
(key-chord-define-global "zw" 'zoom-window-zoom)

(key-chord-define-global "gh" 'ace-window)

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
