(packages-conditional-install '(ace-window zoom-window ace-mc))

(key-chord-define-global "cw" 'ace-window)
(key-chord-define-global "sw" 'ace-swap-window)
(key-chord-define-global "zw" 'zoom-window-zoom)

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

(global-set-key (kbd "C-)") 'ace-mc-add-multiple-cursors)
(global-set-key (kbd "C-M-)") 'ace-mc-add-single-cursor)
