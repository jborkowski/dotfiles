(packages-conditional-install '(neotree))


(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)

(global-set-key (kbd "M-o M-t t") 'neotree-toggle)
(global-set-key (kbd "M-o M-t f") 'neotree-find)
(key-chord-define-global "ct" 'neotree-toggle)
(key-chord-define-global "nf" 'neotree-find)
