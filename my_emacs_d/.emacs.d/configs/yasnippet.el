;; all yasnippet configs + functions used
(packages-conditional-install '(yasnippet))
(require 'yasnippet)

(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/my_snippets")))

(yas-global-mode 1)
