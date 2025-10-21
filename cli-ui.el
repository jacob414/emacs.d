;; Load path settings
(setq emacs-dir "~/src/mine/emacs.d")
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path emacs-dir)

(require 'here-env)

(add-to-list 'load-path (concat emacs-dir "/hosts"))

(setq create-lockfiles nil)

(menu-bar-mode -1)

(require 'base)         ;; UI basics, yasnippet setup helper
(require 'functions)    ;; utility functions (incl. osx-support)
(require 'visual)       ;; theme bootstrap

(require 'custom-keybindings)
