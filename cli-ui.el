;; Load path settings
(setq emacs-dir "~/src/mine/emacs.d")
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path emacs-dir)

(require 'here-env)

(add-to-list 'load-path (concat emacs-dir "/hosts"))

(setq create-lockfiles nil)

(menu-bar-mode -1)

(require 'linum)
(global-linum-mode t)
(setq linum-format "%d ")

(require 'custom-keybindings)
