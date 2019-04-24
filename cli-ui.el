;; Load path settings
(setq emacs-dir "~/src/mine/emacs.d")
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path emacs-dir)

(require 'here-env)

(add-to-list 'load-path (concat site-lisp "/zenburn"))
(add-to-list 'load-path (concat emacs-dir "/hosts"))

(require 'functions)

(require 'my-zenburn)

(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'linum)
(global-linum-mode t)
(setq linum-format "%d ")

(custom-set-faces
 '(linum ((t
           (:inherit (shadow default)
                     :foreground "dark orange"
                     :background "black"
                     ))) ))


(require 'custom-keybindings)
