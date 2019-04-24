;; Are we 'modern'?
(setq modern (>= emacs-major-version 23))

;; Load path settings
(setq emacs-dir "~/src/mine/emacs.d")

(add-to-list 'load-path emacs-dir)
(setq site-lisp (format "%s/%s" emacs-dir "/site-lisp"))

(add-to-list 'load-path (concat site-lisp "/zenburn"))
(add-to-list 'load-path (concat emacs-dir "/hosts"))
