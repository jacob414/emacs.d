;; Are we 'modern'?
(setq modern (>= emacs-major-version 23))

(add-to-list 'load-path emacs-dir)

(setq site-lisp (format "%s/%s" emacs-dir "/site-lisp"))

(add-to-list 'load-path (concat emacs-dir "/zenburn"))
(add-to-list 'load-path (concat emacs-dir "/hosts"))

(dolist
    (project (directory-files (concat emacs-dir "/site-lisp") t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)


(provide 'here-env)
