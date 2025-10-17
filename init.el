;; Safety: local variables and modes considered safe
(setq enable-local-variables :safe
      enable-local-eval nil
      enable-dir-local-variables t
      enable-remote-dir-locals nil)

(dolist (pair '((js-indent-level . integerp)
                (web-mode-markup-indent-offset . integerp)
                (web-mode-css-indent-offset . integerp)
                (web-mode-code-indent-offset . integerp)
                (tab-width . integerp)
                (fill-column . integerp)
                (indent-tabs-mode . booleanp)
                (lexical-binding . booleanp)
                (org-confirm-babel-evaluate . booleanp)
                (coding . symbolp)
                (encoding . symbolp)))
  (put (car pair) 'safe-local-variable (cdr pair)))

(defun my/safe-mode-p (mode)
  (memq mode '(org-mode python-mode web-mode markdown-mode text-mode
                emacs-lisp-mode lisp-mode scheme-mode js-mode
                yaml-mode json-mode conf-mode)))
(put 'mode 'safe-local-variable #'my/safe-mode-p)

;; Paths and local modules
(setq emacs-dir (expand-file-name "~/src/mine/emacs.d"))
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path (concat emacs-dir "/site-lisp"))
(add-to-list 'load-path (concat emacs-dir "/hosts"))

(require 'here-env)     ;; keeps site-lisp/hosts on load-path
(require 'deps)         ;; Checks if expected Elisp modules are
                        ;; present, can install if not.
(my/check-deps-on-startup-maybe-install)
(require 'base)         ;; UI basics, yasnippet setup helper
(require 'functions)    ;; utility functions (incl. osx-support)
(require 'visual)       ;; theme bootstrap
(require 'late-config)
