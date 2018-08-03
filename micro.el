;; Are we 'modern'?
(setq modern (>= emacs-major-version 23))

;; Load path settings
(setq emacs-dir "~/src/mine/emacs.d")

(add-to-list 'load-path emacs-dir)
(setq site-lisp (format "%s/%s" emacs-dir "/site-lisp"))

(add-to-list 'load-path (concat site-lisp "/zenburn"))
(add-to-list 'load-path (concat emacs-dir "/hosts"))

;; Basic environment ----------------------------------------------------------

(require 'functions)
(require 'base)
(if (eq system-type 'darwin) (osx-support) )

(add-to-list 'load-path "~/src/ext/emacs/spacemacs-theme")
(require 'spacemacs-dark-theme)

;; (require 'zenburn-theme)

;; Host specific stuff - should always have the last word ---------------------

(setq yas-snippet-dirs
      '("~/src/mine/emacs.d/snippet"  ;; personal snippets
        ))
(require 'yasnippet)
(yas/global-mode)
(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
